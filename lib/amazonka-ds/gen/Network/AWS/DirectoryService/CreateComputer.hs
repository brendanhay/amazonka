{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateComputer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a computer account in the specified directory, and joins the computer to the directory.
--
--
module Network.AWS.DirectoryService.CreateComputer
    (
    -- * Creating a Request
      createComputer
    , CreateComputer
    -- * Request Lenses
    , ccComputerAttributes
    , ccOrganizationalUnitDistinguishedName
    , ccDirectoryId
    , ccComputerName
    , ccPassword

    -- * Destructuring the Response
    , createComputerResponse
    , CreateComputerResponse
    -- * Response Lenses
    , ccrsComputer
    , ccrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'CreateComputer' operation.
--
--
--
-- /See:/ 'createComputer' smart constructor.
data CreateComputer = CreateComputer'
  { _ccComputerAttributes                  :: !(Maybe [Attribute])
  , _ccOrganizationalUnitDistinguishedName :: !(Maybe Text)
  , _ccDirectoryId                         :: !Text
  , _ccComputerName                        :: !Text
  , _ccPassword                            :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateComputer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccComputerAttributes' - An array of 'Attribute' objects that contain any LDAP attributes to apply to the computer account.
--
-- * 'ccOrganizationalUnitDistinguishedName' - The fully-qualified distinguished name of the organizational unit to place the computer account in.
--
-- * 'ccDirectoryId' - The identifier of the directory in which to create the computer account.
--
-- * 'ccComputerName' - The name of the computer account.
--
-- * 'ccPassword' - A one-time password that is used to join the computer to the directory. You should generate a random, strong password to use for this parameter.
createComputer
    :: Text -- ^ 'ccDirectoryId'
    -> Text -- ^ 'ccComputerName'
    -> Text -- ^ 'ccPassword'
    -> CreateComputer
createComputer pDirectoryId_ pComputerName_ pPassword_ =
  CreateComputer'
    { _ccComputerAttributes = Nothing
    , _ccOrganizationalUnitDistinguishedName = Nothing
    , _ccDirectoryId = pDirectoryId_
    , _ccComputerName = pComputerName_
    , _ccPassword = _Sensitive # pPassword_
    }


-- | An array of 'Attribute' objects that contain any LDAP attributes to apply to the computer account.
ccComputerAttributes :: Lens' CreateComputer [Attribute]
ccComputerAttributes = lens _ccComputerAttributes (\ s a -> s{_ccComputerAttributes = a}) . _Default . _Coerce

-- | The fully-qualified distinguished name of the organizational unit to place the computer account in.
ccOrganizationalUnitDistinguishedName :: Lens' CreateComputer (Maybe Text)
ccOrganizationalUnitDistinguishedName = lens _ccOrganizationalUnitDistinguishedName (\ s a -> s{_ccOrganizationalUnitDistinguishedName = a})

-- | The identifier of the directory in which to create the computer account.
ccDirectoryId :: Lens' CreateComputer Text
ccDirectoryId = lens _ccDirectoryId (\ s a -> s{_ccDirectoryId = a})

-- | The name of the computer account.
ccComputerName :: Lens' CreateComputer Text
ccComputerName = lens _ccComputerName (\ s a -> s{_ccComputerName = a})

-- | A one-time password that is used to join the computer to the directory. You should generate a random, strong password to use for this parameter.
ccPassword :: Lens' CreateComputer Text
ccPassword = lens _ccPassword (\ s a -> s{_ccPassword = a}) . _Sensitive

instance AWSRequest CreateComputer where
        type Rs CreateComputer = CreateComputerResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 CreateComputerResponse' <$>
                   (x .?> "Computer") <*> (pure (fromEnum s)))

instance Hashable CreateComputer where

instance NFData CreateComputer where

instance ToHeaders CreateComputer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.CreateComputer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateComputer where
        toJSON CreateComputer'{..}
          = object
              (catMaybes
                 [("ComputerAttributes" .=) <$> _ccComputerAttributes,
                  ("OrganizationalUnitDistinguishedName" .=) <$>
                    _ccOrganizationalUnitDistinguishedName,
                  Just ("DirectoryId" .= _ccDirectoryId),
                  Just ("ComputerName" .= _ccComputerName),
                  Just ("Password" .= _ccPassword)])

instance ToPath CreateComputer where
        toPath = const "/"

instance ToQuery CreateComputer where
        toQuery = const mempty

-- | Contains the results for the 'CreateComputer' operation.
--
--
--
-- /See:/ 'createComputerResponse' smart constructor.
data CreateComputerResponse = CreateComputerResponse'
  { _ccrsComputer       :: !(Maybe Computer)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateComputerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsComputer' - A 'Computer' object that represents the computer account.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createComputerResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateComputerResponse
createComputerResponse pResponseStatus_ =
  CreateComputerResponse'
    {_ccrsComputer = Nothing, _ccrsResponseStatus = pResponseStatus_}


-- | A 'Computer' object that represents the computer account.
ccrsComputer :: Lens' CreateComputerResponse (Maybe Computer)
ccrsComputer = lens _ccrsComputer (\ s a -> s{_ccrsComputer = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateComputerResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateComputerResponse where

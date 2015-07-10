{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateComputer
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a computer account in the specified directory, and joins the
-- computer to the directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateComputer.html>
module Network.AWS.DirectoryService.CreateComputer
    (
    -- * Request
      CreateComputer
    -- ** Request constructor
    , createComputer
    -- ** Request lenses
    , ccComputerAttributes
    , ccOrganizationalUnitDistinguishedName
    , ccDirectoryId
    , ccComputerName
    , ccPassword

    -- * Response
    , CreateComputerResponse
    -- ** Response constructor
    , createComputerResponse
    -- ** Response lenses
    , ccrComputer
    , ccrStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateComputer operation.
--
-- /See:/ 'createComputer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccComputerAttributes'
--
-- * 'ccOrganizationalUnitDistinguishedName'
--
-- * 'ccDirectoryId'
--
-- * 'ccComputerName'
--
-- * 'ccPassword'
data CreateComputer = CreateComputer'
    { _ccComputerAttributes                  :: !(Maybe [Attribute])
    , _ccOrganizationalUnitDistinguishedName :: !(Maybe Text)
    , _ccDirectoryId                         :: !Text
    , _ccComputerName                        :: !Text
    , _ccPassword                            :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateComputer' smart constructor.
createComputer :: Text -> Text -> Text -> CreateComputer
createComputer pDirectoryId pComputerName pPassword =
    CreateComputer'
    { _ccComputerAttributes = Nothing
    , _ccOrganizationalUnitDistinguishedName = Nothing
    , _ccDirectoryId = pDirectoryId
    , _ccComputerName = pComputerName
    , _ccPassword = _Sensitive # pPassword
    }

-- | An array of Attribute objects that contain any LDAP attributes to apply
-- to the computer account.
ccComputerAttributes :: Lens' CreateComputer [Attribute]
ccComputerAttributes = lens _ccComputerAttributes (\ s a -> s{_ccComputerAttributes = a}) . _Default;

-- | The fully-qualified distinguished name of the organizational unit to
-- place the computer account in.
ccOrganizationalUnitDistinguishedName :: Lens' CreateComputer (Maybe Text)
ccOrganizationalUnitDistinguishedName = lens _ccOrganizationalUnitDistinguishedName (\ s a -> s{_ccOrganizationalUnitDistinguishedName = a});

-- | The identifier of the directory to create the computer account in.
ccDirectoryId :: Lens' CreateComputer Text
ccDirectoryId = lens _ccDirectoryId (\ s a -> s{_ccDirectoryId = a});

-- | The name of the computer account.
ccComputerName :: Lens' CreateComputer Text
ccComputerName = lens _ccComputerName (\ s a -> s{_ccComputerName = a});

-- | A one-time password that is used to join the computer to the directory.
-- You should generate a random, strong password to use for this parameter.
ccPassword :: Lens' CreateComputer Text
ccPassword = lens _ccPassword (\ s a -> s{_ccPassword = a}) . _Sensitive;

instance AWSRequest CreateComputer where
        type Sv CreateComputer = DirectoryService
        type Rs CreateComputer = CreateComputerResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateComputerResponse' <$>
                   (x .?> "Computer") <*> (pure (fromEnum s)))

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
              ["ComputerAttributes" .= _ccComputerAttributes,
               "OrganizationalUnitDistinguishedName" .=
                 _ccOrganizationalUnitDistinguishedName,
               "DirectoryId" .= _ccDirectoryId,
               "ComputerName" .= _ccComputerName,
               "Password" .= _ccPassword]

instance ToPath CreateComputer where
        toPath = const "/"

instance ToQuery CreateComputer where
        toQuery = const mempty

-- | Contains the results for the CreateComputer operation.
--
-- /See:/ 'createComputerResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccrComputer'
--
-- * 'ccrStatus'
data CreateComputerResponse = CreateComputerResponse'
    { _ccrComputer :: !(Maybe Computer)
    , _ccrStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateComputerResponse' smart constructor.
createComputerResponse :: Int -> CreateComputerResponse
createComputerResponse pStatus =
    CreateComputerResponse'
    { _ccrComputer = Nothing
    , _ccrStatus = pStatus
    }

-- | A Computer object the represents the computer account.
ccrComputer :: Lens' CreateComputerResponse (Maybe Computer)
ccrComputer = lens _ccrComputer (\ s a -> s{_ccrComputer = a});

-- | FIXME: Undocumented member.
ccrStatus :: Lens' CreateComputerResponse Int
ccrStatus = lens _ccrStatus (\ s a -> s{_ccrStatus = a});

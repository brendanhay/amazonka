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
    , ccrqComputerAttributes
    , ccrqOrganizationalUnitDistinguishedName
    , ccrqDirectoryId
    , ccrqComputerName
    , ccrqPassword

    -- * Response
    , CreateComputerResponse
    -- ** Response constructor
    , createComputerResponse
    -- ** Response lenses
    , ccrsComputer
    , ccrsStatus
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
-- * 'ccrqComputerAttributes'
--
-- * 'ccrqOrganizationalUnitDistinguishedName'
--
-- * 'ccrqDirectoryId'
--
-- * 'ccrqComputerName'
--
-- * 'ccrqPassword'
data CreateComputer = CreateComputer'
    { _ccrqComputerAttributes                  :: !(Maybe [Attribute])
    , _ccrqOrganizationalUnitDistinguishedName :: !(Maybe Text)
    , _ccrqDirectoryId                         :: !Text
    , _ccrqComputerName                        :: !Text
    , _ccrqPassword                            :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateComputer' smart constructor.
createComputer :: Text -> Text -> Text -> CreateComputer
createComputer pDirectoryId_ pComputerName_ pPassword_ =
    CreateComputer'
    { _ccrqComputerAttributes = Nothing
    , _ccrqOrganizationalUnitDistinguishedName = Nothing
    , _ccrqDirectoryId = pDirectoryId_
    , _ccrqComputerName = pComputerName_
    , _ccrqPassword = _Sensitive # pPassword_
    }

-- | An array of Attribute objects that contain any LDAP attributes to apply
-- to the computer account.
ccrqComputerAttributes :: Lens' CreateComputer [Attribute]
ccrqComputerAttributes = lens _ccrqComputerAttributes (\ s a -> s{_ccrqComputerAttributes = a}) . _Default;

-- | The fully-qualified distinguished name of the organizational unit to
-- place the computer account in.
ccrqOrganizationalUnitDistinguishedName :: Lens' CreateComputer (Maybe Text)
ccrqOrganizationalUnitDistinguishedName = lens _ccrqOrganizationalUnitDistinguishedName (\ s a -> s{_ccrqOrganizationalUnitDistinguishedName = a});

-- | The identifier of the directory to create the computer account in.
ccrqDirectoryId :: Lens' CreateComputer Text
ccrqDirectoryId = lens _ccrqDirectoryId (\ s a -> s{_ccrqDirectoryId = a});

-- | The name of the computer account.
ccrqComputerName :: Lens' CreateComputer Text
ccrqComputerName = lens _ccrqComputerName (\ s a -> s{_ccrqComputerName = a});

-- | A one-time password that is used to join the computer to the directory.
-- You should generate a random, strong password to use for this parameter.
ccrqPassword :: Lens' CreateComputer Text
ccrqPassword = lens _ccrqPassword (\ s a -> s{_ccrqPassword = a}) . _Sensitive;

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
              ["ComputerAttributes" .= _ccrqComputerAttributes,
               "OrganizationalUnitDistinguishedName" .=
                 _ccrqOrganizationalUnitDistinguishedName,
               "DirectoryId" .= _ccrqDirectoryId,
               "ComputerName" .= _ccrqComputerName,
               "Password" .= _ccrqPassword]

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
-- * 'ccrsComputer'
--
-- * 'ccrsStatus'
data CreateComputerResponse = CreateComputerResponse'
    { _ccrsComputer :: !(Maybe Computer)
    , _ccrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateComputerResponse' smart constructor.
createComputerResponse :: Int -> CreateComputerResponse
createComputerResponse pStatus_ =
    CreateComputerResponse'
    { _ccrsComputer = Nothing
    , _ccrsStatus = pStatus_
    }

-- | A Computer object the represents the computer account.
ccrsComputer :: Lens' CreateComputerResponse (Maybe Computer)
ccrsComputer = lens _ccrsComputer (\ s a -> s{_ccrsComputer = a});

-- | FIXME: Undocumented member.
ccrsStatus :: Lens' CreateComputerResponse Int
ccrsStatus = lens _ccrsStatus (\ s a -> s{_ccrsStatus = a});

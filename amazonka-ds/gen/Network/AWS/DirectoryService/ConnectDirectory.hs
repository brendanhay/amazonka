{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.ConnectDirectory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an AD Connector to connect an on-premises directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_ConnectDirectory.html>
module Network.AWS.DirectoryService.ConnectDirectory
    (
    -- * Request
      ConnectDirectory
    -- ** Request constructor
    , connectDirectory
    -- ** Request lenses
    , cdrqShortName
    , cdrqDescription
    , cdrqName
    , cdrqPassword
    , cdrqSize
    , cdrqConnectSettings

    -- * Response
    , ConnectDirectoryResponse
    -- ** Response constructor
    , connectDirectoryResponse
    -- ** Response lenses
    , cdrsDirectoryId
    , cdrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the ConnectDirectory operation.
--
-- /See:/ 'connectDirectory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrqShortName'
--
-- * 'cdrqDescription'
--
-- * 'cdrqName'
--
-- * 'cdrqPassword'
--
-- * 'cdrqSize'
--
-- * 'cdrqConnectSettings'
data ConnectDirectory = ConnectDirectory'
    { _cdrqShortName       :: !(Maybe Text)
    , _cdrqDescription     :: !(Maybe Text)
    , _cdrqName            :: !Text
    , _cdrqPassword        :: !(Sensitive Text)
    , _cdrqSize            :: !DirectorySize
    , _cdrqConnectSettings :: !DirectoryConnectSettings
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConnectDirectory' smart constructor.
connectDirectory :: Text -> Text -> DirectorySize -> DirectoryConnectSettings -> ConnectDirectory
connectDirectory pName_ pPassword_ pSize_ pConnectSettings_ =
    ConnectDirectory'
    { _cdrqShortName = Nothing
    , _cdrqDescription = Nothing
    , _cdrqName = pName_
    , _cdrqPassword = _Sensitive # pPassword_
    , _cdrqSize = pSize_
    , _cdrqConnectSettings = pConnectSettings_
    }

-- | The NetBIOS name of the on-premises directory, such as @CORP@.
cdrqShortName :: Lens' ConnectDirectory (Maybe Text)
cdrqShortName = lens _cdrqShortName (\ s a -> s{_cdrqShortName = a});

-- | A textual description for the directory.
cdrqDescription :: Lens' ConnectDirectory (Maybe Text)
cdrqDescription = lens _cdrqDescription (\ s a -> s{_cdrqDescription = a});

-- | The fully-qualified name of the on-premises directory, such as
-- @corp.example.com@.
cdrqName :: Lens' ConnectDirectory Text
cdrqName = lens _cdrqName (\ s a -> s{_cdrqName = a});

-- | The password for the on-premises user account.
cdrqPassword :: Lens' ConnectDirectory Text
cdrqPassword = lens _cdrqPassword (\ s a -> s{_cdrqPassword = a}) . _Sensitive;

-- | The size of the directory.
cdrqSize :: Lens' ConnectDirectory DirectorySize
cdrqSize = lens _cdrqSize (\ s a -> s{_cdrqSize = a});

-- | A DirectoryConnectSettings object that contains additional information
-- for the operation.
cdrqConnectSettings :: Lens' ConnectDirectory DirectoryConnectSettings
cdrqConnectSettings = lens _cdrqConnectSettings (\ s a -> s{_cdrqConnectSettings = a});

instance AWSRequest ConnectDirectory where
        type Sv ConnectDirectory = DirectoryService
        type Rs ConnectDirectory = ConnectDirectoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ConnectDirectoryResponse' <$>
                   (x .?> "DirectoryId") <*> (pure (fromEnum s)))

instance ToHeaders ConnectDirectory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.ConnectDirectory" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConnectDirectory where
        toJSON ConnectDirectory'{..}
          = object
              ["ShortName" .= _cdrqShortName,
               "Description" .= _cdrqDescription,
               "Name" .= _cdrqName, "Password" .= _cdrqPassword,
               "Size" .= _cdrqSize,
               "ConnectSettings" .= _cdrqConnectSettings]

instance ToPath ConnectDirectory where
        toPath = const "/"

instance ToQuery ConnectDirectory where
        toQuery = const mempty

-- | Contains the results of the ConnectDirectory operation.
--
-- /See:/ 'connectDirectoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdrsDirectoryId'
--
-- * 'cdrsStatus'
data ConnectDirectoryResponse = ConnectDirectoryResponse'
    { _cdrsDirectoryId :: !(Maybe Text)
    , _cdrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConnectDirectoryResponse' smart constructor.
connectDirectoryResponse :: Int -> ConnectDirectoryResponse
connectDirectoryResponse pStatus_ =
    ConnectDirectoryResponse'
    { _cdrsDirectoryId = Nothing
    , _cdrsStatus = pStatus_
    }

-- | The identifier of the new directory.
cdrsDirectoryId :: Lens' ConnectDirectoryResponse (Maybe Text)
cdrsDirectoryId = lens _cdrsDirectoryId (\ s a -> s{_cdrsDirectoryId = a});

-- | FIXME: Undocumented member.
cdrsStatus :: Lens' ConnectDirectoryResponse Int
cdrsStatus = lens _cdrsStatus (\ s a -> s{_cdrsStatus = a});

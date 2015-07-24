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
    , cdShortName
    , cdDescription
    , cdName
    , cdPassword
    , cdSize
    , cdConnectSettings

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
-- * 'cdShortName'
--
-- * 'cdDescription'
--
-- * 'cdName'
--
-- * 'cdPassword'
--
-- * 'cdSize'
--
-- * 'cdConnectSettings'
data ConnectDirectory = ConnectDirectory'
    { _cdShortName       :: !(Maybe Text)
    , _cdDescription     :: !(Maybe Text)
    , _cdName            :: !Text
    , _cdPassword        :: !(Sensitive Text)
    , _cdSize            :: !DirectorySize
    , _cdConnectSettings :: !DirectoryConnectSettings
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConnectDirectory' smart constructor.
connectDirectory :: Text -> Text -> DirectorySize -> DirectoryConnectSettings -> ConnectDirectory
connectDirectory pName_ pPassword_ pSize_ pConnectSettings_ =
    ConnectDirectory'
    { _cdShortName = Nothing
    , _cdDescription = Nothing
    , _cdName = pName_
    , _cdPassword = _Sensitive # pPassword_
    , _cdSize = pSize_
    , _cdConnectSettings = pConnectSettings_
    }

-- | The NetBIOS name of the on-premises directory, such as @CORP@.
cdShortName :: Lens' ConnectDirectory (Maybe Text)
cdShortName = lens _cdShortName (\ s a -> s{_cdShortName = a});

-- | A textual description for the directory.
cdDescription :: Lens' ConnectDirectory (Maybe Text)
cdDescription = lens _cdDescription (\ s a -> s{_cdDescription = a});

-- | The fully-qualified name of the on-premises directory, such as
-- @corp.example.com@.
cdName :: Lens' ConnectDirectory Text
cdName = lens _cdName (\ s a -> s{_cdName = a});

-- | The password for the on-premises user account.
cdPassword :: Lens' ConnectDirectory Text
cdPassword = lens _cdPassword (\ s a -> s{_cdPassword = a}) . _Sensitive;

-- | The size of the directory.
cdSize :: Lens' ConnectDirectory DirectorySize
cdSize = lens _cdSize (\ s a -> s{_cdSize = a});

-- | A DirectoryConnectSettings object that contains additional information
-- for the operation.
cdConnectSettings :: Lens' ConnectDirectory DirectoryConnectSettings
cdConnectSettings = lens _cdConnectSettings (\ s a -> s{_cdConnectSettings = a});

instance AWSRequest ConnectDirectory where
        type Sv ConnectDirectory = DirectoryService
        type Rs ConnectDirectory = ConnectDirectoryResponse
        request = postJSON "ConnectDirectory"
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
              ["ShortName" .= _cdShortName,
               "Description" .= _cdDescription, "Name" .= _cdName,
               "Password" .= _cdPassword, "Size" .= _cdSize,
               "ConnectSettings" .= _cdConnectSettings]

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

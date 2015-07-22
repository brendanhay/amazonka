{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateDirectory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a Simple AD directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateDirectory.html>
module Network.AWS.DirectoryService.CreateDirectory
    (
    -- * Request
      CreateDirectory
    -- ** Request constructor
    , createDirectory
    -- ** Request lenses
    , crqShortName
    , crqVPCSettings
    , crqDescription
    , crqName
    , crqPassword
    , crqSize

    -- * Response
    , CreateDirectoryResponse
    -- ** Response constructor
    , createDirectoryResponse
    -- ** Response lenses
    , crsDirectoryId
    , crsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateDirectory operation.
--
-- /See:/ 'createDirectory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crqShortName'
--
-- * 'crqVPCSettings'
--
-- * 'crqDescription'
--
-- * 'crqName'
--
-- * 'crqPassword'
--
-- * 'crqSize'
data CreateDirectory = CreateDirectory'
    { _crqShortName   :: !(Maybe Text)
    , _crqVPCSettings :: !(Maybe DirectoryVPCSettings)
    , _crqDescription :: !(Maybe Text)
    , _crqName        :: !Text
    , _crqPassword    :: !(Sensitive Text)
    , _crqSize        :: !DirectorySize
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDirectory' smart constructor.
createDirectory :: Text -> Text -> DirectorySize -> CreateDirectory
createDirectory pName pPassword pSize =
    CreateDirectory'
    { _crqShortName = Nothing
    , _crqVPCSettings = Nothing
    , _crqDescription = Nothing
    , _crqName = pName
    , _crqPassword = _Sensitive # pPassword
    , _crqSize = pSize
    }

-- | The short name of the directory, such as @CORP@.
crqShortName :: Lens' CreateDirectory (Maybe Text)
crqShortName = lens _crqShortName (\ s a -> s{_crqShortName = a});

-- | A DirectoryVpcSettings object that contains additional information for
-- the operation.
crqVPCSettings :: Lens' CreateDirectory (Maybe DirectoryVPCSettings)
crqVPCSettings = lens _crqVPCSettings (\ s a -> s{_crqVPCSettings = a});

-- | A textual description for the directory.
crqDescription :: Lens' CreateDirectory (Maybe Text)
crqDescription = lens _crqDescription (\ s a -> s{_crqDescription = a});

-- | The fully qualified name for the directory, such as @corp.example.com@.
crqName :: Lens' CreateDirectory Text
crqName = lens _crqName (\ s a -> s{_crqName = a});

-- | The password for the directory administrator. The directory creation
-- process creates a directory administrator account with the username
-- @Administrator@ and this password.
crqPassword :: Lens' CreateDirectory Text
crqPassword = lens _crqPassword (\ s a -> s{_crqPassword = a}) . _Sensitive;

-- | The size of the directory.
crqSize :: Lens' CreateDirectory DirectorySize
crqSize = lens _crqSize (\ s a -> s{_crqSize = a});

instance AWSRequest CreateDirectory where
        type Sv CreateDirectory = DirectoryService
        type Rs CreateDirectory = CreateDirectoryResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDirectoryResponse' <$>
                   (x .?> "DirectoryId") <*> (pure (fromEnum s)))

instance ToHeaders CreateDirectory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.CreateDirectory" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDirectory where
        toJSON CreateDirectory'{..}
          = object
              ["ShortName" .= _crqShortName,
               "VpcSettings" .= _crqVPCSettings,
               "Description" .= _crqDescription, "Name" .= _crqName,
               "Password" .= _crqPassword, "Size" .= _crqSize]

instance ToPath CreateDirectory where
        toPath = const "/"

instance ToQuery CreateDirectory where
        toQuery = const mempty

-- | Contains the results of the CreateDirectory operation.
--
-- /See:/ 'createDirectoryResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crsDirectoryId'
--
-- * 'crsStatus'
data CreateDirectoryResponse = CreateDirectoryResponse'
    { _crsDirectoryId :: !(Maybe Text)
    , _crsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDirectoryResponse' smart constructor.
createDirectoryResponse :: Int -> CreateDirectoryResponse
createDirectoryResponse pStatus =
    CreateDirectoryResponse'
    { _crsDirectoryId = Nothing
    , _crsStatus = pStatus
    }

-- | The identifier of the directory that was created.
crsDirectoryId :: Lens' CreateDirectoryResponse (Maybe Text)
crsDirectoryId = lens _crsDirectoryId (\ s a -> s{_crsDirectoryId = a});

-- | FIXME: Undocumented member.
crsStatus :: Lens' CreateDirectoryResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});

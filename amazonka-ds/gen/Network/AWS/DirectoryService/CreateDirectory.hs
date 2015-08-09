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
-- Module      : Network.AWS.DirectoryService.CreateDirectory
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Simple AD directory.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateDirectory.html AWS API Reference> for CreateDirectory.
module Network.AWS.DirectoryService.CreateDirectory
    (
    -- * Creating a Request
      CreateDirectory
    , createDirectory
    -- * Request Lenses
    , cShortName
    , cVPCSettings
    , cDescription
    , cName
    , cPassword
    , cSize

    -- * Destructuring the Response
    , CreateDirectoryResponse
    , createDirectoryResponse
    -- * Response Lenses
    , crsDirectoryId
    , crsStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the CreateDirectory operation.
--
-- /See:/ 'createDirectory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cShortName'
--
-- * 'cVPCSettings'
--
-- * 'cDescription'
--
-- * 'cName'
--
-- * 'cPassword'
--
-- * 'cSize'
data CreateDirectory = CreateDirectory'
    { _cShortName :: !(Maybe Text)
    , _cVPCSettings :: !(Maybe DirectoryVPCSettings)
    , _cDescription :: !(Maybe Text)
    , _cName :: !Text
    , _cPassword :: !(Sensitive Text)
    , _cSize :: !DirectorySize
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDirectory' smart constructor.
createDirectory :: Text -> Text -> DirectorySize -> CreateDirectory
createDirectory pName_ pPassword_ pSize_ = 
    CreateDirectory'
    { _cShortName = Nothing
    , _cVPCSettings = Nothing
    , _cDescription = Nothing
    , _cName = pName_
    , _cPassword = _Sensitive # pPassword_
    , _cSize = pSize_
    }

-- | The short name of the directory, such as @CORP@.
cShortName :: Lens' CreateDirectory (Maybe Text)
cShortName = lens _cShortName (\ s a -> s{_cShortName = a});

-- | A DirectoryVpcSettings object that contains additional information for
-- the operation.
cVPCSettings :: Lens' CreateDirectory (Maybe DirectoryVPCSettings)
cVPCSettings = lens _cVPCSettings (\ s a -> s{_cVPCSettings = a});

-- | A textual description for the directory.
cDescription :: Lens' CreateDirectory (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a});

-- | The fully qualified name for the directory, such as @corp.example.com@.
cName :: Lens' CreateDirectory Text
cName = lens _cName (\ s a -> s{_cName = a});

-- | The password for the directory administrator. The directory creation
-- process creates a directory administrator account with the username
-- @Administrator@ and this password.
cPassword :: Lens' CreateDirectory Text
cPassword = lens _cPassword (\ s a -> s{_cPassword = a}) . _Sensitive;

-- | The size of the directory.
cSize :: Lens' CreateDirectory DirectorySize
cSize = lens _cSize (\ s a -> s{_cSize = a});

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
              ["ShortName" .= _cShortName,
               "VpcSettings" .= _cVPCSettings,
               "Description" .= _cDescription, "Name" .= _cName,
               "Password" .= _cPassword, "Size" .= _cSize]

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
    , _crsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDirectoryResponse' smart constructor.
createDirectoryResponse :: Int -> CreateDirectoryResponse
createDirectoryResponse pStatus_ = 
    CreateDirectoryResponse'
    { _crsDirectoryId = Nothing
    , _crsStatus = pStatus_
    }

-- | The identifier of the directory that was created.
crsDirectoryId :: Lens' CreateDirectoryResponse (Maybe Text)
crsDirectoryId = lens _crsDirectoryId (\ s a -> s{_crsDirectoryId = a});

-- | Undocumented member.
crsStatus :: Lens' CreateDirectoryResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});

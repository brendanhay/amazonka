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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Simple AD directory.
--
--
-- Before you call /CreateDirectory/ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the /CreateDirectory/ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
--
module Network.AWS.DirectoryService.CreateDirectory
    (
    -- * Creating a Request
      createDirectory
    , CreateDirectory
    -- * Request Lenses
    , cShortName
    , cVPCSettings
    , cDescription
    , cName
    , cPassword
    , cSize

    -- * Destructuring the Response
    , createDirectoryResponse
    , CreateDirectoryResponse
    -- * Response Lenses
    , crsDirectoryId
    , crsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'CreateDirectory' operation.
--
--
--
-- /See:/ 'createDirectory' smart constructor.
data CreateDirectory = CreateDirectory'
  { _cShortName   :: !(Maybe Text)
  , _cVPCSettings :: !(Maybe DirectoryVPCSettings)
  , _cDescription :: !(Maybe Text)
  , _cName        :: !Text
  , _cPassword    :: !(Sensitive Text)
  , _cSize        :: !DirectorySize
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cShortName' - The short name of the directory, such as @CORP@ .
--
-- * 'cVPCSettings' - A 'DirectoryVpcSettings' object that contains additional information for the operation.
--
-- * 'cDescription' - A textual description for the directory.
--
-- * 'cName' - The fully qualified name for the directory, such as @corp.example.com@ .
--
-- * 'cPassword' - The password for the directory administrator. The directory creation process creates a directory administrator account with the username @Administrator@ and this password.
--
-- * 'cSize' - The size of the directory.
createDirectory
    :: Text -- ^ 'cName'
    -> Text -- ^ 'cPassword'
    -> DirectorySize -- ^ 'cSize'
    -> CreateDirectory
createDirectory pName_ pPassword_ pSize_ =
  CreateDirectory'
    { _cShortName = Nothing
    , _cVPCSettings = Nothing
    , _cDescription = Nothing
    , _cName = pName_
    , _cPassword = _Sensitive # pPassword_
    , _cSize = pSize_
    }


-- | The short name of the directory, such as @CORP@ .
cShortName :: Lens' CreateDirectory (Maybe Text)
cShortName = lens _cShortName (\ s a -> s{_cShortName = a})

-- | A 'DirectoryVpcSettings' object that contains additional information for the operation.
cVPCSettings :: Lens' CreateDirectory (Maybe DirectoryVPCSettings)
cVPCSettings = lens _cVPCSettings (\ s a -> s{_cVPCSettings = a})

-- | A textual description for the directory.
cDescription :: Lens' CreateDirectory (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

-- | The fully qualified name for the directory, such as @corp.example.com@ .
cName :: Lens' CreateDirectory Text
cName = lens _cName (\ s a -> s{_cName = a})

-- | The password for the directory administrator. The directory creation process creates a directory administrator account with the username @Administrator@ and this password.
cPassword :: Lens' CreateDirectory Text
cPassword = lens _cPassword (\ s a -> s{_cPassword = a}) . _Sensitive

-- | The size of the directory.
cSize :: Lens' CreateDirectory DirectorySize
cSize = lens _cSize (\ s a -> s{_cSize = a})

instance AWSRequest CreateDirectory where
        type Rs CreateDirectory = CreateDirectoryResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 CreateDirectoryResponse' <$>
                   (x .?> "DirectoryId") <*> (pure (fromEnum s)))

instance Hashable CreateDirectory where

instance NFData CreateDirectory where

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
              (catMaybes
                 [("ShortName" .=) <$> _cShortName,
                  ("VpcSettings" .=) <$> _cVPCSettings,
                  ("Description" .=) <$> _cDescription,
                  Just ("Name" .= _cName),
                  Just ("Password" .= _cPassword),
                  Just ("Size" .= _cSize)])

instance ToPath CreateDirectory where
        toPath = const "/"

instance ToQuery CreateDirectory where
        toQuery = const mempty

-- | Contains the results of the 'CreateDirectory' operation.
--
--
--
-- /See:/ 'createDirectoryResponse' smart constructor.
data CreateDirectoryResponse = CreateDirectoryResponse'
  { _crsDirectoryId    :: !(Maybe Text)
  , _crsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsDirectoryId' - The identifier of the directory that was created.
--
-- * 'crsResponseStatus' - -- | The response status code.
createDirectoryResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateDirectoryResponse
createDirectoryResponse pResponseStatus_ =
  CreateDirectoryResponse'
    {_crsDirectoryId = Nothing, _crsResponseStatus = pResponseStatus_}


-- | The identifier of the directory that was created.
crsDirectoryId :: Lens' CreateDirectoryResponse (Maybe Text)
crsDirectoryId = lens _crsDirectoryId (\ s a -> s{_crsDirectoryId = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateDirectoryResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateDirectoryResponse where

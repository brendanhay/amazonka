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
-- Module      : Network.AWS.DeviceFarm.UpdateUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an uploaded test specification (test spec).
--
--
module Network.AWS.DeviceFarm.UpdateUpload
    (
    -- * Creating a Request
      updateUpload
    , UpdateUpload
    -- * Request Lenses
    , uuEditContent
    , uuName
    , uuContentType
    , uuArn

    -- * Destructuring the Response
    , updateUploadResponse
    , UpdateUploadResponse
    -- * Response Lenses
    , uursUpload
    , uursResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUpload' smart constructor.
data UpdateUpload = UpdateUpload'
  { _uuEditContent :: !(Maybe Bool)
  , _uuName        :: !(Maybe Text)
  , _uuContentType :: !(Maybe Text)
  , _uuArn         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuEditContent' - Set to true if the YAML file has changed and needs to be updated; otherwise, set to false.
--
-- * 'uuName' - The upload's test spec file name. The name should not contain the '/' character. The test spec file name must end with the @.yaml@ or @.yml@ file extension.
--
-- * 'uuContentType' - The upload's content type (for example, "application/x-yaml").
--
-- * 'uuArn' - The Amazon Resource Name (ARN) of the uploaded test spec.
updateUpload
    :: Text -- ^ 'uuArn'
    -> UpdateUpload
updateUpload pArn_ =
  UpdateUpload'
    { _uuEditContent = Nothing
    , _uuName = Nothing
    , _uuContentType = Nothing
    , _uuArn = pArn_
    }


-- | Set to true if the YAML file has changed and needs to be updated; otherwise, set to false.
uuEditContent :: Lens' UpdateUpload (Maybe Bool)
uuEditContent = lens _uuEditContent (\ s a -> s{_uuEditContent = a})

-- | The upload's test spec file name. The name should not contain the '/' character. The test spec file name must end with the @.yaml@ or @.yml@ file extension.
uuName :: Lens' UpdateUpload (Maybe Text)
uuName = lens _uuName (\ s a -> s{_uuName = a})

-- | The upload's content type (for example, "application/x-yaml").
uuContentType :: Lens' UpdateUpload (Maybe Text)
uuContentType = lens _uuContentType (\ s a -> s{_uuContentType = a})

-- | The Amazon Resource Name (ARN) of the uploaded test spec.
uuArn :: Lens' UpdateUpload Text
uuArn = lens _uuArn (\ s a -> s{_uuArn = a})

instance AWSRequest UpdateUpload where
        type Rs UpdateUpload = UpdateUploadResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUploadResponse' <$>
                   (x .?> "upload") <*> (pure (fromEnum s)))

instance Hashable UpdateUpload where

instance NFData UpdateUpload where

instance ToHeaders UpdateUpload where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.UpdateUpload" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUpload where
        toJSON UpdateUpload'{..}
          = object
              (catMaybes
                 [("editContent" .=) <$> _uuEditContent,
                  ("name" .=) <$> _uuName,
                  ("contentType" .=) <$> _uuContentType,
                  Just ("arn" .= _uuArn)])

instance ToPath UpdateUpload where
        toPath = const "/"

instance ToQuery UpdateUpload where
        toQuery = const mempty

-- | /See:/ 'updateUploadResponse' smart constructor.
data UpdateUploadResponse = UpdateUploadResponse'
  { _uursUpload         :: !(Maybe Upload)
  , _uursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uursUpload' - A test spec uploaded to Device Farm.
--
-- * 'uursResponseStatus' - -- | The response status code.
updateUploadResponse
    :: Int -- ^ 'uursResponseStatus'
    -> UpdateUploadResponse
updateUploadResponse pResponseStatus_ =
  UpdateUploadResponse'
    {_uursUpload = Nothing, _uursResponseStatus = pResponseStatus_}


-- | A test spec uploaded to Device Farm.
uursUpload :: Lens' UpdateUploadResponse (Maybe Upload)
uursUpload = lens _uursUpload (\ s a -> s{_uursUpload = a})

-- | -- | The response status code.
uursResponseStatus :: Lens' UpdateUploadResponse Int
uursResponseStatus = lens _uursResponseStatus (\ s a -> s{_uursResponseStatus = a})

instance NFData UpdateUploadResponse where

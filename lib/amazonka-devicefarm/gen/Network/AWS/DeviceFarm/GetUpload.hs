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
-- Module      : Network.AWS.DeviceFarm.GetUpload
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an upload.
--
--
module Network.AWS.DeviceFarm.GetUpload
    (
    -- * Creating a Request
      getUpload
    , GetUpload
    -- * Request Lenses
    , guArn

    -- * Destructuring the Response
    , getUploadResponse
    , GetUploadResponse
    -- * Response Lenses
    , gursUpload
    , gursResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents a request to the get upload operation.
--
--
--
-- /See:/ 'getUpload' smart constructor.
newtype GetUpload = GetUpload'
  { _guArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guArn' - The upload's ARN.
getUpload
    :: Text -- ^ 'guArn'
    -> GetUpload
getUpload pArn_ = GetUpload' {_guArn = pArn_}


-- | The upload's ARN.
guArn :: Lens' GetUpload Text
guArn = lens _guArn (\ s a -> s{_guArn = a})

instance AWSRequest GetUpload where
        type Rs GetUpload = GetUploadResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetUploadResponse' <$>
                   (x .?> "upload") <*> (pure (fromEnum s)))

instance Hashable GetUpload where

instance NFData GetUpload where

instance ToHeaders GetUpload where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetUpload" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetUpload where
        toJSON GetUpload'{..}
          = object (catMaybes [Just ("arn" .= _guArn)])

instance ToPath GetUpload where
        toPath = const "/"

instance ToQuery GetUpload where
        toQuery = const mempty

-- | Represents the result of a get upload request.
--
--
--
-- /See:/ 'getUploadResponse' smart constructor.
data GetUploadResponse = GetUploadResponse'
  { _gursUpload         :: !(Maybe Upload)
  , _gursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gursUpload' - An app or a set of one or more tests to upload or that have been uploaded.
--
-- * 'gursResponseStatus' - -- | The response status code.
getUploadResponse
    :: Int -- ^ 'gursResponseStatus'
    -> GetUploadResponse
getUploadResponse pResponseStatus_ =
  GetUploadResponse'
    {_gursUpload = Nothing, _gursResponseStatus = pResponseStatus_}


-- | An app or a set of one or more tests to upload or that have been uploaded.
gursUpload :: Lens' GetUploadResponse (Maybe Upload)
gursUpload = lens _gursUpload (\ s a -> s{_gursUpload = a})

-- | -- | The response status code.
gursResponseStatus :: Lens' GetUploadResponse Int
gursResponseStatus = lens _gursResponseStatus (\ s a -> s{_gursResponseStatus = a})

instance NFData GetUploadResponse where

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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an upload.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetUpload.html AWS API Reference> for GetUpload.
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
    , gursStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get upload operation.
--
-- /See:/ 'getUpload' smart constructor.
newtype GetUpload = GetUpload'
    { _guArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetUpload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guArn'
getUpload
    :: Text -- ^ 'guArn'
    -> GetUpload
getUpload pArn_ =
    GetUpload'
    { _guArn = pArn_
    }

-- | The upload\'s ARN.
guArn :: Lens' GetUpload Text
guArn = lens _guArn (\ s a -> s{_guArn = a});

instance AWSRequest GetUpload where
        type Sv GetUpload = DeviceFarm
        type Rs GetUpload = GetUploadResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetUploadResponse' <$>
                   (x .?> "upload") <*> (pure (fromEnum s)))

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
-- /See:/ 'getUploadResponse' smart constructor.
data GetUploadResponse = GetUploadResponse'
    { _gursUpload :: !(Maybe Upload)
    , _gursStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetUploadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gursUpload'
--
-- * 'gursStatus'
getUploadResponse
    :: Int -- ^ 'gursStatus'
    -> GetUploadResponse
getUploadResponse pStatus_ =
    GetUploadResponse'
    { _gursUpload = Nothing
    , _gursStatus = pStatus_
    }

-- | Undocumented member.
gursUpload :: Lens' GetUploadResponse (Maybe Upload)
gursUpload = lens _gursUpload (\ s a -> s{_gursUpload = a});

-- | The response status code.
gursStatus :: Lens' GetUploadResponse Int
gursStatus = lens _gursStatus (\ s a -> s{_gursStatus = a});

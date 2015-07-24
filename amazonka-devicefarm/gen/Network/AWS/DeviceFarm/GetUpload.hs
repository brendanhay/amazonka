{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetUpload
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an upload.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetUpload.html>
module Network.AWS.DeviceFarm.GetUpload
    (
    -- * Request
      GetUpload
    -- ** Request constructor
    , getUpload
    -- ** Request lenses
    , guArn

    -- * Response
    , GetUploadResponse
    -- ** Response constructor
    , getUploadResponse
    -- ** Response lenses
    , gursUpload
    , gursStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get upload operation.
--
-- /See:/ 'getUpload' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'guArn'
newtype GetUpload = GetUpload'
    { _guArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetUpload' smart constructor.
getUpload :: Text -> GetUpload
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
        request = postJSON "GetUpload"
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
        toJSON GetUpload'{..} = object ["arn" .= _guArn]

instance ToPath GetUpload where
        toPath = const "/"

instance ToQuery GetUpload where
        toQuery = const mempty

-- | Represents the result of a get upload request.
--
-- /See:/ 'getUploadResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gursUpload'
--
-- * 'gursStatus'
data GetUploadResponse = GetUploadResponse'
    { _gursUpload :: !(Maybe Upload)
    , _gursStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetUploadResponse' smart constructor.
getUploadResponse :: Int -> GetUploadResponse
getUploadResponse pStatus_ =
    GetUploadResponse'
    { _gursUpload = Nothing
    , _gursStatus = pStatus_
    }

-- | FIXME: Undocumented member.
gursUpload :: Lens' GetUploadResponse (Maybe Upload)
gursUpload = lens _gursUpload (\ s a -> s{_gursUpload = a});

-- | FIXME: Undocumented member.
gursStatus :: Lens' GetUploadResponse Int
gursStatus = lens _gursStatus (\ s a -> s{_gursStatus = a});

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
-- Module      : Network.AWS.S3.GetPublicAccessBlock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @PublicAccessBlock@ configuration for an Amazon S3 bucket.
--
--
module Network.AWS.S3.GetPublicAccessBlock
    (
    -- * Creating a Request
      getPublicAccessBlock
    , GetPublicAccessBlock
    -- * Request Lenses
    , gpabBucket

    -- * Destructuring the Response
    , getPublicAccessBlockResponse
    , GetPublicAccessBlockResponse
    -- * Response Lenses
    , gpabrsPublicAccessBlockConfiguration
    , gpabrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getPublicAccessBlock' smart constructor.
newtype GetPublicAccessBlock = GetPublicAccessBlock'
  { _gpabBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPublicAccessBlock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpabBucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to retrieve.
getPublicAccessBlock
    :: BucketName -- ^ 'gpabBucket'
    -> GetPublicAccessBlock
getPublicAccessBlock pBucket_ = GetPublicAccessBlock' {_gpabBucket = pBucket_}


-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration you want to retrieve.
gpabBucket :: Lens' GetPublicAccessBlock BucketName
gpabBucket = lens _gpabBucket (\ s a -> s{_gpabBucket = a})

instance AWSRequest GetPublicAccessBlock where
        type Rs GetPublicAccessBlock =
             GetPublicAccessBlockResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetPublicAccessBlockResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable GetPublicAccessBlock where

instance NFData GetPublicAccessBlock where

instance ToHeaders GetPublicAccessBlock where
        toHeaders = const mempty

instance ToPath GetPublicAccessBlock where
        toPath GetPublicAccessBlock'{..}
          = mconcat ["/", toBS _gpabBucket]

instance ToQuery GetPublicAccessBlock where
        toQuery = const (mconcat ["publicAccessBlock"])

-- | /See:/ 'getPublicAccessBlockResponse' smart constructor.
data GetPublicAccessBlockResponse = GetPublicAccessBlockResponse'
  { _gpabrsPublicAccessBlockConfiguration :: !(Maybe PublicAccessBlockConfiguration)
  , _gpabrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPublicAccessBlockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpabrsPublicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration currently in effect for this Amazon S3 bucket.
--
-- * 'gpabrsResponseStatus' - -- | The response status code.
getPublicAccessBlockResponse
    :: Int -- ^ 'gpabrsResponseStatus'
    -> GetPublicAccessBlockResponse
getPublicAccessBlockResponse pResponseStatus_ =
  GetPublicAccessBlockResponse'
    { _gpabrsPublicAccessBlockConfiguration = Nothing
    , _gpabrsResponseStatus = pResponseStatus_
    }


-- | The @PublicAccessBlock@ configuration currently in effect for this Amazon S3 bucket.
gpabrsPublicAccessBlockConfiguration :: Lens' GetPublicAccessBlockResponse (Maybe PublicAccessBlockConfiguration)
gpabrsPublicAccessBlockConfiguration = lens _gpabrsPublicAccessBlockConfiguration (\ s a -> s{_gpabrsPublicAccessBlockConfiguration = a})

-- | -- | The response status code.
gpabrsResponseStatus :: Lens' GetPublicAccessBlockResponse Int
gpabrsResponseStatus = lens _gpabrsResponseStatus (\ s a -> s{_gpabrsResponseStatus = a})

instance NFData GetPublicAccessBlockResponse where

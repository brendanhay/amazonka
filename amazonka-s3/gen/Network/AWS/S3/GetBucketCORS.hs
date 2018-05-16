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
-- Module      : Network.AWS.S3.GetBucketCORS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cors configuration for the bucket.
module Network.AWS.S3.GetBucketCORS
    (
    -- * Creating a Request
      getBucketCORS
    , GetBucketCORS
    -- * Request Lenses
    , gbcBucket

    -- * Destructuring the Response
    , getBucketCORSResponse
    , GetBucketCORSResponse
    -- * Response Lenses
    , gbcrsCORSRules
    , gbcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketCORS' smart constructor.
newtype GetBucketCORS = GetBucketCORS'
  { _gbcBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketCORS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbcBucket' - Undocumented member.
getBucketCORS
    :: BucketName -- ^ 'gbcBucket'
    -> GetBucketCORS
getBucketCORS pBucket_ = GetBucketCORS' {_gbcBucket = pBucket_}


-- | Undocumented member.
gbcBucket :: Lens' GetBucketCORS BucketName
gbcBucket = lens _gbcBucket (\ s a -> s{_gbcBucket = a})

instance AWSRequest GetBucketCORS where
        type Rs GetBucketCORS = GetBucketCORSResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketCORSResponse' <$>
                   (may (parseXMLList "CORSRule") x) <*>
                     (pure (fromEnum s)))

instance Hashable GetBucketCORS where

instance NFData GetBucketCORS where

instance ToHeaders GetBucketCORS where
        toHeaders = const mempty

instance ToPath GetBucketCORS where
        toPath GetBucketCORS'{..}
          = mconcat ["/", toBS _gbcBucket]

instance ToQuery GetBucketCORS where
        toQuery = const (mconcat ["cors"])

-- | /See:/ 'getBucketCORSResponse' smart constructor.
data GetBucketCORSResponse = GetBucketCORSResponse'
  { _gbcrsCORSRules      :: !(Maybe [CORSRule])
  , _gbcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketCORSResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbcrsCORSRules' - Undocumented member.
--
-- * 'gbcrsResponseStatus' - -- | The response status code.
getBucketCORSResponse
    :: Int -- ^ 'gbcrsResponseStatus'
    -> GetBucketCORSResponse
getBucketCORSResponse pResponseStatus_ =
  GetBucketCORSResponse'
    {_gbcrsCORSRules = Nothing, _gbcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
gbcrsCORSRules :: Lens' GetBucketCORSResponse [CORSRule]
gbcrsCORSRules = lens _gbcrsCORSRules (\ s a -> s{_gbcrsCORSRules = a}) . _Default . _Coerce

-- | -- | The response status code.
gbcrsResponseStatus :: Lens' GetBucketCORSResponse Int
gbcrsResponseStatus = lens _gbcrsResponseStatus (\ s a -> s{_gbcrsResponseStatus = a})

instance NFData GetBucketCORSResponse where

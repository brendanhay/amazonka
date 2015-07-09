{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketCORS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Returns the cors configuration for the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketCORS.html>
module Network.AWS.S3.GetBucketCORS
    (
    -- * Request
      GetBucketCORS
    -- ** Request constructor
    , getBucketCORS
    -- ** Request lenses
    , gbcBucket

    -- * Response
    , GetBucketCORSResponse
    -- ** Response constructor
    , getBucketCORSResponse
    -- ** Response lenses
    , gbcrCORSRules
    , gbcrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketCORS' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbcBucket'
newtype GetBucketCORS = GetBucketCORS'
    { _gbcBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketCORS' smart constructor.
getBucketCORS :: BucketName -> GetBucketCORS
getBucketCORS pBucket =
    GetBucketCORS'
    { _gbcBucket = pBucket
    }

-- | FIXME: Undocumented member.
gbcBucket :: Lens' GetBucketCORS BucketName
gbcBucket = lens _gbcBucket (\ s a -> s{_gbcBucket = a});

instance AWSRequest GetBucketCORS where
        type Sv GetBucketCORS = S3
        type Rs GetBucketCORS = GetBucketCORSResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketCORSResponse' <$>
                   (may (parseXMLList "CORSRule") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders GetBucketCORS where
        toHeaders = const mempty

instance ToPath GetBucketCORS where
        toPath GetBucketCORS'{..}
          = mconcat ["/", toText _gbcBucket]

instance ToQuery GetBucketCORS where
        toQuery = const (mconcat ["cors"])

-- | /See:/ 'getBucketCORSResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbcrCORSRules'
--
-- * 'gbcrStatus'
data GetBucketCORSResponse = GetBucketCORSResponse'
    { _gbcrCORSRules :: !(Maybe [CORSRule])
    , _gbcrStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketCORSResponse' smart constructor.
getBucketCORSResponse :: Int -> GetBucketCORSResponse
getBucketCORSResponse pStatus =
    GetBucketCORSResponse'
    { _gbcrCORSRules = Nothing
    , _gbcrStatus = pStatus
    }

-- | FIXME: Undocumented member.
gbcrCORSRules :: Lens' GetBucketCORSResponse [CORSRule]
gbcrCORSRules = lens _gbcrCORSRules (\ s a -> s{_gbcrCORSRules = a}) . _Default;

-- | FIXME: Undocumented member.
gbcrStatus :: Lens' GetBucketCORSResponse Int
gbcrStatus = lens _gbcrStatus (\ s a -> s{_gbcrStatus = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketCORS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cors configuration for the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketCORS.html AWS API Reference> for GetBucketCORS.
module Network.AWS.S3.GetBucketCORS
    (
    -- * Creating a Request
      GetBucketCORS
    , getBucketCORS
    -- * Request Lenses
    , gbcBucket

    -- * Destructuring the Response
    , GetBucketCORSResponse
    , getBucketCORSResponse
    -- * Response Lenses
    , gbcrsCORSRules
    , gbcrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketCORS' smart constructor.
getBucketCORS :: BucketName -> GetBucketCORS
getBucketCORS pBucket_ =
    GetBucketCORS'
    { _gbcBucket = pBucket_
    }

-- | Undocumented member.
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
          = mconcat ["/", toBS _gbcBucket]

instance ToQuery GetBucketCORS where
        toQuery = const (mconcat ["cors"])

-- | /See:/ 'getBucketCORSResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbcrsCORSRules'
--
-- * 'gbcrsStatus'
data GetBucketCORSResponse = GetBucketCORSResponse'
    { _gbcrsCORSRules :: !(Maybe [CORSRule])
    , _gbcrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketCORSResponse' smart constructor.
getBucketCORSResponse :: Int -> GetBucketCORSResponse
getBucketCORSResponse pStatus_ =
    GetBucketCORSResponse'
    { _gbcrsCORSRules = Nothing
    , _gbcrsStatus = pStatus_
    }

-- | Undocumented member.
gbcrsCORSRules :: Lens' GetBucketCORSResponse [CORSRule]
gbcrsCORSRules = lens _gbcrsCORSRules (\ s a -> s{_gbcrsCORSRules = a}) . _Default . _Coerce;

-- | Undocumented member.
gbcrsStatus :: Lens' GetBucketCORSResponse Int
gbcrsStatus = lens _gbcrsStatus (\ s a -> s{_gbcrsStatus = a});

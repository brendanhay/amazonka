{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the versioning state of a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketVersioning.html>
module Network.AWS.S3.GetBucketVersioning
    (
    -- * Request
      GetBucketVersioning
    -- ** Request constructor
    , getBucketVersioning
    -- ** Request lenses
    , gbvBucket

    -- * Response
    , GetBucketVersioningResponse
    -- ** Response constructor
    , getBucketVersioningResponse
    -- ** Response lenses
    , gbvrMFADelete
    , gbvrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketVersioning' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbvBucket'
newtype GetBucketVersioning = GetBucketVersioning'
    { _gbvBucket :: BucketName
    } deriving (Eq,Show)

-- | 'GetBucketVersioning' smart constructor.
getBucketVersioning :: BucketName -> GetBucketVersioning
getBucketVersioning pBucket =
    GetBucketVersioning'
    { _gbvBucket = pBucket
    }

-- | FIXME: Undocumented member.
gbvBucket :: Lens' GetBucketVersioning BucketName
gbvBucket = lens _gbvBucket (\ s a -> s{_gbvBucket = a});

instance AWSRequest GetBucketVersioning where
        type Sv GetBucketVersioning = S3
        type Rs GetBucketVersioning =
             GetBucketVersioningResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketVersioningResponse' <$>
                   (x .@? "MfaDelete") <*> (pure (fromEnum s)))

instance ToHeaders GetBucketVersioning where
        toHeaders = const mempty

instance ToPath GetBucketVersioning where
        toPath GetBucketVersioning'{..}
          = mconcat ["/", toText _gbvBucket]

instance ToQuery GetBucketVersioning where
        toQuery = const (mconcat ["versioning"])

-- | /See:/ 'getBucketVersioningResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbvrMFADelete'
--
-- * 'gbvrStatus'
data GetBucketVersioningResponse = GetBucketVersioningResponse'
    { _gbvrMFADelete :: !(Maybe MFADeleteStatus)
    , _gbvrStatus    :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetBucketVersioningResponse' smart constructor.
getBucketVersioningResponse :: Int -> GetBucketVersioningResponse
getBucketVersioningResponse pStatus =
    GetBucketVersioningResponse'
    { _gbvrMFADelete = Nothing
    , _gbvrStatus = pStatus
    }

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
gbvrMFADelete :: Lens' GetBucketVersioningResponse (Maybe MFADeleteStatus)
gbvrMFADelete = lens _gbvrMFADelete (\ s a -> s{_gbvrMFADelete = a});

-- | FIXME: Undocumented member.
gbvrStatus :: Lens' GetBucketVersioningResponse Int
gbvrStatus = lens _gbvrStatus (\ s a -> s{_gbvrStatus = a});

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.PutBucketCORS
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

-- | Sets the cors configuration for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketCORS.html>
module Network.AWS.S3.PutBucketCORS
    (
    -- * Request
      PutBucketCORS
    -- ** Request constructor
    , putBucketCORS
    -- ** Request lenses
    , pbcContentMD5
    , pbcCORSConfiguration
    , pbcBucket

    -- * Response
    , PutBucketCORSResponse
    -- ** Response constructor
    , putBucketCORSResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.S3.Types

-- | /See:/ 'putBucketCORS' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbcContentMD5'
--
-- * 'pbcCORSConfiguration'
--
-- * 'pbcBucket'
data PutBucketCORS = PutBucketCORS'{_pbcContentMD5 :: Maybe Text, _pbcCORSConfiguration :: Maybe CORSConfiguration, _pbcBucket :: BucketName} deriving (Eq, Read, Show)

-- | 'PutBucketCORS' smart constructor.
putBucketCORS :: BucketName -> PutBucketCORS
putBucketCORS pBucket = PutBucketCORS'{_pbcContentMD5 = Nothing, _pbcCORSConfiguration = Nothing, _pbcBucket = pBucket};

-- | FIXME: Undocumented member.
pbcContentMD5 :: Lens' PutBucketCORS (Maybe Text)
pbcContentMD5 = lens _pbcContentMD5 (\ s a -> s{_pbcContentMD5 = a});

-- | FIXME: Undocumented member.
pbcCORSConfiguration :: Lens' PutBucketCORS (Maybe CORSConfiguration)
pbcCORSConfiguration = lens _pbcCORSConfiguration (\ s a -> s{_pbcCORSConfiguration = a});

-- | FIXME: Undocumented member.
pbcBucket :: Lens' PutBucketCORS BucketName
pbcBucket = lens _pbcBucket (\ s a -> s{_pbcBucket = a});

instance AWSRequest PutBucketCORS where
        type Sv PutBucketCORS = S3
        type Rs PutBucketCORS = PutBucketCORSResponse
        request = putXML
        response = receiveNull PutBucketCORSResponse'

instance ToElement PutBucketCORS where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}CORSConfiguration"

instance ToHeaders PutBucketCORS where
        toHeaders PutBucketCORS'{..}
          = mconcat ["Content-MD5" =# _pbcContentMD5]

instance ToPath PutBucketCORS where
        toPath PutBucketCORS'{..}
          = mconcat ["/", toText _pbcBucket]

instance ToQuery PutBucketCORS where
        toQuery = const (mconcat ["cors"])

instance ToXML PutBucketCORS where
        toXML PutBucketCORS'{..}
          = mconcat
              ["CORSConfiguration" @= _pbcCORSConfiguration]

-- | /See:/ 'putBucketCORSResponse' smart constructor.
data PutBucketCORSResponse = PutBucketCORSResponse' deriving (Eq, Read, Show)

-- | 'PutBucketCORSResponse' smart constructor.
putBucketCORSResponse :: PutBucketCORSResponse
putBucketCORSResponse = PutBucketCORSResponse';

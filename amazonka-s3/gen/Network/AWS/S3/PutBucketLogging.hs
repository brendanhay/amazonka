{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.PutBucketLogging
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

-- | Set the logging parameters for a bucket and to specify permissions for
-- who can view and modify the logging parameters. To set the logging
-- status of a bucket, you must be the bucket owner.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketLogging.html>
module Network.AWS.S3.PutBucketLogging
    (
    -- * Request
      PutBucketLogging
    -- ** Request constructor
    , putBucketLogging
    -- ** Request lenses
    , pblContentMD5
    , pblBucket
    , pblBucketLoggingStatus

    -- * Response
    , PutBucketLoggingResponse
    -- ** Response constructor
    , putBucketLoggingResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketLogging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pblContentMD5'
--
-- * 'pblBucket'
--
-- * 'pblBucketLoggingStatus'
data PutBucketLogging = PutBucketLogging'
    { _pblContentMD5          :: !(Maybe Text)
    , _pblBucket              :: !BucketName
    , _pblBucketLoggingStatus :: !BucketLoggingStatus
    } deriving (Eq,Show)

-- | 'PutBucketLogging' smart constructor.
putBucketLogging :: BucketName -> BucketLoggingStatus -> PutBucketLogging
putBucketLogging pBucket pBucketLoggingStatus =
    PutBucketLogging'
    { _pblContentMD5 = Nothing
    , _pblBucket = pBucket
    , _pblBucketLoggingStatus = pBucketLoggingStatus
    }

-- | FIXME: Undocumented member.
pblContentMD5 :: Lens' PutBucketLogging (Maybe Text)
pblContentMD5 = lens _pblContentMD5 (\ s a -> s{_pblContentMD5 = a});

-- | FIXME: Undocumented member.
pblBucket :: Lens' PutBucketLogging BucketName
pblBucket = lens _pblBucket (\ s a -> s{_pblBucket = a});

-- | FIXME: Undocumented member.
pblBucketLoggingStatus :: Lens' PutBucketLogging BucketLoggingStatus
pblBucketLoggingStatus = lens _pblBucketLoggingStatus (\ s a -> s{_pblBucketLoggingStatus = a});

instance AWSRequest PutBucketLogging where
        type Sv PutBucketLogging = S3
        type Rs PutBucketLogging = PutBucketLoggingResponse
        request = putXML
        response = receiveNull PutBucketLoggingResponse'

instance ToElement PutBucketLogging where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}BucketLoggingStatus"
              .
              _pblBucketLoggingStatus

instance ToHeaders PutBucketLogging where
        toHeaders PutBucketLogging'{..}
          = mconcat ["Content-MD5" =# _pblContentMD5]

instance ToPath PutBucketLogging where
        toPath PutBucketLogging'{..}
          = mconcat ["/", toText _pblBucket]

instance ToQuery PutBucketLogging where
        toQuery = const (mconcat ["logging"])

-- | /See:/ 'putBucketLoggingResponse' smart constructor.
data PutBucketLoggingResponse =
    PutBucketLoggingResponse'
    deriving (Eq,Read,Show)

-- | 'PutBucketLoggingResponse' smart constructor.
putBucketLoggingResponse :: PutBucketLoggingResponse
putBucketLoggingResponse = PutBucketLoggingResponse'

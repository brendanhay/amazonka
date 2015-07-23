{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketLogging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Set the logging parameters for a bucket and to specify permissions for
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
    , pblrqContentMD5
    , pblrqBucket
    , pblrqBucketLoggingStatus

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
-- * 'pblrqContentMD5'
--
-- * 'pblrqBucket'
--
-- * 'pblrqBucketLoggingStatus'
data PutBucketLogging = PutBucketLogging'
    { _pblrqContentMD5          :: !(Maybe Text)
    , _pblrqBucket              :: !BucketName
    , _pblrqBucketLoggingStatus :: !BucketLoggingStatus
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketLogging' smart constructor.
putBucketLogging :: BucketName -> BucketLoggingStatus -> PutBucketLogging
putBucketLogging pBucket_ pBucketLoggingStatus_ =
    PutBucketLogging'
    { _pblrqContentMD5 = Nothing
    , _pblrqBucket = pBucket_
    , _pblrqBucketLoggingStatus = pBucketLoggingStatus_
    }

-- | FIXME: Undocumented member.
pblrqContentMD5 :: Lens' PutBucketLogging (Maybe Text)
pblrqContentMD5 = lens _pblrqContentMD5 (\ s a -> s{_pblrqContentMD5 = a});

-- | FIXME: Undocumented member.
pblrqBucket :: Lens' PutBucketLogging BucketName
pblrqBucket = lens _pblrqBucket (\ s a -> s{_pblrqBucket = a});

-- | FIXME: Undocumented member.
pblrqBucketLoggingStatus :: Lens' PutBucketLogging BucketLoggingStatus
pblrqBucketLoggingStatus = lens _pblrqBucketLoggingStatus (\ s a -> s{_pblrqBucketLoggingStatus = a});

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
              _pblrqBucketLoggingStatus

instance ToHeaders PutBucketLogging where
        toHeaders PutBucketLogging'{..}
          = mconcat ["Content-MD5" =# _pblrqContentMD5]

instance ToPath PutBucketLogging where
        toPath PutBucketLogging'{..}
          = mconcat ["/", toText _pblrqBucket]

instance ToQuery PutBucketLogging where
        toQuery = const (mconcat ["logging"])

-- | /See:/ 'putBucketLoggingResponse' smart constructor.
data PutBucketLoggingResponse =
    PutBucketLoggingResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketLoggingResponse' smart constructor.
putBucketLoggingResponse :: PutBucketLoggingResponse
putBucketLoggingResponse = PutBucketLoggingResponse'

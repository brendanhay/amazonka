{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketLifecycle
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets lifecycle configuration for your bucket. If a lifecycle
-- configuration exists, it replaces it.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketLifecycle.html>
module Network.AWS.S3.PutBucketLifecycle
    (
    -- * Request
      PutBucketLifecycle
    -- ** Request constructor
    , putBucketLifecycle
    -- ** Request lenses
    , prqContentMD5
    , prqLifecycleConfiguration
    , prqBucket

    -- * Response
    , PutBucketLifecycleResponse
    -- ** Response constructor
    , putBucketLifecycleResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketLifecycle' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prqContentMD5'
--
-- * 'prqLifecycleConfiguration'
--
-- * 'prqBucket'
data PutBucketLifecycle = PutBucketLifecycle'
    { _prqContentMD5             :: !(Maybe Text)
    , _prqLifecycleConfiguration :: !(Maybe LifecycleConfiguration)
    , _prqBucket                 :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketLifecycle' smart constructor.
putBucketLifecycle :: BucketName -> PutBucketLifecycle
putBucketLifecycle pBucket =
    PutBucketLifecycle'
    { _prqContentMD5 = Nothing
    , _prqLifecycleConfiguration = Nothing
    , _prqBucket = pBucket
    }

-- | FIXME: Undocumented member.
prqContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
prqContentMD5 = lens _prqContentMD5 (\ s a -> s{_prqContentMD5 = a});

-- | FIXME: Undocumented member.
prqLifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
prqLifecycleConfiguration = lens _prqLifecycleConfiguration (\ s a -> s{_prqLifecycleConfiguration = a});

-- | FIXME: Undocumented member.
prqBucket :: Lens' PutBucketLifecycle BucketName
prqBucket = lens _prqBucket (\ s a -> s{_prqBucket = a});

instance AWSRequest PutBucketLifecycle where
        type Sv PutBucketLifecycle = S3
        type Rs PutBucketLifecycle =
             PutBucketLifecycleResponse
        request = putXML
        response = receiveNull PutBucketLifecycleResponse'

instance ToElement PutBucketLifecycle where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}LifecycleConfiguration"
              .
              _prqLifecycleConfiguration

instance ToHeaders PutBucketLifecycle where
        toHeaders PutBucketLifecycle'{..}
          = mconcat ["Content-MD5" =# _prqContentMD5]

instance ToPath PutBucketLifecycle where
        toPath PutBucketLifecycle'{..}
          = mconcat ["/", toText _prqBucket]

instance ToQuery PutBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'putBucketLifecycleResponse' smart constructor.
data PutBucketLifecycleResponse =
    PutBucketLifecycleResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketLifecycleResponse' smart constructor.
putBucketLifecycleResponse :: PutBucketLifecycleResponse
putBucketLifecycleResponse = PutBucketLifecycleResponse'

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
    , putContentMD5
    , putLifecycleConfiguration
    , putBucket

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
-- * 'putContentMD5'
--
-- * 'putLifecycleConfiguration'
--
-- * 'putBucket'
data PutBucketLifecycle = PutBucketLifecycle'
    { _putContentMD5             :: !(Maybe Text)
    , _putLifecycleConfiguration :: !(Maybe LifecycleConfiguration)
    , _putBucket                 :: !BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketLifecycle' smart constructor.
putBucketLifecycle :: BucketName -> PutBucketLifecycle
putBucketLifecycle pBucket =
    PutBucketLifecycle'
    { _putContentMD5 = Nothing
    , _putLifecycleConfiguration = Nothing
    , _putBucket = pBucket
    }

-- | FIXME: Undocumented member.
putContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
putContentMD5 = lens _putContentMD5 (\ s a -> s{_putContentMD5 = a});

-- | FIXME: Undocumented member.
putLifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
putLifecycleConfiguration = lens _putLifecycleConfiguration (\ s a -> s{_putLifecycleConfiguration = a});

-- | FIXME: Undocumented member.
putBucket :: Lens' PutBucketLifecycle BucketName
putBucket = lens _putBucket (\ s a -> s{_putBucket = a});

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
              _putLifecycleConfiguration

instance ToHeaders PutBucketLifecycle where
        toHeaders PutBucketLifecycle'{..}
          = mconcat ["Content-MD5" =# _putContentMD5]

instance ToPath PutBucketLifecycle where
        toPath PutBucketLifecycle'{..}
          = mconcat ["/", toText _putBucket]

instance ToQuery PutBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'putBucketLifecycleResponse' smart constructor.
data PutBucketLifecycleResponse =
    PutBucketLifecycleResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketLifecycleResponse' smart constructor.
putBucketLifecycleResponse :: PutBucketLifecycleResponse
putBucketLifecycleResponse = PutBucketLifecycleResponse'

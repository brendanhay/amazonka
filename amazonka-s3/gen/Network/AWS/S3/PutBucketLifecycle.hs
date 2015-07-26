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
    , pContentMD5
    , pLifecycleConfiguration
    , pBucket

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
-- * 'pContentMD5'
--
-- * 'pLifecycleConfiguration'
--
-- * 'pBucket'
data PutBucketLifecycle = PutBucketLifecycle'
    { _pContentMD5             :: !(Maybe Text)
    , _pLifecycleConfiguration :: !(Maybe LifecycleConfiguration)
    , _pBucket                 :: !BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketLifecycle' smart constructor.
putBucketLifecycle :: BucketName -> PutBucketLifecycle
putBucketLifecycle pBucket_ =
    PutBucketLifecycle'
    { _pContentMD5 = Nothing
    , _pLifecycleConfiguration = Nothing
    , _pBucket = pBucket_
    }

-- | FIXME: Undocumented member.
pContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
pContentMD5 = lens _pContentMD5 (\ s a -> s{_pContentMD5 = a});

-- | FIXME: Undocumented member.
pLifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
pLifecycleConfiguration = lens _pLifecycleConfiguration (\ s a -> s{_pLifecycleConfiguration = a});

-- | FIXME: Undocumented member.
pBucket :: Lens' PutBucketLifecycle BucketName
pBucket = lens _pBucket (\ s a -> s{_pBucket = a});

instance AWSRequest PutBucketLifecycle where
        type Sv PutBucketLifecycle = S3
        type Rs PutBucketLifecycle =
             PutBucketLifecycleResponse
        request = contentMD5 . putXML
        response = receiveNull PutBucketLifecycleResponse'

instance ToElement PutBucketLifecycle where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}LifecycleConfiguration"
              .
              _pLifecycleConfiguration

instance ToHeaders PutBucketLifecycle where
        toHeaders PutBucketLifecycle'{..}
          = mconcat ["Content-MD5" =# _pContentMD5]

instance ToPath PutBucketLifecycle where
        toPath PutBucketLifecycle'{..}
          = mconcat ["/", toPath _pBucket]

instance ToQuery PutBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'putBucketLifecycleResponse' smart constructor.
data PutBucketLifecycleResponse =
    PutBucketLifecycleResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketLifecycleResponse' smart constructor.
putBucketLifecycleResponse :: PutBucketLifecycleResponse
putBucketLifecycleResponse = PutBucketLifecycleResponse'

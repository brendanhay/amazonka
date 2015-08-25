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
-- Module      : Network.AWS.S3.PutBucketLifecycle
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets lifecycle configuration for your bucket. If a lifecycle
-- configuration exists, it replaces it.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketLifecycle.html AWS API Reference> for PutBucketLifecycle.
module Network.AWS.S3.PutBucketLifecycle
    (
    -- * Creating a Request
      putBucketLifecycle
    , PutBucketLifecycle
    -- * Request Lenses
    , pContentMD5
    , pLifecycleConfiguration
    , pBucket

    -- * Destructuring the Response
    , putBucketLifecycleResponse
    , PutBucketLifecycleResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketLifecycle' smart constructor.
data PutBucketLifecycle = PutBucketLifecycle'
    { _pContentMD5             :: !(Maybe Text)
    , _pLifecycleConfiguration :: !(Maybe LifecycleConfiguration)
    , _pBucket                 :: !BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutBucketLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pContentMD5'
--
-- * 'pLifecycleConfiguration'
--
-- * 'pBucket'
putBucketLifecycle
    :: BucketName -- ^ 'pBucket'
    -> PutBucketLifecycle
putBucketLifecycle pBucket_ =
    PutBucketLifecycle'
    { _pContentMD5 = Nothing
    , _pLifecycleConfiguration = Nothing
    , _pBucket = pBucket_
    }

-- | Undocumented member.
pContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
pContentMD5 = lens _pContentMD5 (\ s a -> s{_pContentMD5 = a});

-- | Undocumented member.
pLifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
pLifecycleConfiguration = lens _pLifecycleConfiguration (\ s a -> s{_pLifecycleConfiguration = a});

-- | Undocumented member.
pBucket :: Lens' PutBucketLifecycle BucketName
pBucket = lens _pBucket (\ s a -> s{_pBucket = a});

instance AWSRequest PutBucketLifecycle where
        type Rs PutBucketLifecycle =
             PutBucketLifecycleResponse
        request = contentMD5 . putXML s3
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
          = mconcat ["/", toBS _pBucket]

instance ToQuery PutBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'putBucketLifecycleResponse' smart constructor.
data PutBucketLifecycleResponse =
    PutBucketLifecycleResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutBucketLifecycleResponse' with the minimum fields required to make a request.
--
putBucketLifecycleResponse
    :: PutBucketLifecycleResponse
putBucketLifecycleResponse = PutBucketLifecycleResponse'

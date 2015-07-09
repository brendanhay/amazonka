{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Replaces a policy on a bucket. If the bucket already has a policy, the
-- one in this request completely replaces it.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketPolicy.html>
module Network.AWS.S3.PutBucketPolicy
    (
    -- * Request
      PutBucketPolicy
    -- ** Request constructor
    , putBucketPolicy
    -- ** Request lenses
    , pbpContentMD5
    , pbpBucket
    , pbpPolicy

    -- * Response
    , PutBucketPolicyResponse
    -- ** Response constructor
    , putBucketPolicyResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbpContentMD5'
--
-- * 'pbpBucket'
--
-- * 'pbpPolicy'
data PutBucketPolicy = PutBucketPolicy'
    { _pbpContentMD5 :: !(Maybe Text)
    , _pbpBucket     :: !BucketName
    , _pbpPolicy     :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketPolicy' smart constructor.
putBucketPolicy :: BucketName -> Text -> PutBucketPolicy
putBucketPolicy pBucket pPolicy =
    PutBucketPolicy'
    { _pbpContentMD5 = Nothing
    , _pbpBucket = pBucket
    , _pbpPolicy = pPolicy
    }

-- | FIXME: Undocumented member.
pbpContentMD5 :: Lens' PutBucketPolicy (Maybe Text)
pbpContentMD5 = lens _pbpContentMD5 (\ s a -> s{_pbpContentMD5 = a});

-- | FIXME: Undocumented member.
pbpBucket :: Lens' PutBucketPolicy BucketName
pbpBucket = lens _pbpBucket (\ s a -> s{_pbpBucket = a});

-- | The bucket policy as a JSON document.
pbpPolicy :: Lens' PutBucketPolicy Text
pbpPolicy = lens _pbpPolicy (\ s a -> s{_pbpPolicy = a});

instance AWSRequest PutBucketPolicy where
        type Sv PutBucketPolicy = S3
        type Rs PutBucketPolicy = PutBucketPolicyResponse
        request = putXML
        response = receiveNull PutBucketPolicyResponse'

instance ToElement PutBucketPolicy where
        toElement = mkElement "Policy" . _pbpPolicy

instance ToHeaders PutBucketPolicy where
        toHeaders PutBucketPolicy'{..}
          = mconcat ["Content-MD5" =# _pbpContentMD5]

instance ToPath PutBucketPolicy where
        toPath PutBucketPolicy'{..}
          = mconcat ["/", toText _pbpBucket]

instance ToQuery PutBucketPolicy where
        toQuery = const (mconcat ["policy"])

-- | /See:/ 'putBucketPolicyResponse' smart constructor.
data PutBucketPolicyResponse =
    PutBucketPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketPolicyResponse' smart constructor.
putBucketPolicyResponse :: PutBucketPolicyResponse
putBucketPolicyResponse = PutBucketPolicyResponse'

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Replaces a policy on a bucket. If the bucket already has a policy, the
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
    , pbprqContentMD5
    , pbprqBucket
    , pbprqPolicy

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
-- * 'pbprqContentMD5'
--
-- * 'pbprqBucket'
--
-- * 'pbprqPolicy'
data PutBucketPolicy = PutBucketPolicy'
    { _pbprqContentMD5 :: !(Maybe Text)
    , _pbprqBucket     :: !BucketName
    , _pbprqPolicy     :: !Text
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketPolicy' smart constructor.
putBucketPolicy :: BucketName -> Text -> PutBucketPolicy
putBucketPolicy pBucket_ pPolicy_ =
    PutBucketPolicy'
    { _pbprqContentMD5 = Nothing
    , _pbprqBucket = pBucket_
    , _pbprqPolicy = pPolicy_
    }

-- | FIXME: Undocumented member.
pbprqContentMD5 :: Lens' PutBucketPolicy (Maybe Text)
pbprqContentMD5 = lens _pbprqContentMD5 (\ s a -> s{_pbprqContentMD5 = a});

-- | FIXME: Undocumented member.
pbprqBucket :: Lens' PutBucketPolicy BucketName
pbprqBucket = lens _pbprqBucket (\ s a -> s{_pbprqBucket = a});

-- | The bucket policy as a JSON document.
pbprqPolicy :: Lens' PutBucketPolicy Text
pbprqPolicy = lens _pbprqPolicy (\ s a -> s{_pbprqPolicy = a});

instance AWSRequest PutBucketPolicy where
        type Sv PutBucketPolicy = S3
        type Rs PutBucketPolicy = PutBucketPolicyResponse
        request = putXML
        response = receiveNull PutBucketPolicyResponse'

instance ToElement PutBucketPolicy where
        toElement = mkElement "Policy" . _pbprqPolicy

instance ToHeaders PutBucketPolicy where
        toHeaders PutBucketPolicy'{..}
          = mconcat ["Content-MD5" =# _pbprqContentMD5]

instance ToPath PutBucketPolicy where
        toPath PutBucketPolicy'{..}
          = mconcat ["/", toText _pbprqBucket]

instance ToQuery PutBucketPolicy where
        toQuery = const (mconcat ["policy"])

-- | /See:/ 'putBucketPolicyResponse' smart constructor.
data PutBucketPolicyResponse =
    PutBucketPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketPolicyResponse' smart constructor.
putBucketPolicyResponse :: PutBucketPolicyResponse
putBucketPolicyResponse = PutBucketPolicyResponse'

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
-- Module      : Network.AWS.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces a policy on a bucket. If the bucket already has a policy, the
-- one in this request completely replaces it.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketPolicy.html AWS API Reference> for PutBucketPolicy.
module Network.AWS.S3.PutBucketPolicy
    (
    -- * Creating a Request
      putBucketPolicy
    , PutBucketPolicy
    -- * Request Lenses
    , pbpContentMD5
    , pbpBucket
    , pbpPolicy

    -- * Destructuring the Response
    , putBucketPolicyResponse
    , PutBucketPolicyResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketPolicy' smart constructor.
data PutBucketPolicy = PutBucketPolicy'
    { _pbpContentMD5 :: !(Maybe Text)
    , _pbpBucket     :: !BucketName
    , _pbpPolicy     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutBucketPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbpContentMD5'
--
-- * 'pbpBucket'
--
-- * 'pbpPolicy'
putBucketPolicy
    :: BucketName -- ^ 'pbpBucket'
    -> Text -- ^ 'pbpPolicy'
    -> PutBucketPolicy
putBucketPolicy pBucket_ pPolicy_ =
    PutBucketPolicy'
    { _pbpContentMD5 = Nothing
    , _pbpBucket = pBucket_
    , _pbpPolicy = pPolicy_
    }

-- | Undocumented member.
pbpContentMD5 :: Lens' PutBucketPolicy (Maybe Text)
pbpContentMD5 = lens _pbpContentMD5 (\ s a -> s{_pbpContentMD5 = a});

-- | Undocumented member.
pbpBucket :: Lens' PutBucketPolicy BucketName
pbpBucket = lens _pbpBucket (\ s a -> s{_pbpBucket = a});

-- | The bucket policy as a JSON document.
pbpPolicy :: Lens' PutBucketPolicy Text
pbpPolicy = lens _pbpPolicy (\ s a -> s{_pbpPolicy = a});

instance AWSRequest PutBucketPolicy where
        type Rs PutBucketPolicy = PutBucketPolicyResponse
        request = contentMD5 . putXML s3
        response = receiveNull PutBucketPolicyResponse'

instance ToElement PutBucketPolicy where
        toElement = mkElement "Policy" . _pbpPolicy

instance ToHeaders PutBucketPolicy where
        toHeaders PutBucketPolicy'{..}
          = mconcat ["Content-MD5" =# _pbpContentMD5]

instance ToPath PutBucketPolicy where
        toPath PutBucketPolicy'{..}
          = mconcat ["/", toBS _pbpBucket]

instance ToQuery PutBucketPolicy where
        toQuery = const (mconcat ["policy"])

-- | /See:/ 'putBucketPolicyResponse' smart constructor.
data PutBucketPolicyResponse =
    PutBucketPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PutBucketPolicyResponse' with the minimum fields required to make a request.
--
putBucketPolicyResponse
    :: PutBucketPolicyResponse
putBucketPolicyResponse = PutBucketPolicyResponse'

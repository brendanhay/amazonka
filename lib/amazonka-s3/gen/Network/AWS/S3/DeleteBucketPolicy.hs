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
-- Module      : Network.AWS.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the policy from the bucket.
module Network.AWS.S3.DeleteBucketPolicy
    (
    -- * Creating a Request
      deleteBucketPolicy
    , DeleteBucketPolicy
    -- * Request Lenses
    , dbpBucket

    -- * Destructuring the Response
    , deleteBucketPolicyResponse
    , DeleteBucketPolicyResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketPolicy' smart constructor.
newtype DeleteBucketPolicy = DeleteBucketPolicy'
  { _dbpBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbpBucket' - Undocumented member.
deleteBucketPolicy
    :: BucketName -- ^ 'dbpBucket'
    -> DeleteBucketPolicy
deleteBucketPolicy pBucket_ = DeleteBucketPolicy' {_dbpBucket = pBucket_}


-- | Undocumented member.
dbpBucket :: Lens' DeleteBucketPolicy BucketName
dbpBucket = lens _dbpBucket (\ s a -> s{_dbpBucket = a})

instance AWSRequest DeleteBucketPolicy where
        type Rs DeleteBucketPolicy =
             DeleteBucketPolicyResponse
        request = delete s3
        response = receiveNull DeleteBucketPolicyResponse'

instance Hashable DeleteBucketPolicy where

instance NFData DeleteBucketPolicy where

instance ToHeaders DeleteBucketPolicy where
        toHeaders = const mempty

instance ToPath DeleteBucketPolicy where
        toPath DeleteBucketPolicy'{..}
          = mconcat ["/", toBS _dbpBucket]

instance ToQuery DeleteBucketPolicy where
        toQuery = const (mconcat ["policy"])

-- | /See:/ 'deleteBucketPolicyResponse' smart constructor.
data DeleteBucketPolicyResponse =
  DeleteBucketPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketPolicyResponse' with the minimum fields required to make a request.
--
deleteBucketPolicyResponse
    :: DeleteBucketPolicyResponse
deleteBucketPolicyResponse = DeleteBucketPolicyResponse'


instance NFData DeleteBucketPolicyResponse where

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
-- Module      : Network.AWS.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the tags from the bucket.
module Network.AWS.S3.DeleteBucketTagging
    (
    -- * Creating a Request
      deleteBucketTagging
    , DeleteBucketTagging
    -- * Request Lenses
    , dbtBucket

    -- * Destructuring the Response
    , deleteBucketTaggingResponse
    , DeleteBucketTaggingResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketTagging' smart constructor.
newtype DeleteBucketTagging = DeleteBucketTagging'
  { _dbtBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketTagging' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbtBucket' - Undocumented member.
deleteBucketTagging
    :: BucketName -- ^ 'dbtBucket'
    -> DeleteBucketTagging
deleteBucketTagging pBucket_ = DeleteBucketTagging' {_dbtBucket = pBucket_}


-- | Undocumented member.
dbtBucket :: Lens' DeleteBucketTagging BucketName
dbtBucket = lens _dbtBucket (\ s a -> s{_dbtBucket = a})

instance AWSRequest DeleteBucketTagging where
        type Rs DeleteBucketTagging =
             DeleteBucketTaggingResponse
        request = delete s3
        response = receiveNull DeleteBucketTaggingResponse'

instance Hashable DeleteBucketTagging where

instance NFData DeleteBucketTagging where

instance ToHeaders DeleteBucketTagging where
        toHeaders = const mempty

instance ToPath DeleteBucketTagging where
        toPath DeleteBucketTagging'{..}
          = mconcat ["/", toBS _dbtBucket]

instance ToQuery DeleteBucketTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'deleteBucketTaggingResponse' smart constructor.
data DeleteBucketTaggingResponse =
  DeleteBucketTaggingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketTaggingResponse' with the minimum fields required to make a request.
--
deleteBucketTaggingResponse
    :: DeleteBucketTaggingResponse
deleteBucketTaggingResponse = DeleteBucketTaggingResponse'


instance NFData DeleteBucketTaggingResponse where

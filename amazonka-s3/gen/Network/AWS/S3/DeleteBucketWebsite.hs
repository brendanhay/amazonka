{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the website configuration from the bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketWebsite.html AWS API Reference> for DeleteBucketWebsite.
module Network.AWS.S3.DeleteBucketWebsite
    (
    -- * Creating a Request
      DeleteBucketWebsite
    , deleteBucketWebsite
    -- * Request Lenses
    , dbwBucket

    -- * Destructuring the Response
    , DeleteBucketWebsiteResponse
    , deleteBucketWebsiteResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketWebsite' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbwBucket'
newtype DeleteBucketWebsite = DeleteBucketWebsite'
    { _dbwBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketWebsite' smart constructor.
deleteBucketWebsite :: BucketName -> DeleteBucketWebsite
deleteBucketWebsite pBucket_ =
    DeleteBucketWebsite'
    { _dbwBucket = pBucket_
    }

-- | Undocumented member.
dbwBucket :: Lens' DeleteBucketWebsite BucketName
dbwBucket = lens _dbwBucket (\ s a -> s{_dbwBucket = a});

instance AWSRequest DeleteBucketWebsite where
        type Sv DeleteBucketWebsite = S3
        type Rs DeleteBucketWebsite =
             DeleteBucketWebsiteResponse
        request = delete
        response = receiveNull DeleteBucketWebsiteResponse'

instance ToHeaders DeleteBucketWebsite where
        toHeaders = const mempty

instance ToPath DeleteBucketWebsite where
        toPath DeleteBucketWebsite'{..}
          = mconcat ["/", toBS _dbwBucket]

instance ToQuery DeleteBucketWebsite where
        toQuery = const (mconcat ["website"])

-- | /See:/ 'deleteBucketWebsiteResponse' smart constructor.
data DeleteBucketWebsiteResponse =
    DeleteBucketWebsiteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketWebsiteResponse' smart constructor.
deleteBucketWebsiteResponse :: DeleteBucketWebsiteResponse
deleteBucketWebsiteResponse = DeleteBucketWebsiteResponse'

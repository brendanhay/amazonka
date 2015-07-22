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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the website configuration from the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketWebsite.html>
module Network.AWS.S3.DeleteBucketWebsite
    (
    -- * Request
      DeleteBucketWebsite
    -- ** Request constructor
    , deleteBucketWebsite
    -- ** Request lenses
    , dbwrqBucket

    -- * Response
    , DeleteBucketWebsiteResponse
    -- ** Response constructor
    , deleteBucketWebsiteResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteBucketWebsite' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbwrqBucket'
newtype DeleteBucketWebsite = DeleteBucketWebsite'
    { _dbwrqBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeleteBucketWebsite' smart constructor.
deleteBucketWebsite :: BucketName -> DeleteBucketWebsite
deleteBucketWebsite pBucket =
    DeleteBucketWebsite'
    { _dbwrqBucket = pBucket
    }

-- | FIXME: Undocumented member.
dbwrqBucket :: Lens' DeleteBucketWebsite BucketName
dbwrqBucket = lens _dbwrqBucket (\ s a -> s{_dbwrqBucket = a});

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
          = mconcat ["/", toText _dbwrqBucket]

instance ToQuery DeleteBucketWebsite where
        toQuery = const (mconcat ["website"])

-- | /See:/ 'deleteBucketWebsiteResponse' smart constructor.
data DeleteBucketWebsiteResponse =
    DeleteBucketWebsiteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteBucketWebsiteResponse' smart constructor.
deleteBucketWebsiteResponse :: DeleteBucketWebsiteResponse
deleteBucketWebsiteResponse = DeleteBucketWebsiteResponse'

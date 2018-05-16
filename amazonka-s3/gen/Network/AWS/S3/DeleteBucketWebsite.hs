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
-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the website configuration from the bucket.
module Network.AWS.S3.DeleteBucketWebsite
    (
    -- * Creating a Request
      deleteBucketWebsite
    , DeleteBucketWebsite
    -- * Request Lenses
    , dbwBucket

    -- * Destructuring the Response
    , deleteBucketWebsiteResponse
    , DeleteBucketWebsiteResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteBucketWebsite' smart constructor.
newtype DeleteBucketWebsite = DeleteBucketWebsite'
  { _dbwBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketWebsite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbwBucket' - Undocumented member.
deleteBucketWebsite
    :: BucketName -- ^ 'dbwBucket'
    -> DeleteBucketWebsite
deleteBucketWebsite pBucket_ = DeleteBucketWebsite' {_dbwBucket = pBucket_}


-- | Undocumented member.
dbwBucket :: Lens' DeleteBucketWebsite BucketName
dbwBucket = lens _dbwBucket (\ s a -> s{_dbwBucket = a})

instance AWSRequest DeleteBucketWebsite where
        type Rs DeleteBucketWebsite =
             DeleteBucketWebsiteResponse
        request = delete s3
        response = receiveNull DeleteBucketWebsiteResponse'

instance Hashable DeleteBucketWebsite where

instance NFData DeleteBucketWebsite where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBucketWebsiteResponse' with the minimum fields required to make a request.
--
deleteBucketWebsiteResponse
    :: DeleteBucketWebsiteResponse
deleteBucketWebsiteResponse = DeleteBucketWebsiteResponse'


instance NFData DeleteBucketWebsiteResponse where

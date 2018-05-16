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
-- Module      : Network.AWS.S3.HeadBucket
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation is useful to determine if a bucket exists and you have permission to access it.
module Network.AWS.S3.HeadBucket
    (
    -- * Creating a Request
      headBucket
    , HeadBucket
    -- * Request Lenses
    , hbBucket

    -- * Destructuring the Response
    , headBucketResponse
    , HeadBucketResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'headBucket' smart constructor.
newtype HeadBucket = HeadBucket'
  { _hbBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HeadBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hbBucket' - Undocumented member.
headBucket
    :: BucketName -- ^ 'hbBucket'
    -> HeadBucket
headBucket pBucket_ = HeadBucket' {_hbBucket = pBucket_}


-- | Undocumented member.
hbBucket :: Lens' HeadBucket BucketName
hbBucket = lens _hbBucket (\ s a -> s{_hbBucket = a})

instance AWSRequest HeadBucket where
        type Rs HeadBucket = HeadBucketResponse
        request = head' s3
        response = receiveNull HeadBucketResponse'

instance Hashable HeadBucket where

instance NFData HeadBucket where

instance ToHeaders HeadBucket where
        toHeaders = const mempty

instance ToPath HeadBucket where
        toPath HeadBucket'{..}
          = mconcat ["/", toBS _hbBucket]

instance ToQuery HeadBucket where
        toQuery = const mempty

-- | /See:/ 'headBucketResponse' smart constructor.
data HeadBucketResponse =
  HeadBucketResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HeadBucketResponse' with the minimum fields required to make a request.
--
headBucketResponse
    :: HeadBucketResponse
headBucketResponse = HeadBucketResponse'


instance NFData HeadBucketResponse where

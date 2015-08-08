{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the versioning state of a bucket.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketVersioning.html AWS API Reference> for GetBucketVersioning.
module Network.AWS.S3.GetBucketVersioning
    (
    -- * Creating a Request
      GetBucketVersioning
    , getBucketVersioning
    -- * Request Lenses
    , gbvBucket

    -- * Destructuring the Response
    , GetBucketVersioningResponse
    , getBucketVersioningResponse
    -- * Response Lenses
    , gbvrsMFADelete
    , gbvrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketVersioning' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbvBucket'
newtype GetBucketVersioning = GetBucketVersioning'
    { _gbvBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketVersioning' smart constructor.
getBucketVersioning :: BucketName -> GetBucketVersioning
getBucketVersioning pBucket_ =
    GetBucketVersioning'
    { _gbvBucket = pBucket_
    }

-- | Undocumented member.
gbvBucket :: Lens' GetBucketVersioning BucketName
gbvBucket = lens _gbvBucket (\ s a -> s{_gbvBucket = a});

instance AWSRequest GetBucketVersioning where
        type Sv GetBucketVersioning = S3
        type Rs GetBucketVersioning =
             GetBucketVersioningResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketVersioningResponse' <$>
                   (x .@? "MfaDelete") <*> (pure (fromEnum s)))

instance ToHeaders GetBucketVersioning where
        toHeaders = const mempty

instance ToPath GetBucketVersioning where
        toPath GetBucketVersioning'{..}
          = mconcat ["/", toBS _gbvBucket]

instance ToQuery GetBucketVersioning where
        toQuery = const (mconcat ["versioning"])

-- | /See:/ 'getBucketVersioningResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbvrsMFADelete'
--
-- * 'gbvrsStatus'
data GetBucketVersioningResponse = GetBucketVersioningResponse'
    { _gbvrsMFADelete :: !(Maybe MFADeleteStatus)
    , _gbvrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketVersioningResponse' smart constructor.
getBucketVersioningResponse :: Int -> GetBucketVersioningResponse
getBucketVersioningResponse pStatus_ =
    GetBucketVersioningResponse'
    { _gbvrsMFADelete = Nothing
    , _gbvrsStatus = pStatus_
    }

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
gbvrsMFADelete :: Lens' GetBucketVersioningResponse (Maybe MFADeleteStatus)
gbvrsMFADelete = lens _gbvrsMFADelete (\ s a -> s{_gbvrsMFADelete = a});

-- | Undocumented member.
gbvrsStatus :: Lens' GetBucketVersioningResponse Int
gbvrsStatus = lens _gbvrsStatus (\ s a -> s{_gbvrsStatus = a});

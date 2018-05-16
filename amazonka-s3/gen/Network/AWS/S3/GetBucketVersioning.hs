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
-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the versioning state of a bucket.
module Network.AWS.S3.GetBucketVersioning
    (
    -- * Creating a Request
      getBucketVersioning
    , GetBucketVersioning
    -- * Request Lenses
    , gbvBucket

    -- * Destructuring the Response
    , getBucketVersioningResponse
    , GetBucketVersioningResponse
    -- * Response Lenses
    , gbvrsStatus
    , gbvrsMFADelete
    , gbvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'getBucketVersioning' smart constructor.
newtype GetBucketVersioning = GetBucketVersioning'
  { _gbvBucket :: BucketName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketVersioning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvBucket' - Undocumented member.
getBucketVersioning
    :: BucketName -- ^ 'gbvBucket'
    -> GetBucketVersioning
getBucketVersioning pBucket_ = GetBucketVersioning' {_gbvBucket = pBucket_}


-- | Undocumented member.
gbvBucket :: Lens' GetBucketVersioning BucketName
gbvBucket = lens _gbvBucket (\ s a -> s{_gbvBucket = a})

instance AWSRequest GetBucketVersioning where
        type Rs GetBucketVersioning =
             GetBucketVersioningResponse
        request = get s3
        response
          = receiveXML
              (\ s h x ->
                 GetBucketVersioningResponse' <$>
                   (x .@? "Status") <*> (x .@? "MfaDelete") <*>
                     (pure (fromEnum s)))

instance Hashable GetBucketVersioning where

instance NFData GetBucketVersioning where

instance ToHeaders GetBucketVersioning where
        toHeaders = const mempty

instance ToPath GetBucketVersioning where
        toPath GetBucketVersioning'{..}
          = mconcat ["/", toBS _gbvBucket]

instance ToQuery GetBucketVersioning where
        toQuery = const (mconcat ["versioning"])

-- | /See:/ 'getBucketVersioningResponse' smart constructor.
data GetBucketVersioningResponse = GetBucketVersioningResponse'
  { _gbvrsStatus         :: !(Maybe BucketVersioningStatus)
  , _gbvrsMFADelete      :: !(Maybe MFADeleteStatus)
  , _gbvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBucketVersioningResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbvrsStatus' - The versioning state of the bucket.
--
-- * 'gbvrsMFADelete' - Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
--
-- * 'gbvrsResponseStatus' - -- | The response status code.
getBucketVersioningResponse
    :: Int -- ^ 'gbvrsResponseStatus'
    -> GetBucketVersioningResponse
getBucketVersioningResponse pResponseStatus_ =
  GetBucketVersioningResponse'
    { _gbvrsStatus = Nothing
    , _gbvrsMFADelete = Nothing
    , _gbvrsResponseStatus = pResponseStatus_
    }


-- | The versioning state of the bucket.
gbvrsStatus :: Lens' GetBucketVersioningResponse (Maybe BucketVersioningStatus)
gbvrsStatus = lens _gbvrsStatus (\ s a -> s{_gbvrsStatus = a})

-- | Specifies whether MFA delete is enabled in the bucket versioning configuration. This element is only returned if the bucket has been configured with MFA delete. If the bucket has never been so configured, this element is not returned.
gbvrsMFADelete :: Lens' GetBucketVersioningResponse (Maybe MFADeleteStatus)
gbvrsMFADelete = lens _gbvrsMFADelete (\ s a -> s{_gbvrsMFADelete = a})

-- | -- | The response status code.
gbvrsResponseStatus :: Lens' GetBucketVersioningResponse Int
gbvrsResponseStatus = lens _gbvrsResponseStatus (\ s a -> s{_gbvrsResponseStatus = a})

instance NFData GetBucketVersioningResponse where

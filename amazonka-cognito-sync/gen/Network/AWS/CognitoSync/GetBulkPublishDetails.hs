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
-- Module      : Network.AWS.CognitoSync.GetBulkPublishDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the status of the last BulkPublish operation for an identity pool.
--
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
--
module Network.AWS.CognitoSync.GetBulkPublishDetails
    (
    -- * Creating a Request
      getBulkPublishDetails
    , GetBulkPublishDetails
    -- * Request Lenses
    , gbpdIdentityPoolId

    -- * Destructuring the Response
    , getBulkPublishDetailsResponse
    , GetBulkPublishDetailsResponse
    -- * Response Lenses
    , gbpdrsBulkPublishStartTime
    , gbpdrsIdentityPoolId
    , gbpdrsBulkPublishCompleteTime
    , gbpdrsFailureMessage
    , gbpdrsBulkPublishStatus
    , gbpdrsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the GetBulkPublishDetails operation.
--
-- /See:/ 'getBulkPublishDetails' smart constructor.
newtype GetBulkPublishDetails = GetBulkPublishDetails'
  { _gbpdIdentityPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBulkPublishDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpdIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
getBulkPublishDetails
    :: Text -- ^ 'gbpdIdentityPoolId'
    -> GetBulkPublishDetails
getBulkPublishDetails pIdentityPoolId_ =
  GetBulkPublishDetails' {_gbpdIdentityPoolId = pIdentityPoolId_}


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
gbpdIdentityPoolId :: Lens' GetBulkPublishDetails Text
gbpdIdentityPoolId = lens _gbpdIdentityPoolId (\ s a -> s{_gbpdIdentityPoolId = a})

instance AWSRequest GetBulkPublishDetails where
        type Rs GetBulkPublishDetails =
             GetBulkPublishDetailsResponse
        request = postJSON cognitoSync
        response
          = receiveJSON
              (\ s h x ->
                 GetBulkPublishDetailsResponse' <$>
                   (x .?> "BulkPublishStartTime") <*>
                     (x .?> "IdentityPoolId")
                     <*> (x .?> "BulkPublishCompleteTime")
                     <*> (x .?> "FailureMessage")
                     <*> (x .?> "BulkPublishStatus")
                     <*> (pure (fromEnum s)))

instance Hashable GetBulkPublishDetails where

instance NFData GetBulkPublishDetails where

instance ToHeaders GetBulkPublishDetails where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetBulkPublishDetails where
        toJSON = const (Object mempty)

instance ToPath GetBulkPublishDetails where
        toPath GetBulkPublishDetails'{..}
          = mconcat
              ["/identitypools/", toBS _gbpdIdentityPoolId,
               "/getBulkPublishDetails"]

instance ToQuery GetBulkPublishDetails where
        toQuery = const mempty

-- | The output for the GetBulkPublishDetails operation.
--
-- /See:/ 'getBulkPublishDetailsResponse' smart constructor.
data GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse'
  { _gbpdrsBulkPublishStartTime    :: !(Maybe POSIX)
  , _gbpdrsIdentityPoolId          :: !(Maybe Text)
  , _gbpdrsBulkPublishCompleteTime :: !(Maybe POSIX)
  , _gbpdrsFailureMessage          :: !(Maybe Text)
  , _gbpdrsBulkPublishStatus       :: !(Maybe BulkPublishStatus)
  , _gbpdrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetBulkPublishDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpdrsBulkPublishStartTime' - The date/time at which the last bulk publish was initiated.
--
-- * 'gbpdrsIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- * 'gbpdrsBulkPublishCompleteTime' - If BulkPublishStatus is SUCCEEDED, the time the last bulk publish operation completed.
--
-- * 'gbpdrsFailureMessage' - If BulkPublishStatus is FAILED this field will contain the error message that caused the bulk publish to fail.
--
-- * 'gbpdrsBulkPublishStatus' - Status of the last bulk publish operation, valid values are: NOT_STARTED - No bulk publish has been requested for this identity pool IN_PROGRESS - Data is being published to the configured stream SUCCEEDED - All data for the identity pool has been published to the configured stream FAILED - Some portion of the data has failed to publish, check FailureMessage for the cause.
--
-- * 'gbpdrsResponseStatus' - -- | The response status code.
getBulkPublishDetailsResponse
    :: Int -- ^ 'gbpdrsResponseStatus'
    -> GetBulkPublishDetailsResponse
getBulkPublishDetailsResponse pResponseStatus_ =
  GetBulkPublishDetailsResponse'
    { _gbpdrsBulkPublishStartTime = Nothing
    , _gbpdrsIdentityPoolId = Nothing
    , _gbpdrsBulkPublishCompleteTime = Nothing
    , _gbpdrsFailureMessage = Nothing
    , _gbpdrsBulkPublishStatus = Nothing
    , _gbpdrsResponseStatus = pResponseStatus_
    }


-- | The date/time at which the last bulk publish was initiated.
gbpdrsBulkPublishStartTime :: Lens' GetBulkPublishDetailsResponse (Maybe UTCTime)
gbpdrsBulkPublishStartTime = lens _gbpdrsBulkPublishStartTime (\ s a -> s{_gbpdrsBulkPublishStartTime = a}) . mapping _Time

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
gbpdrsIdentityPoolId :: Lens' GetBulkPublishDetailsResponse (Maybe Text)
gbpdrsIdentityPoolId = lens _gbpdrsIdentityPoolId (\ s a -> s{_gbpdrsIdentityPoolId = a})

-- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish operation completed.
gbpdrsBulkPublishCompleteTime :: Lens' GetBulkPublishDetailsResponse (Maybe UTCTime)
gbpdrsBulkPublishCompleteTime = lens _gbpdrsBulkPublishCompleteTime (\ s a -> s{_gbpdrsBulkPublishCompleteTime = a}) . mapping _Time

-- | If BulkPublishStatus is FAILED this field will contain the error message that caused the bulk publish to fail.
gbpdrsFailureMessage :: Lens' GetBulkPublishDetailsResponse (Maybe Text)
gbpdrsFailureMessage = lens _gbpdrsFailureMessage (\ s a -> s{_gbpdrsFailureMessage = a})

-- | Status of the last bulk publish operation, valid values are: NOT_STARTED - No bulk publish has been requested for this identity pool IN_PROGRESS - Data is being published to the configured stream SUCCEEDED - All data for the identity pool has been published to the configured stream FAILED - Some portion of the data has failed to publish, check FailureMessage for the cause.
gbpdrsBulkPublishStatus :: Lens' GetBulkPublishDetailsResponse (Maybe BulkPublishStatus)
gbpdrsBulkPublishStatus = lens _gbpdrsBulkPublishStatus (\ s a -> s{_gbpdrsBulkPublishStatus = a})

-- | -- | The response status code.
gbpdrsResponseStatus :: Lens' GetBulkPublishDetailsResponse Int
gbpdrsResponseStatus = lens _gbpdrsResponseStatus (\ s a -> s{_gbpdrsResponseStatus = a})

instance NFData GetBulkPublishDetailsResponse where

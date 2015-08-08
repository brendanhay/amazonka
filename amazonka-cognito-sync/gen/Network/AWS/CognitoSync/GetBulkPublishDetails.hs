{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetBulkPublishDetails
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the status of the last BulkPublish operation for an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
--
-- /See:/ <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_GetBulkPublishDetails.html AWS API Reference> for GetBulkPublishDetails.
module Network.AWS.CognitoSync.GetBulkPublishDetails
    (
    -- * Creating a Request
      GetBulkPublishDetails
    , getBulkPublishDetails
    -- * Request Lenses
    , gbpdIdentityPoolId

    -- * Destructuring the Response
    , GetBulkPublishDetailsResponse
    , getBulkPublishDetailsResponse
    -- * Response Lenses
    , gbpdrsBulkPublishStartTime
    , gbpdrsIdentityPoolId
    , gbpdrsBulkPublishCompleteTime
    , gbpdrsFailureMessage
    , gbpdrsBulkPublishStatus
    , gbpdrsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the GetBulkPublishDetails operation.
--
-- /See:/ 'getBulkPublishDetails' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpdIdentityPoolId'
newtype GetBulkPublishDetails = GetBulkPublishDetails'
    { _gbpdIdentityPoolId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBulkPublishDetails' smart constructor.
getBulkPublishDetails :: Text -> GetBulkPublishDetails
getBulkPublishDetails pIdentityPoolId_ =
    GetBulkPublishDetails'
    { _gbpdIdentityPoolId = pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
gbpdIdentityPoolId :: Lens' GetBulkPublishDetails Text
gbpdIdentityPoolId = lens _gbpdIdentityPoolId (\ s a -> s{_gbpdIdentityPoolId = a});

instance AWSRequest GetBulkPublishDetails where
        type Sv GetBulkPublishDetails = CognitoSync
        type Rs GetBulkPublishDetails =
             GetBulkPublishDetailsResponse
        request = postJSON
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpdrsBulkPublishStartTime'
--
-- * 'gbpdrsIdentityPoolId'
--
-- * 'gbpdrsBulkPublishCompleteTime'
--
-- * 'gbpdrsFailureMessage'
--
-- * 'gbpdrsBulkPublishStatus'
--
-- * 'gbpdrsStatus'
data GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse'
    { _gbpdrsBulkPublishStartTime    :: !(Maybe POSIX)
    , _gbpdrsIdentityPoolId          :: !(Maybe Text)
    , _gbpdrsBulkPublishCompleteTime :: !(Maybe POSIX)
    , _gbpdrsFailureMessage          :: !(Maybe Text)
    , _gbpdrsBulkPublishStatus       :: !(Maybe BulkPublishStatus)
    , _gbpdrsStatus                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBulkPublishDetailsResponse' smart constructor.
getBulkPublishDetailsResponse :: Int -> GetBulkPublishDetailsResponse
getBulkPublishDetailsResponse pStatus_ =
    GetBulkPublishDetailsResponse'
    { _gbpdrsBulkPublishStartTime = Nothing
    , _gbpdrsIdentityPoolId = Nothing
    , _gbpdrsBulkPublishCompleteTime = Nothing
    , _gbpdrsFailureMessage = Nothing
    , _gbpdrsBulkPublishStatus = Nothing
    , _gbpdrsStatus = pStatus_
    }

-- | The date\/time at which the last bulk publish was initiated.
gbpdrsBulkPublishStartTime :: Lens' GetBulkPublishDetailsResponse (Maybe UTCTime)
gbpdrsBulkPublishStartTime = lens _gbpdrsBulkPublishStartTime (\ s a -> s{_gbpdrsBulkPublishStartTime = a}) . mapping _Time;

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
gbpdrsIdentityPoolId :: Lens' GetBulkPublishDetailsResponse (Maybe Text)
gbpdrsIdentityPoolId = lens _gbpdrsIdentityPoolId (\ s a -> s{_gbpdrsIdentityPoolId = a});

-- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish
-- operation completed.
gbpdrsBulkPublishCompleteTime :: Lens' GetBulkPublishDetailsResponse (Maybe UTCTime)
gbpdrsBulkPublishCompleteTime = lens _gbpdrsBulkPublishCompleteTime (\ s a -> s{_gbpdrsBulkPublishCompleteTime = a}) . mapping _Time;

-- | If BulkPublishStatus is FAILED this field will contain the error message
-- that caused the bulk publish to fail.
gbpdrsFailureMessage :: Lens' GetBulkPublishDetailsResponse (Maybe Text)
gbpdrsFailureMessage = lens _gbpdrsFailureMessage (\ s a -> s{_gbpdrsFailureMessage = a});

-- | Status of the last bulk publish operation, valid values are:
--
-- NOT_STARTED - No bulk publish has been requested for this identity pool
--
-- IN_PROGRESS - Data is being published to the configured stream
--
-- SUCCEEDED - All data for the identity pool has been published to the
-- configured stream
--
-- FAILED - Some portion of the data has failed to publish, check
-- FailureMessage for the cause.
gbpdrsBulkPublishStatus :: Lens' GetBulkPublishDetailsResponse (Maybe BulkPublishStatus)
gbpdrsBulkPublishStatus = lens _gbpdrsBulkPublishStatus (\ s a -> s{_gbpdrsBulkPublishStatus = a});

-- | Undocumented member.
gbpdrsStatus :: Lens' GetBulkPublishDetailsResponse Int
gbpdrsStatus = lens _gbpdrsStatus (\ s a -> s{_gbpdrsStatus = a});

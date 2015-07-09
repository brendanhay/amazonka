{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetBulkPublishDetails
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Get the status of the last BulkPublish operation for an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_GetBulkPublishDetails.html>
module Network.AWS.CognitoSync.GetBulkPublishDetails
    (
    -- * Request
      GetBulkPublishDetails
    -- ** Request constructor
    , getBulkPublishDetails
    -- ** Request lenses
    , gbpdIdentityPoolId

    -- * Response
    , GetBulkPublishDetailsResponse
    -- ** Response constructor
    , getBulkPublishDetailsResponse
    -- ** Response lenses
    , gbpdrBulkPublishStartTime
    , gbpdrIdentityPoolId
    , gbpdrBulkPublishCompleteTime
    , gbpdrFailureMessage
    , gbpdrBulkPublishStatus
    , gbpdrStatus
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
getBulkPublishDetails pIdentityPoolId =
    GetBulkPublishDetails'
    { _gbpdIdentityPoolId = pIdentityPoolId
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
              ["/identitypools/", toText _gbpdIdentityPoolId,
               "/getBulkPublishDetails"]

instance ToQuery GetBulkPublishDetails where
        toQuery = const mempty

-- | The output for the GetBulkPublishDetails operation.
--
-- /See:/ 'getBulkPublishDetailsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbpdrBulkPublishStartTime'
--
-- * 'gbpdrIdentityPoolId'
--
-- * 'gbpdrBulkPublishCompleteTime'
--
-- * 'gbpdrFailureMessage'
--
-- * 'gbpdrBulkPublishStatus'
--
-- * 'gbpdrStatus'
data GetBulkPublishDetailsResponse = GetBulkPublishDetailsResponse'
    { _gbpdrBulkPublishStartTime    :: !(Maybe POSIX)
    , _gbpdrIdentityPoolId          :: !(Maybe Text)
    , _gbpdrBulkPublishCompleteTime :: !(Maybe POSIX)
    , _gbpdrFailureMessage          :: !(Maybe Text)
    , _gbpdrBulkPublishStatus       :: !(Maybe BulkPublishStatus)
    , _gbpdrStatus                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBulkPublishDetailsResponse' smart constructor.
getBulkPublishDetailsResponse :: Int -> GetBulkPublishDetailsResponse
getBulkPublishDetailsResponse pStatus =
    GetBulkPublishDetailsResponse'
    { _gbpdrBulkPublishStartTime = Nothing
    , _gbpdrIdentityPoolId = Nothing
    , _gbpdrBulkPublishCompleteTime = Nothing
    , _gbpdrFailureMessage = Nothing
    , _gbpdrBulkPublishStatus = Nothing
    , _gbpdrStatus = pStatus
    }

-- | The date\/time at which the last bulk publish was initiated.
gbpdrBulkPublishStartTime :: Lens' GetBulkPublishDetailsResponse (Maybe UTCTime)
gbpdrBulkPublishStartTime = lens _gbpdrBulkPublishStartTime (\ s a -> s{_gbpdrBulkPublishStartTime = a}) . mapping _Time;

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. GUID generation is unique within a region.
gbpdrIdentityPoolId :: Lens' GetBulkPublishDetailsResponse (Maybe Text)
gbpdrIdentityPoolId = lens _gbpdrIdentityPoolId (\ s a -> s{_gbpdrIdentityPoolId = a});

-- | If BulkPublishStatus is SUCCEEDED, the time the last bulk publish
-- operation completed.
gbpdrBulkPublishCompleteTime :: Lens' GetBulkPublishDetailsResponse (Maybe UTCTime)
gbpdrBulkPublishCompleteTime = lens _gbpdrBulkPublishCompleteTime (\ s a -> s{_gbpdrBulkPublishCompleteTime = a}) . mapping _Time;

-- | If BulkPublishStatus is FAILED this field will contain the error message
-- that caused the bulk publish to fail.
gbpdrFailureMessage :: Lens' GetBulkPublishDetailsResponse (Maybe Text)
gbpdrFailureMessage = lens _gbpdrFailureMessage (\ s a -> s{_gbpdrFailureMessage = a});

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
gbpdrBulkPublishStatus :: Lens' GetBulkPublishDetailsResponse (Maybe BulkPublishStatus)
gbpdrBulkPublishStatus = lens _gbpdrBulkPublishStatus (\ s a -> s{_gbpdrBulkPublishStatus = a});

-- | FIXME: Undocumented member.
gbpdrStatus :: Lens' GetBulkPublishDetailsResponse Int
gbpdrStatus = lens _gbpdrStatus (\ s a -> s{_gbpdrStatus = a});

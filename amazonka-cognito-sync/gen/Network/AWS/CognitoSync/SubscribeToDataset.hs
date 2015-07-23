{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.SubscribeToDataset
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Subscribes to receive notifications when a dataset is modified by
-- another device.
--
-- This API can only be called with temporary credentials provided by
-- Cognito Identity. You cannot call this API with developer credentials.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_SubscribeToDataset.html>
module Network.AWS.CognitoSync.SubscribeToDataset
    (
    -- * Request
      SubscribeToDataset
    -- ** Request constructor
    , subscribeToDataset
    -- ** Request lenses
    , stdrqIdentityPoolId
    , stdrqIdentityId
    , stdrqDatasetName
    , stdrqDeviceId

    -- * Response
    , SubscribeToDatasetResponse
    -- ** Response constructor
    , subscribeToDatasetResponse
    -- ** Response lenses
    , stdrsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to SubscribeToDatasetRequest.
--
-- /See:/ 'subscribeToDataset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stdrqIdentityPoolId'
--
-- * 'stdrqIdentityId'
--
-- * 'stdrqDatasetName'
--
-- * 'stdrqDeviceId'
data SubscribeToDataset = SubscribeToDataset'
    { _stdrqIdentityPoolId :: !Text
    , _stdrqIdentityId     :: !Text
    , _stdrqDatasetName    :: !Text
    , _stdrqDeviceId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubscribeToDataset' smart constructor.
subscribeToDataset :: Text -> Text -> Text -> Text -> SubscribeToDataset
subscribeToDataset pIdentityPoolId_ pIdentityId_ pDatasetName_ pDeviceId_ =
    SubscribeToDataset'
    { _stdrqIdentityPoolId = pIdentityPoolId_
    , _stdrqIdentityId = pIdentityId_
    , _stdrqDatasetName = pDatasetName_
    , _stdrqDeviceId = pDeviceId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which the identity belongs.
stdrqIdentityPoolId :: Lens' SubscribeToDataset Text
stdrqIdentityPoolId = lens _stdrqIdentityPoolId (\ s a -> s{_stdrqIdentityPoolId = a});

-- | Unique ID for this identity.
stdrqIdentityId :: Lens' SubscribeToDataset Text
stdrqIdentityId = lens _stdrqIdentityId (\ s a -> s{_stdrqIdentityId = a});

-- | The name of the dataset to subcribe to.
stdrqDatasetName :: Lens' SubscribeToDataset Text
stdrqDatasetName = lens _stdrqDatasetName (\ s a -> s{_stdrqDatasetName = a});

-- | The unique ID generated for this device by Cognito.
stdrqDeviceId :: Lens' SubscribeToDataset Text
stdrqDeviceId = lens _stdrqDeviceId (\ s a -> s{_stdrqDeviceId = a});

instance AWSRequest SubscribeToDataset where
        type Sv SubscribeToDataset = CognitoSync
        type Rs SubscribeToDataset =
             SubscribeToDatasetResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 SubscribeToDatasetResponse' <$> (pure (fromEnum s)))

instance ToHeaders SubscribeToDataset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SubscribeToDataset where
        toJSON = const (Object mempty)

instance ToPath SubscribeToDataset where
        toPath SubscribeToDataset'{..}
          = mconcat
              ["/identitypools/", toText _stdrqIdentityPoolId,
               "/identities/", toText _stdrqIdentityId,
               "/datasets/", toText _stdrqDatasetName,
               "/subscriptions/", toText _stdrqDeviceId]

instance ToQuery SubscribeToDataset where
        toQuery = const mempty

-- | Response to a SubscribeToDataset request.
--
-- /See:/ 'subscribeToDatasetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stdrsStatus'
newtype SubscribeToDatasetResponse = SubscribeToDatasetResponse'
    { _stdrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubscribeToDatasetResponse' smart constructor.
subscribeToDatasetResponse :: Int -> SubscribeToDatasetResponse
subscribeToDatasetResponse pStatus_ =
    SubscribeToDatasetResponse'
    { _stdrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
stdrsStatus :: Lens' SubscribeToDatasetResponse Int
stdrsStatus = lens _stdrsStatus (\ s a -> s{_stdrsStatus = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CognitoSync.SubscribeToDataset
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Subscribes to receive notifications when a dataset is modified by
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
    , stdIdentityPoolId
    , stdIdentityId
    , stdDatasetName
    , stdDeviceId

    -- * Response
    , SubscribeToDatasetResponse
    -- ** Response constructor
    , subscribeToDatasetResponse
    -- ** Response lenses
    , stdrStatus
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
-- * 'stdIdentityPoolId'
--
-- * 'stdIdentityId'
--
-- * 'stdDatasetName'
--
-- * 'stdDeviceId'
data SubscribeToDataset = SubscribeToDataset'
    { _stdIdentityPoolId :: !Text
    , _stdIdentityId     :: !Text
    , _stdDatasetName    :: !Text
    , _stdDeviceId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubscribeToDataset' smart constructor.
subscribeToDataset :: Text -> Text -> Text -> Text -> SubscribeToDataset
subscribeToDataset pIdentityPoolId pIdentityId pDatasetName pDeviceId =
    SubscribeToDataset'
    { _stdIdentityPoolId = pIdentityPoolId
    , _stdIdentityId = pIdentityId
    , _stdDatasetName = pDatasetName
    , _stdDeviceId = pDeviceId
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which the identity belongs.
stdIdentityPoolId :: Lens' SubscribeToDataset Text
stdIdentityPoolId = lens _stdIdentityPoolId (\ s a -> s{_stdIdentityPoolId = a});

-- | Unique ID for this identity.
stdIdentityId :: Lens' SubscribeToDataset Text
stdIdentityId = lens _stdIdentityId (\ s a -> s{_stdIdentityId = a});

-- | The name of the dataset to subcribe to.
stdDatasetName :: Lens' SubscribeToDataset Text
stdDatasetName = lens _stdDatasetName (\ s a -> s{_stdDatasetName = a});

-- | The unique ID generated for this device by Cognito.
stdDeviceId :: Lens' SubscribeToDataset Text
stdDeviceId = lens _stdDeviceId (\ s a -> s{_stdDeviceId = a});

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
              ["/identitypools/", toText _stdIdentityPoolId,
               "/identities/", toText _stdIdentityId, "/datasets/",
               toText _stdDatasetName, "/subscriptions/",
               toText _stdDeviceId]

instance ToQuery SubscribeToDataset where
        toQuery = const mempty

-- | Response to a SubscribeToDataset request.
--
-- /See:/ 'subscribeToDatasetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stdrStatus'
newtype SubscribeToDatasetResponse = SubscribeToDatasetResponse'
    { _stdrStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubscribeToDatasetResponse' smart constructor.
subscribeToDatasetResponse :: Int -> SubscribeToDatasetResponse
subscribeToDatasetResponse pStatus =
    SubscribeToDatasetResponse'
    { _stdrStatus = pStatus
    }

-- | FIXME: Undocumented member.
stdrStatus :: Lens' SubscribeToDatasetResponse Int
stdrStatus = lens _stdrStatus (\ s a -> s{_stdrStatus = a});

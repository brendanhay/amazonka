{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CognitoSync.UnsubscribeFromDataset
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Unsubscribes from receiving notifications when a dataset is modified by
-- another device.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_UnsubscribeFromDataset.html>
module Network.AWS.CognitoSync.UnsubscribeFromDataset
    (
    -- * Request
      UnsubscribeFromDataset
    -- ** Request constructor
    , unsubscribeFromDataset
    -- ** Request lenses
    , ufdIdentityPoolId
    , ufdIdentityId
    , ufdDatasetName
    , ufdDeviceId

    -- * Response
    , UnsubscribeFromDatasetResponse
    -- ** Response constructor
    , unsubscribeFromDatasetResponse
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unsubscribeFromDataset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufdIdentityPoolId'
--
-- * 'ufdIdentityId'
--
-- * 'ufdDatasetName'
--
-- * 'ufdDeviceId'
data UnsubscribeFromDataset = UnsubscribeFromDataset'{_ufdIdentityPoolId :: Text, _ufdIdentityId :: Text, _ufdDatasetName :: Text, _ufdDeviceId :: Text} deriving (Eq, Read, Show)

-- | 'UnsubscribeFromDataset' smart constructor.
unsubscribeFromDataset :: Text -> Text -> Text -> Text -> UnsubscribeFromDataset
unsubscribeFromDataset pIdentityPoolId pIdentityId pDatasetName pDeviceId = UnsubscribeFromDataset'{_ufdIdentityPoolId = pIdentityPoolId, _ufdIdentityId = pIdentityId, _ufdDatasetName = pDatasetName, _ufdDeviceId = pDeviceId};

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which this identity belongs.
ufdIdentityPoolId :: Lens' UnsubscribeFromDataset Text
ufdIdentityPoolId = lens _ufdIdentityPoolId (\ s a -> s{_ufdIdentityPoolId = a});

-- | Unique ID for this identity.
ufdIdentityId :: Lens' UnsubscribeFromDataset Text
ufdIdentityId = lens _ufdIdentityId (\ s a -> s{_ufdIdentityId = a});

-- | The name of the dataset from which to unsubcribe.
ufdDatasetName :: Lens' UnsubscribeFromDataset Text
ufdDatasetName = lens _ufdDatasetName (\ s a -> s{_ufdDatasetName = a});

-- | The unique ID generated for this device by Cognito.
ufdDeviceId :: Lens' UnsubscribeFromDataset Text
ufdDeviceId = lens _ufdDeviceId (\ s a -> s{_ufdDeviceId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest UnsubscribeFromDataset where
        type Sv UnsubscribeFromDataset = CognitoSync
        type Rs UnsubscribeFromDataset =
             UnsubscribeFromDatasetResponse
        request = delete
        response
          = receiveNull UnsubscribeFromDatasetResponse'

instance ToHeaders UnsubscribeFromDataset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath UnsubscribeFromDataset where
        toPath UnsubscribeFromDataset'{..}
          = mconcat
              ["/identitypools/", toText _ufdIdentityPoolId,
               "/identities/", toText _ufdIdentityId, "/datasets/",
               toText _ufdDatasetName, "/subscriptions/",
               toText _ufdDeviceId]

instance ToQuery UnsubscribeFromDataset where
        toQuery = const mempty

-- | /See:/ 'unsubscribeFromDatasetResponse' smart constructor.
data UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse' deriving (Eq, Read, Show)

-- | 'UnsubscribeFromDatasetResponse' smart constructor.
unsubscribeFromDatasetResponse :: UnsubscribeFromDatasetResponse
unsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse';

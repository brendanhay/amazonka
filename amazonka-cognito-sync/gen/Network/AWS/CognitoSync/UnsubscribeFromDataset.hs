{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.UnsubscribeFromDataset
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Unsubscribes from receiving notifications when a dataset is modified by
-- another device.
--
-- This API can only be called with temporary credentials provided by
-- Cognito Identity. You cannot call this API with developer credentials.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_UnsubscribeFromDataset.html>
module Network.AWS.CognitoSync.UnsubscribeFromDataset
    (
    -- * Request
      UnsubscribeFromDataset
    -- ** Request constructor
    , unsubscribeFromDataset
    -- ** Request lenses
    , ufdrqIdentityPoolId
    , ufdrqIdentityId
    , ufdrqDatasetName
    , ufdrqDeviceId

    -- * Response
    , UnsubscribeFromDatasetResponse
    -- ** Response constructor
    , unsubscribeFromDatasetResponse
    -- ** Response lenses
    , ufdrsStatus
    ) where

import           Network.AWS.CognitoSync.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | A request to UnsubscribeFromDataset.
--
-- /See:/ 'unsubscribeFromDataset' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufdrqIdentityPoolId'
--
-- * 'ufdrqIdentityId'
--
-- * 'ufdrqDatasetName'
--
-- * 'ufdrqDeviceId'
data UnsubscribeFromDataset = UnsubscribeFromDataset'
    { _ufdrqIdentityPoolId :: !Text
    , _ufdrqIdentityId     :: !Text
    , _ufdrqDatasetName    :: !Text
    , _ufdrqDeviceId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnsubscribeFromDataset' smart constructor.
unsubscribeFromDataset :: Text -> Text -> Text -> Text -> UnsubscribeFromDataset
unsubscribeFromDataset pIdentityPoolId pIdentityId pDatasetName pDeviceId =
    UnsubscribeFromDataset'
    { _ufdrqIdentityPoolId = pIdentityPoolId
    , _ufdrqIdentityId = pIdentityId
    , _ufdrqDatasetName = pDatasetName
    , _ufdrqDeviceId = pDeviceId
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which this identity belongs.
ufdrqIdentityPoolId :: Lens' UnsubscribeFromDataset Text
ufdrqIdentityPoolId = lens _ufdrqIdentityPoolId (\ s a -> s{_ufdrqIdentityPoolId = a});

-- | Unique ID for this identity.
ufdrqIdentityId :: Lens' UnsubscribeFromDataset Text
ufdrqIdentityId = lens _ufdrqIdentityId (\ s a -> s{_ufdrqIdentityId = a});

-- | The name of the dataset from which to unsubcribe.
ufdrqDatasetName :: Lens' UnsubscribeFromDataset Text
ufdrqDatasetName = lens _ufdrqDatasetName (\ s a -> s{_ufdrqDatasetName = a});

-- | The unique ID generated for this device by Cognito.
ufdrqDeviceId :: Lens' UnsubscribeFromDataset Text
ufdrqDeviceId = lens _ufdrqDeviceId (\ s a -> s{_ufdrqDeviceId = a});

instance AWSRequest UnsubscribeFromDataset where
        type Sv UnsubscribeFromDataset = CognitoSync
        type Rs UnsubscribeFromDataset =
             UnsubscribeFromDatasetResponse
        request = delete
        response
          = receiveJSON
              (\ s h x ->
                 UnsubscribeFromDatasetResponse' <$>
                   (pure (fromEnum s)))

instance ToHeaders UnsubscribeFromDataset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath UnsubscribeFromDataset where
        toPath UnsubscribeFromDataset'{..}
          = mconcat
              ["/identitypools/", toText _ufdrqIdentityPoolId,
               "/identities/", toText _ufdrqIdentityId,
               "/datasets/", toText _ufdrqDatasetName,
               "/subscriptions/", toText _ufdrqDeviceId]

instance ToQuery UnsubscribeFromDataset where
        toQuery = const mempty

-- | Response to an UnsubscribeFromDataset request.
--
-- /See:/ 'unsubscribeFromDatasetResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufdrsStatus'
newtype UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse'
    { _ufdrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnsubscribeFromDatasetResponse' smart constructor.
unsubscribeFromDatasetResponse :: Int -> UnsubscribeFromDatasetResponse
unsubscribeFromDatasetResponse pStatus =
    UnsubscribeFromDatasetResponse'
    { _ufdrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ufdrsStatus :: Lens' UnsubscribeFromDatasetResponse Int
ufdrsStatus = lens _ufdrsStatus (\ s a -> s{_ufdrsStatus = a});

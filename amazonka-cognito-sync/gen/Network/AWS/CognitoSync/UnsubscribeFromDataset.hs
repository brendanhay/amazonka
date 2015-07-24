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
    , ufdIdentityPoolId
    , ufdIdentityId
    , ufdDatasetName
    , ufdDeviceId

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
-- * 'ufdIdentityPoolId'
--
-- * 'ufdIdentityId'
--
-- * 'ufdDatasetName'
--
-- * 'ufdDeviceId'
data UnsubscribeFromDataset = UnsubscribeFromDataset'
    { _ufdIdentityPoolId :: !Text
    , _ufdIdentityId     :: !Text
    , _ufdDatasetName    :: !Text
    , _ufdDeviceId       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UnsubscribeFromDataset' smart constructor.
unsubscribeFromDataset :: Text -> Text -> Text -> Text -> UnsubscribeFromDataset
unsubscribeFromDataset pIdentityPoolId_ pIdentityId_ pDatasetName_ pDeviceId_ =
    UnsubscribeFromDataset'
    { _ufdIdentityPoolId = pIdentityPoolId_
    , _ufdIdentityId = pIdentityId_
    , _ufdDatasetName = pDatasetName_
    , _ufdDeviceId = pDeviceId_
    }

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

instance AWSRequest UnsubscribeFromDataset where
        type Sv UnsubscribeFromDataset = CognitoSync
        type Rs UnsubscribeFromDataset =
             UnsubscribeFromDatasetResponse
        request = delete "UnsubscribeFromDataset"
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
              ["/identitypools/", toText _ufdIdentityPoolId,
               "/identities/", toText _ufdIdentityId, "/datasets/",
               toText _ufdDatasetName, "/subscriptions/",
               toText _ufdDeviceId]

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
unsubscribeFromDatasetResponse pStatus_ =
    UnsubscribeFromDatasetResponse'
    { _ufdrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
ufdrsStatus :: Lens' UnsubscribeFromDatasetResponse Int
ufdrsStatus = lens _ufdrsStatus (\ s a -> s{_ufdrsStatus = a});

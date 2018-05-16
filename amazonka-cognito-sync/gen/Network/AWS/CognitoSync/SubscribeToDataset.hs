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
-- Module      : Network.AWS.CognitoSync.SubscribeToDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Subscribes to receive notifications when a dataset is modified by another device.
--
--
-- This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.
--
module Network.AWS.CognitoSync.SubscribeToDataset
    (
    -- * Creating a Request
      subscribeToDataset
    , SubscribeToDataset
    -- * Request Lenses
    , stdIdentityPoolId
    , stdIdentityId
    , stdDatasetName
    , stdDeviceId

    -- * Destructuring the Response
    , subscribeToDatasetResponse
    , SubscribeToDatasetResponse
    -- * Response Lenses
    , stdrsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to SubscribeToDatasetRequest.
--
--
--
-- /See:/ 'subscribeToDataset' smart constructor.
data SubscribeToDataset = SubscribeToDataset'
  { _stdIdentityPoolId :: !Text
  , _stdIdentityId     :: !Text
  , _stdDatasetName    :: !Text
  , _stdDeviceId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribeToDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which the identity belongs.
--
-- * 'stdIdentityId' - Unique ID for this identity.
--
-- * 'stdDatasetName' - The name of the dataset to subcribe to.
--
-- * 'stdDeviceId' - The unique ID generated for this device by Cognito.
subscribeToDataset
    :: Text -- ^ 'stdIdentityPoolId'
    -> Text -- ^ 'stdIdentityId'
    -> Text -- ^ 'stdDatasetName'
    -> Text -- ^ 'stdDeviceId'
    -> SubscribeToDataset
subscribeToDataset pIdentityPoolId_ pIdentityId_ pDatasetName_ pDeviceId_ =
  SubscribeToDataset'
    { _stdIdentityPoolId = pIdentityPoolId_
    , _stdIdentityId = pIdentityId_
    , _stdDatasetName = pDatasetName_
    , _stdDeviceId = pDeviceId_
    }


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which the identity belongs.
stdIdentityPoolId :: Lens' SubscribeToDataset Text
stdIdentityPoolId = lens _stdIdentityPoolId (\ s a -> s{_stdIdentityPoolId = a})

-- | Unique ID for this identity.
stdIdentityId :: Lens' SubscribeToDataset Text
stdIdentityId = lens _stdIdentityId (\ s a -> s{_stdIdentityId = a})

-- | The name of the dataset to subcribe to.
stdDatasetName :: Lens' SubscribeToDataset Text
stdDatasetName = lens _stdDatasetName (\ s a -> s{_stdDatasetName = a})

-- | The unique ID generated for this device by Cognito.
stdDeviceId :: Lens' SubscribeToDataset Text
stdDeviceId = lens _stdDeviceId (\ s a -> s{_stdDeviceId = a})

instance AWSRequest SubscribeToDataset where
        type Rs SubscribeToDataset =
             SubscribeToDatasetResponse
        request = postJSON cognitoSync
        response
          = receiveEmpty
              (\ s h x ->
                 SubscribeToDatasetResponse' <$> (pure (fromEnum s)))

instance Hashable SubscribeToDataset where

instance NFData SubscribeToDataset where

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
              ["/identitypools/", toBS _stdIdentityPoolId,
               "/identities/", toBS _stdIdentityId, "/datasets/",
               toBS _stdDatasetName, "/subscriptions/",
               toBS _stdDeviceId]

instance ToQuery SubscribeToDataset where
        toQuery = const mempty

-- | Response to a SubscribeToDataset request.
--
--
--
-- /See:/ 'subscribeToDatasetResponse' smart constructor.
newtype SubscribeToDatasetResponse = SubscribeToDatasetResponse'
  { _stdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribeToDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stdrsResponseStatus' - -- | The response status code.
subscribeToDatasetResponse
    :: Int -- ^ 'stdrsResponseStatus'
    -> SubscribeToDatasetResponse
subscribeToDatasetResponse pResponseStatus_ =
  SubscribeToDatasetResponse' {_stdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
stdrsResponseStatus :: Lens' SubscribeToDatasetResponse Int
stdrsResponseStatus = lens _stdrsResponseStatus (\ s a -> s{_stdrsResponseStatus = a})

instance NFData SubscribeToDatasetResponse where

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
-- Module      : Network.AWS.CognitoSync.UnsubscribeFromDataset
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unsubscribes from receiving notifications when a dataset is modified by another device.
--
--
-- This API can only be called with temporary credentials provided by Cognito Identity. You cannot call this API with developer credentials.
--
module Network.AWS.CognitoSync.UnsubscribeFromDataset
    (
    -- * Creating a Request
      unsubscribeFromDataset
    , UnsubscribeFromDataset
    -- * Request Lenses
    , ufdIdentityPoolId
    , ufdIdentityId
    , ufdDatasetName
    , ufdDeviceId

    -- * Destructuring the Response
    , unsubscribeFromDatasetResponse
    , UnsubscribeFromDatasetResponse
    -- * Response Lenses
    , ufdrsResponseStatus
    ) where

import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to UnsubscribeFromDataset.
--
--
--
-- /See:/ 'unsubscribeFromDataset' smart constructor.
data UnsubscribeFromDataset = UnsubscribeFromDataset'
  { _ufdIdentityPoolId :: !Text
  , _ufdIdentityId     :: !Text
  , _ufdDatasetName    :: !Text
  , _ufdDeviceId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsubscribeFromDataset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufdIdentityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which this identity belongs.
--
-- * 'ufdIdentityId' - Unique ID for this identity.
--
-- * 'ufdDatasetName' - The name of the dataset from which to unsubcribe.
--
-- * 'ufdDeviceId' - The unique ID generated for this device by Cognito.
unsubscribeFromDataset
    :: Text -- ^ 'ufdIdentityPoolId'
    -> Text -- ^ 'ufdIdentityId'
    -> Text -- ^ 'ufdDatasetName'
    -> Text -- ^ 'ufdDeviceId'
    -> UnsubscribeFromDataset
unsubscribeFromDataset pIdentityPoolId_ pIdentityId_ pDatasetName_ pDeviceId_ =
  UnsubscribeFromDataset'
    { _ufdIdentityPoolId = pIdentityPoolId_
    , _ufdIdentityId = pIdentityId_
    , _ufdDatasetName = pDatasetName_
    , _ufdDeviceId = pDeviceId_
    }


-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. The ID of the pool to which this identity belongs.
ufdIdentityPoolId :: Lens' UnsubscribeFromDataset Text
ufdIdentityPoolId = lens _ufdIdentityPoolId (\ s a -> s{_ufdIdentityPoolId = a})

-- | Unique ID for this identity.
ufdIdentityId :: Lens' UnsubscribeFromDataset Text
ufdIdentityId = lens _ufdIdentityId (\ s a -> s{_ufdIdentityId = a})

-- | The name of the dataset from which to unsubcribe.
ufdDatasetName :: Lens' UnsubscribeFromDataset Text
ufdDatasetName = lens _ufdDatasetName (\ s a -> s{_ufdDatasetName = a})

-- | The unique ID generated for this device by Cognito.
ufdDeviceId :: Lens' UnsubscribeFromDataset Text
ufdDeviceId = lens _ufdDeviceId (\ s a -> s{_ufdDeviceId = a})

instance AWSRequest UnsubscribeFromDataset where
        type Rs UnsubscribeFromDataset =
             UnsubscribeFromDatasetResponse
        request = delete cognitoSync
        response
          = receiveEmpty
              (\ s h x ->
                 UnsubscribeFromDatasetResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UnsubscribeFromDataset where

instance NFData UnsubscribeFromDataset where

instance ToHeaders UnsubscribeFromDataset where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath UnsubscribeFromDataset where
        toPath UnsubscribeFromDataset'{..}
          = mconcat
              ["/identitypools/", toBS _ufdIdentityPoolId,
               "/identities/", toBS _ufdIdentityId, "/datasets/",
               toBS _ufdDatasetName, "/subscriptions/",
               toBS _ufdDeviceId]

instance ToQuery UnsubscribeFromDataset where
        toQuery = const mempty

-- | Response to an UnsubscribeFromDataset request.
--
--
--
-- /See:/ 'unsubscribeFromDatasetResponse' smart constructor.
newtype UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse'
  { _ufdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsubscribeFromDatasetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufdrsResponseStatus' - -- | The response status code.
unsubscribeFromDatasetResponse
    :: Int -- ^ 'ufdrsResponseStatus'
    -> UnsubscribeFromDatasetResponse
unsubscribeFromDatasetResponse pResponseStatus_ =
  UnsubscribeFromDatasetResponse' {_ufdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ufdrsResponseStatus :: Lens' UnsubscribeFromDatasetResponse Int
ufdrsResponseStatus = lens _ufdrsResponseStatus (\ s a -> s{_ufdrsResponseStatus = a})

instance NFData UnsubscribeFromDatasetResponse where

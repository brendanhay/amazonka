{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CognitoSync.UnsubscribeFromDataset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Unsubscribe from receiving notifications when a dataset is modified by
-- another device.
module Network.AWS.CognitoSync.UnsubscribeFromDataset
    (
    -- * Request
      UnsubscribeFromDataset
    -- ** Request constructor
    , unsubscribeFromDataset
    -- ** Request lenses
    , ufdDatasetName
    , ufdDeviceId
    , ufdIdentityId
    , ufdIdentityPoolId

    -- * Response
    , UnsubscribeFromDatasetResponse
    -- ** Response constructor
    , unsubscribeFromDatasetResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CognitoSync.Types

data UnsubscribeFromDataset = UnsubscribeFromDataset
    { _ufdDatasetName    :: Text
    , _ufdDeviceId       :: Text
    , _ufdIdentityId     :: Text
    , _ufdIdentityPoolId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UnsubscribeFromDataset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ufdDatasetName' @::@ 'Text'
--
-- * 'ufdDeviceId' @::@ 'Text'
--
-- * 'ufdIdentityId' @::@ 'Text'
--
-- * 'ufdIdentityPoolId' @::@ 'Text'
--
unsubscribeFromDataset :: Text -- ^ 'ufdIdentityPoolId'
                       -> Text -- ^ 'ufdIdentityId'
                       -> Text -- ^ 'ufdDatasetName'
                       -> Text -- ^ 'ufdDeviceId'
                       -> UnsubscribeFromDataset
unsubscribeFromDataset p1 p2 p3 p4 = UnsubscribeFromDataset
    { _ufdIdentityPoolId = p1
    , _ufdIdentityId     = p2
    , _ufdDatasetName    = p3
    , _ufdDeviceId       = p4
    }

-- | The name of the dataset from which to unsubcribe.
ufdDatasetName :: Lens' UnsubscribeFromDataset Text
ufdDatasetName = lens _ufdDatasetName (\s a -> s { _ufdDatasetName = a })

-- | The unique ID generated for this device by Cognito.
ufdDeviceId :: Lens' UnsubscribeFromDataset Text
ufdDeviceId = lens _ufdDeviceId (\s a -> s { _ufdDeviceId = a })

-- | Unique ID for this identity.
ufdIdentityId :: Lens' UnsubscribeFromDataset Text
ufdIdentityId = lens _ufdIdentityId (\s a -> s { _ufdIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which this identity belongs.
ufdIdentityPoolId :: Lens' UnsubscribeFromDataset Text
ufdIdentityPoolId =
    lens _ufdIdentityPoolId (\s a -> s { _ufdIdentityPoolId = a })

instance ToPath UnsubscribeFromDataset where
    toPath UnsubscribeFromDataset{..} = mconcat
        [ "/identitypools/"
        , toText _ufdIdentityPoolId
        , "/identities/"
        , toText _ufdIdentityId
        , "/datasets/"
        , toText _ufdDatasetName
        , "/subscriptions/"
        , toText _ufdDeviceId
        ]

instance ToQuery UnsubscribeFromDataset where
    toQuery = const mempty

instance ToHeaders UnsubscribeFromDataset

data UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UnsubscribeFromDatasetResponse' constructor.
unsubscribeFromDatasetResponse :: UnsubscribeFromDatasetResponse
unsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse

instance AWSRequest UnsubscribeFromDataset where
    type Sv UnsubscribeFromDataset = CognitoSync
    type Rs UnsubscribeFromDataset = UnsubscribeFromDatasetResponse

    request  = delete
    response = nullaryResponse UnsubscribeFromDatasetResponse

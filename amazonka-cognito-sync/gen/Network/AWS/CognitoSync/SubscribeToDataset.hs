{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.SubscribeToDataset
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Subscribes to receive notifications when a dataset is modified by another
-- device.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_SubscribeToDataset.html>
module Network.AWS.CognitoSync.SubscribeToDataset
    (
    -- * Request
      SubscribeToDataset
    -- ** Request constructor
    , subscribeToDataset
    -- ** Request lenses
    , stdDatasetName
    , stdDeviceId
    , stdIdentityId
    , stdIdentityPoolId

    -- * Response
    , SubscribeToDatasetResponse
    -- ** Response constructor
    , subscribeToDatasetResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

data SubscribeToDataset = SubscribeToDataset
    { _stdDatasetName    :: Text
    , _stdDeviceId       :: Text
    , _stdIdentityId     :: Text
    , _stdIdentityPoolId :: Text
    } deriving (Eq, Ord, Show)

-- | 'SubscribeToDataset' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stdDatasetName' @::@ 'Text'
--
-- * 'stdDeviceId' @::@ 'Text'
--
-- * 'stdIdentityId' @::@ 'Text'
--
-- * 'stdIdentityPoolId' @::@ 'Text'
--
subscribeToDataset :: Text -- ^ 'stdIdentityPoolId'
                   -> Text -- ^ 'stdIdentityId'
                   -> Text -- ^ 'stdDatasetName'
                   -> Text -- ^ 'stdDeviceId'
                   -> SubscribeToDataset
subscribeToDataset p1 p2 p3 p4 = SubscribeToDataset
    { _stdIdentityPoolId = p1
    , _stdIdentityId     = p2
    , _stdDatasetName    = p3
    , _stdDeviceId       = p4
    }

-- | The name of the dataset to subcribe to.
stdDatasetName :: Lens' SubscribeToDataset Text
stdDatasetName = lens _stdDatasetName (\s a -> s { _stdDatasetName = a })

-- | The unique ID generated for this device by Cognito.
stdDeviceId :: Lens' SubscribeToDataset Text
stdDeviceId = lens _stdDeviceId (\s a -> s { _stdDeviceId = a })

-- | Unique ID for this identity.
stdIdentityId :: Lens' SubscribeToDataset Text
stdIdentityId = lens _stdIdentityId (\s a -> s { _stdIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which the identity belongs.
stdIdentityPoolId :: Lens' SubscribeToDataset Text
stdIdentityPoolId =
    lens _stdIdentityPoolId (\s a -> s { _stdIdentityPoolId = a })

data SubscribeToDatasetResponse = SubscribeToDatasetResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SubscribeToDatasetResponse' constructor.
subscribeToDatasetResponse :: SubscribeToDatasetResponse
subscribeToDatasetResponse = SubscribeToDatasetResponse

instance ToPath SubscribeToDataset where
    toPath SubscribeToDataset{..} = mconcat
        [ "/identitypools/"
        , toText _stdIdentityPoolId
        , "/identities/"
        , toText _stdIdentityId
        , "/datasets/"
        , toText _stdDatasetName
        , "/subscriptions/"
        , toText _stdDeviceId
        ]

instance ToQuery SubscribeToDataset where
    toQuery = const mempty

instance ToHeaders SubscribeToDataset

instance ToJSON SubscribeToDataset where
    toJSON = const (toJSON Empty)

instance AWSRequest SubscribeToDataset where
    type Sv SubscribeToDataset = CognitoSync
    type Rs SubscribeToDataset = SubscribeToDatasetResponse

    request  = post
    response = nullResponse SubscribeToDatasetResponse


Some kind of operator / class to check the types whether to continue?

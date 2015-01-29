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

-- Module      : Network.AWS.EC2.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the data feed for Spot Instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html SpotInstance Data Feed> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSpotDatafeedSubscription.html>
module Network.AWS.EC2.DeleteSpotDatafeedSubscription
    (
    -- * Request
      DeleteSpotDatafeedSubscription
    -- ** Request constructor
    , deleteSpotDatafeedSubscription
    -- ** Request lenses
    , dsds1DryRun

    -- * Response
    , DeleteSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , deleteSpotDatafeedSubscriptionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

newtype DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    { _dsds1DryRun :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteSpotDatafeedSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsds1DryRun' @::@ 'Maybe' 'Bool'
--
deleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription
deleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    { _dsds1DryRun = Nothing
    }

dsds1DryRun :: Lens' DeleteSpotDatafeedSubscription (Maybe Bool)
dsds1DryRun = lens _dsds1DryRun (\s a -> s { _dsds1DryRun = a })

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteSpotDatafeedSubscriptionResponse' constructor.
deleteSpotDatafeedSubscriptionResponse :: DeleteSpotDatafeedSubscriptionResponse
deleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse

instance ToPath DeleteSpotDatafeedSubscription where
    toPath = const "/"

instance ToQuery DeleteSpotDatafeedSubscription where
    toQuery DeleteSpotDatafeedSubscription{..} = mconcat
        [ "DryRun" =? _dsds1DryRun
        ]

instance ToHeaders DeleteSpotDatafeedSubscription

instance AWSRequest DeleteSpotDatafeedSubscription where
    type Sv DeleteSpotDatafeedSubscription = EC2
    type Rs DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscriptionResponse

    request  = post "DeleteSpotDatafeedSubscription"
    response = nullResponse DeleteSpotDatafeedSubscriptionResponse

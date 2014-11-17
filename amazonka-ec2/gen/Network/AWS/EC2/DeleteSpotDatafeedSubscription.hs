{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Deletes the datafeed for Spot Instances. For more information, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide.
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
    } deriving (Eq, Ord, Show, Generic)

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
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteSpotDatafeedSubscriptionResponse' constructor.
deleteSpotDatafeedSubscriptionResponse :: DeleteSpotDatafeedSubscriptionResponse
deleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse

instance AWSRequest DeleteSpotDatafeedSubscription where
    type Sv DeleteSpotDatafeedSubscription = EC2
    type Rs DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscriptionResponse

    request  = post "DeleteSpotDatafeedSubscription"
    response = nullResponse DeleteSpotDatafeedSubscriptionResponse

instance ToPath DeleteSpotDatafeedSubscription where
    toPath = const "/"

instance ToHeaders DeleteSpotDatafeedSubscription

instance ToQuery DeleteSpotDatafeedSubscription

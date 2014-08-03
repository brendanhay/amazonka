{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the datafeed for Spot Instances. For more information, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example request deletes the datafeed for the AWS account.
-- https://ec2.amazonaws.com/?Action=DeleteSpotDatafeedSubscription
-- &amp;AUTHPARAMS &lt;DeleteSpotDatafeedSubscriptionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteSpotDatafeedSubscriptionResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteSpotDatafeedSubscription where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteSpotDatafeedSubscription' request.
deleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription
deleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    { _dsdstDryRun = Nothing
    }

data DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    { _dsdstDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

makeLenses ''DeleteSpotDatafeedSubscription

instance ToQuery DeleteSpotDatafeedSubscription where
    toQuery = genericToQuery def

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteSpotDatafeedSubscriptionResponse

instance AWSRequest DeleteSpotDatafeedSubscription where
    type Sv DeleteSpotDatafeedSubscription = EC2
    type Rs DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscriptionResponse

    request = post "DeleteSpotDatafeedSubscription"
    response _ _ = return (Right DeleteSpotDatafeedSubscriptionResponse)

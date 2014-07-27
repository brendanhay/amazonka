{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DeleteSpotDatafeedSubscription
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
module Network.AWS.EC2.V2014_05_01.DeleteSpotDatafeedSubscription where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription
    { _dsdstDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DeleteSpotDatafeedSubscription where
    toQuery = genericToQuery def

instance AWSRequest DeleteSpotDatafeedSubscription where
    type Sv DeleteSpotDatafeedSubscription = EC2
    type Rs DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscriptionResponse

    request = post "DeleteSpotDatafeedSubscription"
    response _ _ = return (Right DeleteSpotDatafeedSubscriptionResponse)

data DeleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse
    deriving (Eq, Show, Generic)

{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the datafeed for Spot Instances. For more information, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example describes the datafeed for the account.
-- https://ec2.amazonaws.com/?Action=DescribeSpotDatafeedSubscription
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE 123456789012
-- my-s3-bucket spotdata_ Active.
module Network.AWS.EC2.V2014_05_01.DescribeSpotDatafeedSubscription where

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
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    { _dsdsrDryRun :: Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DescribeSpotDatafeedSubscription where
    toQuery = genericToQuery def

instance AWSRequest DescribeSpotDatafeedSubscription where
    type Sv DescribeSpotDatafeedSubscription = EC2
    type Rs DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscriptionResponse

    request = post "DescribeSpotDatafeedSubscription"

    response _ = xmlResponse

data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { _dsdssSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
      -- ^ The Spot Instance datafeed subscription.
    } deriving (Generic)

instance FromXML DescribeSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions

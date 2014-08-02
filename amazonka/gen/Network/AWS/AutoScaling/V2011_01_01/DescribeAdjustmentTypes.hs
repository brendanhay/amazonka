{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns policy adjustment types for use in the PutScalingPolicy action.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01
-- &Action=DescribeAdjustmentTypes &AUTHPARAMS ChangeInCapacity ExactCapacity
-- PercentChangeInCapacity cc5f0337-b694-11e2-afc0-6544dEXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.DescribeAdjustmentTypes where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DescribeAdjustmentTypes = DescribeAdjustmentTypes
    deriving (Eq, Show, Generic)

makeLenses ''DescribeAdjustmentTypes

instance ToQuery DescribeAdjustmentTypes where
    toQuery = genericToQuery def

data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { _dataAdjustmentTypes :: [AdjustmentType]
      -- ^ A list of specific policy adjustment types.
    } deriving (Generic)

makeLenses ''DescribeAdjustmentTypesResponse

instance FromXML DescribeAdjustmentTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAdjustmentTypes where
    type Sv DescribeAdjustmentTypes = AutoScaling
    type Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesResponse

    request = post "DescribeAdjustmentTypes"
    response _ = xmlResponse

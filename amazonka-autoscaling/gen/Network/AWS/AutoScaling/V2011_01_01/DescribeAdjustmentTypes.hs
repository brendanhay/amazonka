{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.AutoScaling.V2011_01_01.DescribeAdjustmentTypes
    (
    -- * Request
      DescribeAdjustmentTypes
    -- ** Request constructor
    , mkDescribeAdjustmentTypes
    -- * Response
    , DescribeAdjustmentTypesResponse
    -- ** Response constructor
    , mkDescribeAdjustmentTypesResponse
    -- ** Response lenses
    , datrAdjustmentTypes
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

data DescribeAdjustmentTypes = DescribeAdjustmentTypes
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAdjustmentTypes' request.
mkDescribeAdjustmentTypes :: DescribeAdjustmentTypes
mkDescribeAdjustmentTypes = DescribeAdjustmentTypes

instance ToQuery DescribeAdjustmentTypes where
    toQuery = genericQuery def

-- | The output of the DescribeAdjustmentTypes action.
newtype DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { _datrAdjustmentTypes :: [AdjustmentType]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAdjustmentTypesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AdjustmentTypes ::@ @[AdjustmentType]@
--
mkDescribeAdjustmentTypesResponse :: DescribeAdjustmentTypesResponse
mkDescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { _datrAdjustmentTypes = mempty
    }

-- | A list of specific policy adjustment types.
datrAdjustmentTypes :: Lens' DescribeAdjustmentTypesResponse [AdjustmentType]
datrAdjustmentTypes =
    lens _datrAdjustmentTypes (\s a -> s { _datrAdjustmentTypes = a })

instance FromXML DescribeAdjustmentTypesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAdjustmentTypes where
    type Sv DescribeAdjustmentTypes = AutoScaling
    type Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesResponse

    request = post "DescribeAdjustmentTypes"
    response _ = xmlResponse

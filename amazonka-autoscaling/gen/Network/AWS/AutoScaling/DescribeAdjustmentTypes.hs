{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns policy adjustment types for use in the PutScalingPolicy action.
module Network.AWS.AutoScaling.DescribeAdjustmentTypes
    (
    -- * Request
      DescribeAdjustmentTypes
    -- ** Request constructor
    , describeAdjustmentTypes

    -- * Response
    , DescribeAdjustmentTypesResponse
    -- ** Response constructor
    , describeAdjustmentTypesResponse
    -- ** Response lenses
    , datrAdjustmentTypes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeAdjustmentTypes = DescribeAdjustmentTypes
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAdjustmentTypes' constructor.
describeAdjustmentTypes :: DescribeAdjustmentTypes
describeAdjustmentTypes = DescribeAdjustmentTypes

newtype DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { _datrAdjustmentTypes :: [AdjustmentType]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeAdjustmentTypesResponse where
    type Item DescribeAdjustmentTypesResponse = AdjustmentType

    fromList = DescribeAdjustmentTypesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _datrAdjustmentTypes

-- | 'DescribeAdjustmentTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'datrAdjustmentTypes' @::@ ['AdjustmentType']
--
describeAdjustmentTypesResponse :: DescribeAdjustmentTypesResponse
describeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse
    { _datrAdjustmentTypes = mempty
    }

-- | A list of specific policy adjustment types.
datrAdjustmentTypes :: Lens' DescribeAdjustmentTypesResponse [AdjustmentType]
datrAdjustmentTypes =
    lens _datrAdjustmentTypes (\s a -> s { _datrAdjustmentTypes = a })

instance AWSRequest DescribeAdjustmentTypes where
    type Sv DescribeAdjustmentTypes = AutoScaling
    type Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesResponse

    request  = post "DescribeAdjustmentTypes"
    response = xmlResponse

instance FromXML DescribeAdjustmentTypesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAdjustmentTypesResponse"

instance ToPath DescribeAdjustmentTypes where
    toPath = const "/"

instance ToHeaders DescribeAdjustmentTypes

instance ToQuery DescribeAdjustmentTypes

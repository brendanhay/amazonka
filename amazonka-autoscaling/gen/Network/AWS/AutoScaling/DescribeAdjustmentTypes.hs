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
    , DescribeAdjustmentTypesAnswer
    -- ** Response constructor
    , describeAdjustmentTypesResponse
    -- ** Response lenses
    , dataAdjustmentTypes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeAdjustmentTypes = DescribeAdjustmentTypes
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAdjustmentTypes' constructor.
describeAdjustmentTypes :: DescribeAdjustmentTypes
describeAdjustmentTypes = DescribeAdjustmentTypes

instance ToQuery DescribeAdjustmentTypes

instance ToPath DescribeAdjustmentTypes where
    toPath = const "/"

newtype DescribeAdjustmentTypesAnswer = DescribeAdjustmentTypesAnswer
    { _dataAdjustmentTypes :: [AdjustmentType]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeAdjustmentTypesAnswer where
    type Item DescribeAdjustmentTypesAnswer = AdjustmentType

    fromList = DescribeAdjustmentTypesAnswer . fromList
    toList   = toList . _dataAdjustmentTypes

-- | 'DescribeAdjustmentTypesAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dataAdjustmentTypes' @::@ ['AdjustmentType']
--
describeAdjustmentTypesResponse :: DescribeAdjustmentTypesAnswer
describeAdjustmentTypesResponse = DescribeAdjustmentTypesAnswer
    { _dataAdjustmentTypes = mempty
    }

-- | A list of specific policy adjustment types.
dataAdjustmentTypes :: Lens' DescribeAdjustmentTypesAnswer [AdjustmentType]
dataAdjustmentTypes =
    lens _dataAdjustmentTypes (\s a -> s { _dataAdjustmentTypes = a })

instance FromXML DescribeAdjustmentTypesAnswer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAdjustmentTypesAnswer"

instance AWSRequest DescribeAdjustmentTypes where
    type Sv DescribeAdjustmentTypes = AutoScaling
    type Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesAnswer

    request  = post "DescribeAdjustmentTypes"
    response = xmlResponse $ \h x -> DescribeAdjustmentTypesAnswer
        <$> x %| "AdjustmentTypes"

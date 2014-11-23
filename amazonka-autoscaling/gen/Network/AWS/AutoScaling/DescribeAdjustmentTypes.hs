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

-- Module      : Network.AWS.AutoScaling.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the policy adjustment types for use with PutScalingPolicy.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAdjustmentTypes.html>
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
    { _datrAdjustmentTypes :: List "AdjustmentTypes" AdjustmentType
    } deriving (Eq, Show, Monoid, Semigroup)

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

-- | The policy adjustment types.
datrAdjustmentTypes :: Lens' DescribeAdjustmentTypesResponse [AdjustmentType]
datrAdjustmentTypes =
    lens _datrAdjustmentTypes (\s a -> s { _datrAdjustmentTypes = a })
        . _List

instance ToPath DescribeAdjustmentTypes where
    toPath = const "/"

instance ToQuery DescribeAdjustmentTypes where
    toQuery = const mempty

instance ToHeaders DescribeAdjustmentTypes

instance AWSRequest DescribeAdjustmentTypes where
    type Sv DescribeAdjustmentTypes = AutoScaling
    type Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesResponse

    request  = post "DescribeAdjustmentTypes"
    response = xmlResponse

instance FromXML DescribeAdjustmentTypesResponse where
    parseXML = withElement "DescribeAdjustmentTypesResult" $ \x -> DescribeAdjustmentTypesResponse
        <$> x .@? "AdjustmentTypes"

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

-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
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

-- | Returns scaling process types for use in the 'ResumeProcesses' and 'SuspendProcesses' actions.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingProcessTypes.html>
module Network.AWS.AutoScaling.DescribeScalingProcessTypes
    (
    -- * Request
      DescribeScalingProcessTypes
    -- ** Request constructor
    , describeScalingProcessTypes

    -- * Response
    , DescribeScalingProcessTypesResponse
    -- ** Response constructor
    , describeScalingProcessTypesResponse
    -- ** Response lenses
    , dsptrProcesses
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeScalingProcessTypes = DescribeScalingProcessTypes
    deriving (Eq, Ord, Show, Generic)

-- | 'DescribeScalingProcessTypes' constructor.
describeScalingProcessTypes :: DescribeScalingProcessTypes
describeScalingProcessTypes = DescribeScalingProcessTypes

newtype DescribeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
    { _dsptrProcesses :: List "member" ProcessType
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeScalingProcessTypesResponse where
    type Item DescribeScalingProcessTypesResponse = ProcessType

    fromList = DescribeScalingProcessTypesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dsptrProcesses

-- | 'DescribeScalingProcessTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsptrProcesses' @::@ ['ProcessType']
--
describeScalingProcessTypesResponse :: DescribeScalingProcessTypesResponse
describeScalingProcessTypesResponse = DescribeScalingProcessTypesResponse
    { _dsptrProcesses = mempty
    }

-- | The names of the process types.
dsptrProcesses :: Lens' DescribeScalingProcessTypesResponse [ProcessType]
dsptrProcesses = lens _dsptrProcesses (\s a -> s { _dsptrProcesses = a }) . _List

instance ToPath DescribeScalingProcessTypes where
    toPath = const "/"

instance ToQuery DescribeScalingProcessTypes where
    toQuery = const mempty

instance ToHeaders DescribeScalingProcessTypes

instance AWSRequest DescribeScalingProcessTypes where
    type Sv DescribeScalingProcessTypes = AutoScaling
    type Rs DescribeScalingProcessTypes = DescribeScalingProcessTypesResponse

    request  = post "DescribeScalingProcessTypes"
    response = xmlResponse

instance FromXML DescribeScalingProcessTypesResponse where
    parseXML = withElement "DescribeScalingProcessTypesResult" $ \x -> DescribeScalingProcessTypesResponse
        <$> x .@  "Processes"

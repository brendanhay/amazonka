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

-- Module      : Network.AWS.AutoScaling.DescribeScalingProcessTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns scaling process types for use in the ResumeProcesses and
-- SuspendProcesses actions.
module Network.AWS.AutoScaling.DescribeScalingProcessTypes
    (
    -- * Request
      DescribeScalingProcessTypes
    -- ** Request constructor
    , describeScalingProcessTypes

    -- * Response
    , ProcessesType
    -- ** Response constructor
    , describeScalingProcessTypesResponse
    -- ** Response lenses
    , ptProcesses
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeScalingProcessTypes = DescribeScalingProcessTypes

-- | 'DescribeScalingProcessTypes' constructor.
describeScalingProcessTypes :: DescribeScalingProcessTypes
describeScalingProcessTypes = DescribeScalingProcessTypes

instance ToPath DescribeScalingProcessTypes where
    toPath = const "/"

instance ToQuery DescribeScalingProcessTypes

newtype ProcessesType = ProcessesType
    { _ptProcesses :: [ProcessType]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'ProcessesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ptProcesses' @::@ ['ProcessType']
--
describeScalingProcessTypesResponse :: ProcessesType
describeScalingProcessTypesResponse = ProcessesType
    { _ptProcesses = mempty
    }

-- | A list of ProcessType names.
ptProcesses :: Lens' ProcessesType [ProcessType]
ptProcesses = lens _ptProcesses (\s a -> s { _ptProcesses = a })

instance AWSRequest DescribeScalingProcessTypes where
    type Sv DescribeScalingProcessTypes = AutoScaling
    type Rs DescribeScalingProcessTypes = ProcessesType

    request  = post "DescribeScalingProcessTypes"
    response = xmlResponse $ \h x -> ProcessesType
        <$> x %| "Processes"

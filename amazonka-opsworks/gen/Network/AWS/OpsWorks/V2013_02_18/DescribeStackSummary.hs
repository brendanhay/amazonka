{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeStackSummary
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the number of layers and apps in a specified stack, and the
-- number of instances in each state, such as running_setup or online.
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeStackSummary
    (
    -- * Request
      DescribeStackSummary
    -- ** Request constructor
    , describeStackSummary
    -- ** Request lenses
    , dssrStackId

    -- * Response
    , DescribeStackSummaryResponse
    -- ** Response lenses
    , dsssStackSummary
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeStackSummary' request.
describeStackSummary :: Text -- ^ 'dssrStackId'
                     -> DescribeStackSummary
describeStackSummary p1 = DescribeStackSummary
    { _dssrStackId = p1
    }

data DescribeStackSummary = DescribeStackSummary
    { _dssrStackId :: Text
      -- ^ The stack ID.
    } deriving (Show, Generic)

-- | The stack ID.
dssrStackId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeStackSummary
    -> f DescribeStackSummary
dssrStackId f x =
    (\y -> x { _dssrStackId = y })
       <$> f (_dssrStackId x)
{-# INLINE dssrStackId #-}

instance ToPath DescribeStackSummary

instance ToQuery DescribeStackSummary

instance ToHeaders DescribeStackSummary

instance ToJSON DescribeStackSummary

data DescribeStackSummaryResponse = DescribeStackSummaryResponse
    { _dsssStackSummary :: Maybe StackSummary
      -- ^ A StackSummary object that contains the results.
    } deriving (Show, Generic)

-- | A StackSummary object that contains the results.
dsssStackSummary
    :: Functor f
    => (Maybe StackSummary
    -> f (Maybe StackSummary))
    -> DescribeStackSummaryResponse
    -> f DescribeStackSummaryResponse
dsssStackSummary f x =
    (\y -> x { _dsssStackSummary = y })
       <$> f (_dsssStackSummary x)
{-# INLINE dsssStackSummary #-}

instance FromJSON DescribeStackSummaryResponse

instance AWSRequest DescribeStackSummary where
    type Sv DescribeStackSummary = OpsWorks
    type Rs DescribeStackSummary = DescribeStackSummaryResponse

    request = get
    response _ = jsonResponse

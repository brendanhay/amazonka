{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks
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
module Network.AWS.OpsWorks
    (
    -- * Request
      DescribeStackSummary
    -- ** Request constructor
    , mkDescribeStackSummary
    -- ** Request lenses
    , dssStackId

    -- * Response
    , DescribeStackSummaryResponse
    -- ** Response constructor
    , mkDescribeStackSummaryResponse
    -- ** Response lenses
    , dssrStackSummary
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DescribeStackSummary = DescribeStackSummary
    { _dssStackId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStackSummary' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Text@
--
mkDescribeStackSummary :: Text -- ^ 'dssStackId'
                       -> DescribeStackSummary
mkDescribeStackSummary p1 = DescribeStackSummary
    { _dssStackId = p1
    }

-- | The stack ID.
dssStackId :: Lens' DescribeStackSummary Text
dssStackId = lens _dssStackId (\s a -> s { _dssStackId = a })

instance ToPath DescribeStackSummary

instance ToQuery DescribeStackSummary

instance ToHeaders DescribeStackSummary

instance ToJSON DescribeStackSummary

-- | Contains the response to a DescribeStackSummary request.
newtype DescribeStackSummaryResponse = DescribeStackSummaryResponse
    { _dssrStackSummary :: Maybe StackSummary
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStackSummaryResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackSummary ::@ @Maybe StackSummary@
--
mkDescribeStackSummaryResponse :: DescribeStackSummaryResponse
mkDescribeStackSummaryResponse = DescribeStackSummaryResponse
    { _dssrStackSummary = Nothing
    }

-- | A StackSummary object that contains the results.
dssrStackSummary :: Lens' DescribeStackSummaryResponse (Maybe StackSummary)
dssrStackSummary =
    lens _dssrStackSummary (\s a -> s { _dssrStackSummary = a })

instance FromJSON DescribeStackSummaryResponse

instance AWSRequest DescribeStackSummary where
    type Sv DescribeStackSummary = OpsWorks
    type Rs DescribeStackSummary = DescribeStackSummaryResponse

    request = get
    response _ = jsonResponse

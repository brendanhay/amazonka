{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DescribeDeployments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a specified set of deployments. You must specify
-- at least one of the parameters. Required Permissions: To use this action,
-- an IAM user must have a Show, Deploy, or Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DescribeDeployments
    (
    -- * Request
      DescribeDeployments
    -- ** Request constructor
    , describeDeployments
    -- ** Request lenses
    , ddrStackId
    , ddrAppId
    , ddrDeploymentIds

    -- * Response
    , DescribeDeploymentsResponse
    -- ** Response lenses
    , ddsDeployments
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeDeployments' request.
describeDeployments :: DescribeDeployments
describeDeployments = DescribeDeployments
    { _ddrStackId = Nothing
    , _ddrAppId = Nothing
    , _ddrDeploymentIds = mempty
    }
{-# INLINE describeDeployments #-}

data DescribeDeployments = DescribeDeployments
    { _ddrStackId :: Maybe Text
      -- ^ The stack ID. If you include this parameter, DescribeDeployments
      -- returns a description of the commands associated with the
      -- specified stack.
    , _ddrAppId :: Maybe Text
      -- ^ The app ID. If you include this parameter, DescribeDeployments
      -- returns a description of the commands associated with the
      -- specified app.
    , _ddrDeploymentIds :: [Text]
      -- ^ An array of deployment IDs to be described. If you include this
      -- parameter, DescribeDeployments returns a description of the
      -- specified deployments. Otherwise, it returns a description of
      -- every deployment.
    } deriving (Show, Generic)

-- | The stack ID. If you include this parameter, DescribeDeployments returns a
-- description of the commands associated with the specified stack.
ddrStackId :: Lens' DescribeDeployments (Maybe Text)
ddrStackId f x =
    f (_ddrStackId x)
        <&> \y -> x { _ddrStackId = y }
{-# INLINE ddrStackId #-}

-- | The app ID. If you include this parameter, DescribeDeployments returns a
-- description of the commands associated with the specified app.
ddrAppId :: Lens' DescribeDeployments (Maybe Text)
ddrAppId f x =
    f (_ddrAppId x)
        <&> \y -> x { _ddrAppId = y }
{-# INLINE ddrAppId #-}

-- | An array of deployment IDs to be described. If you include this parameter,
-- DescribeDeployments returns a description of the specified deployments.
-- Otherwise, it returns a description of every deployment.
ddrDeploymentIds :: Lens' DescribeDeployments ([Text])
ddrDeploymentIds f x =
    f (_ddrDeploymentIds x)
        <&> \y -> x { _ddrDeploymentIds = y }
{-# INLINE ddrDeploymentIds #-}

instance ToPath DescribeDeployments

instance ToQuery DescribeDeployments

instance ToHeaders DescribeDeployments

instance ToJSON DescribeDeployments

data DescribeDeploymentsResponse = DescribeDeploymentsResponse
    { _ddsDeployments :: [Deployment]
      -- ^ An array of Deployment objects that describe the deployments.
    } deriving (Show, Generic)

-- | An array of Deployment objects that describe the deployments.
ddsDeployments :: Lens' DescribeDeploymentsResponse ([Deployment])
ddsDeployments f x =
    f (_ddsDeployments x)
        <&> \y -> x { _ddsDeployments = y }
{-# INLINE ddsDeployments #-}

instance FromJSON DescribeDeploymentsResponse

instance AWSRequest DescribeDeployments where
    type Sv DescribeDeployments = OpsWorks
    type Rs DescribeDeployments = DescribeDeploymentsResponse

    request = get
    response _ = jsonResponse

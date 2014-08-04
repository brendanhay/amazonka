{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DescribeDeployments where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeDeployments' request.
describeDeployments :: DescribeDeployments
describeDeployments = DescribeDeployments
    { _ddrAppId = Nothing
    , _ddrStackId = Nothing
    , _ddrDeploymentIds = mempty
    }

data DescribeDeployments = DescribeDeployments
    { _ddrAppId :: Maybe Text
      -- ^ The app ID. If you include this parameter, DescribeDeployments
      -- returns a description of the commands associated with the
      -- specified app.
    , _ddrStackId :: Maybe Text
      -- ^ The stack ID. If you include this parameter, DescribeDeployments
      -- returns a description of the commands associated with the
      -- specified stack.
    , _ddrDeploymentIds :: [Text]
      -- ^ An array of deployment IDs to be described. If you include this
      -- parameter, DescribeDeployments returns a description of the
      -- specified deployments. Otherwise, it returns a description of
      -- every deployment.
    } deriving (Generic)

makeLenses ''DescribeDeployments

instance ToPath DescribeDeployments

instance ToQuery DescribeDeployments

instance ToHeaders DescribeDeployments

instance ToJSON DescribeDeployments

data DescribeDeploymentsResponse = DescribeDeploymentsResponse
    { _ddsDeployments :: [Deployment]
      -- ^ An array of Deployment objects that describe the deployments.
    } deriving (Generic)

makeLenses ''DescribeDeploymentsResponse

instance FromJSON DescribeDeploymentsResponse

instance AWSRequest DescribeDeployments where
    type Sv DescribeDeployments = OpsWorks
    type Rs DescribeDeployments = DescribeDeploymentsResponse

    request = get
    response _ = jsonResponse

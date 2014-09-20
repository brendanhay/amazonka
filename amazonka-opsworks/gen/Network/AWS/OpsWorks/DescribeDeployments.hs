{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DescribeDeployments
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
module Network.AWS.OpsWorks.DescribeDeployments
    (
    -- * Request
      DescribeDeployments
    -- ** Request constructor
    , describeDeployments
    -- ** Request lenses
    , ddStackId
    , ddAppId
    , ddDeploymentIds

    -- * Response
    , DescribeDeploymentsResponse
    -- ** Response constructor
    , describeDeploymentsResponse
    -- ** Response lenses
    , ddrDeployments
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DescribeDeployments = DescribeDeployments
    { _ddStackId :: Maybe Text
    , _ddAppId :: Maybe Text
    , _ddDeploymentIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDeployments' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Maybe Text@
--
-- * @AppId ::@ @Maybe Text@
--
-- * @DeploymentIds ::@ @[Text]@
--
describeDeployments :: DescribeDeployments
describeDeployments = DescribeDeployments
    { _ddStackId = Nothing
    , _ddAppId = Nothing
    , _ddDeploymentIds = mempty
    }

-- | The stack ID. If you include this parameter, DescribeDeployments returns a
-- description of the commands associated with the specified stack.
ddStackId :: Lens' DescribeDeployments (Maybe Text)
ddStackId = lens _ddStackId (\s a -> s { _ddStackId = a })

-- | The app ID. If you include this parameter, DescribeDeployments returns a
-- description of the commands associated with the specified app.
ddAppId :: Lens' DescribeDeployments (Maybe Text)
ddAppId = lens _ddAppId (\s a -> s { _ddAppId = a })

-- | An array of deployment IDs to be described. If you include this parameter,
-- DescribeDeployments returns a description of the specified deployments.
-- Otherwise, it returns a description of every deployment.
ddDeploymentIds :: Lens' DescribeDeployments [Text]
ddDeploymentIds = lens _ddDeploymentIds (\s a -> s { _ddDeploymentIds = a })

instance ToPath DescribeDeployments

instance ToQuery DescribeDeployments

instance ToHeaders DescribeDeployments

instance ToJSON DescribeDeployments

-- | Contains the response to a DescribeDeployments request.
newtype DescribeDeploymentsResponse = DescribeDeploymentsResponse
    { _ddrDeployments :: [Deployment]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeDeploymentsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Deployments ::@ @[Deployment]@
--
describeDeploymentsResponse :: DescribeDeploymentsResponse
describeDeploymentsResponse = DescribeDeploymentsResponse
    { _ddrDeployments = mempty
    }

-- | An array of Deployment objects that describe the deployments.
ddrDeployments :: Lens' DescribeDeploymentsResponse [Deployment]
ddrDeployments = lens _ddrDeployments (\s a -> s { _ddrDeployments = a })

instance FromJSON DescribeDeploymentsResponse

instance AWSRequest DescribeDeployments where
    type Sv DescribeDeployments = OpsWorks
    type Rs DescribeDeployments = DescribeDeploymentsResponse

    request = get
    response _ = jsonResponse

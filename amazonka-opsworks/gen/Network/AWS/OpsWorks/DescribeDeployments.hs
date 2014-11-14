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

-- Module      : Network.AWS.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a specified set of deployments. Required
-- Permissions: To use this action, an IAM user must have a Show, Deploy, or
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.DescribeDeployments
    (
    -- * Request
      DescribeDeployments
    -- ** Request constructor
    , describeDeployments
    -- ** Request lenses
    , ddAppId
    , ddDeploymentIds
    , ddStackId

    -- * Response
    , DescribeDeploymentsResponse
    -- ** Response constructor
    , describeDeploymentsResponse
    -- ** Response lenses
    , ddrDeployments
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data DescribeDeployments = DescribeDeployments
    { _ddAppId         :: Maybe Text
    , _ddDeploymentIds :: [Text]
    , _ddStackId       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeDeployments' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddAppId' @::@ 'Maybe' 'Text'
--
-- * 'ddDeploymentIds' @::@ ['Text']
--
-- * 'ddStackId' @::@ 'Maybe' 'Text'
--
describeDeployments :: DescribeDeployments
describeDeployments = DescribeDeployments
    { _ddStackId       = Nothing
    , _ddAppId         = Nothing
    , _ddDeploymentIds = mempty
    }

-- | The app ID. If you include this parameter, DescribeDeployments returns a
-- description of the commands associated with the specified app.
ddAppId :: Lens' DescribeDeployments (Maybe Text)
ddAppId = lens _ddAppId (\s a -> s { _ddAppId = a })

-- | An array of deployment IDs to be described. If you include this
-- parameter, DescribeDeployments returns a description of the specified
-- deployments. Otherwise, it returns a description of every deployment.
ddDeploymentIds :: Lens' DescribeDeployments [Text]
ddDeploymentIds = lens _ddDeploymentIds (\s a -> s { _ddDeploymentIds = a })

-- | The stack ID. If you include this parameter, DescribeDeployments returns
-- a description of the commands associated with the specified stack.
ddStackId :: Lens' DescribeDeployments (Maybe Text)
ddStackId = lens _ddStackId (\s a -> s { _ddStackId = a })

instance ToPath DescribeDeployments where
    toPath = const "/"

instance ToQuery DescribeDeployments where
    toQuery = const mempty

instance ToHeaders DescribeDeployments

instance ToBody DescribeDeployments where
    toBody = toBody . encode . _ddStackId

newtype DescribeDeploymentsResponse = DescribeDeploymentsResponse
    { _ddrDeployments :: [Deployment]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeDeploymentsResponse where
    type Item DescribeDeploymentsResponse = Deployment

    fromList = DescribeDeploymentsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ddrDeployments

-- | 'DescribeDeploymentsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrDeployments' @::@ ['Deployment']
--
describeDeploymentsResponse :: DescribeDeploymentsResponse
describeDeploymentsResponse = DescribeDeploymentsResponse
    { _ddrDeployments = mempty
    }

-- | An array of Deployment objects that describe the deployments.
ddrDeployments :: Lens' DescribeDeploymentsResponse [Deployment]
ddrDeployments = lens _ddrDeployments (\s a -> s { _ddrDeployments = a })

instance AWSRequest DescribeDeployments where
    type Sv DescribeDeployments = OpsWorks
    type Rs DescribeDeployments = DescribeDeploymentsResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeDeploymentsResponse
        <$> o .: "Deployments"

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

-- Module      : Network.AWS.OpsWorks.DescribeDeployments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a description of a specified set of deployments.
--
-- Required Permissions: To use this action, an IAM user must have a Show,
-- Deploy, or Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DescribeDeployments.html>
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
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data DescribeDeployments = DescribeDeployments
    { _ddAppId         :: Maybe Text
    , _ddDeploymentIds :: List "InstanceIds" Text
    , _ddStackId       :: Maybe Text
    } deriving (Eq, Ord, Show)

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

-- | The app ID. If you include this parameter, 'DescribeDeployments' returns a
-- description of the commands associated with the specified app.
ddAppId :: Lens' DescribeDeployments (Maybe Text)
ddAppId = lens _ddAppId (\s a -> s { _ddAppId = a })

-- | An array of deployment IDs to be described. If you include this parameter, 'DescribeDeployments' returns a description of the specified deployments. Otherwise, it returns a
-- description of every deployment.
ddDeploymentIds :: Lens' DescribeDeployments [Text]
ddDeploymentIds = lens _ddDeploymentIds (\s a -> s { _ddDeploymentIds = a }) . _List

-- | The stack ID. If you include this parameter, 'DescribeDeployments' returns a
-- description of the commands associated with the specified stack.
ddStackId :: Lens' DescribeDeployments (Maybe Text)
ddStackId = lens _ddStackId (\s a -> s { _ddStackId = a })

newtype DescribeDeploymentsResponse = DescribeDeploymentsResponse
    { _ddrDeployments :: List "Deployments" Deployment
    } deriving (Eq, Show, Monoid, Semigroup)

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

-- | An array of 'Deployment' objects that describe the deployments.
ddrDeployments :: Lens' DescribeDeploymentsResponse [Deployment]
ddrDeployments = lens _ddrDeployments (\s a -> s { _ddrDeployments = a }) . _List

instance ToPath DescribeDeployments where
    toPath = const "/"

instance ToQuery DescribeDeployments where
    toQuery = const mempty

instance ToHeaders DescribeDeployments

instance ToJSON DescribeDeployments where
    toJSON DescribeDeployments{..} = object
        [ "StackId"       .= _ddStackId
        , "AppId"         .= _ddAppId
        , "DeploymentIds" .= _ddDeploymentIds
        ]

instance AWSRequest DescribeDeployments where
    type Sv DescribeDeployments = OpsWorks
    type Rs DescribeDeployments = DescribeDeploymentsResponse

    request  = post "DescribeDeployments"
    response = jsonResponse

instance FromJSON DescribeDeploymentsResponse where
    parseJSON = withObject "DescribeDeploymentsResponse" $ \o -> DescribeDeploymentsResponse
        <$> o .:  "Deployments"

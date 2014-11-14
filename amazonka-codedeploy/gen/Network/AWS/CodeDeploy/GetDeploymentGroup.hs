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

-- Module      : Network.AWS.CodeDeploy.GetDeploymentGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about a deployment group.
module Network.AWS.CodeDeploy.GetDeploymentGroup
    (
    -- * Request
      GetDeploymentGroup
    -- ** Request constructor
    , getDeploymentGroup
    -- ** Request lenses
    , gdgApplicationName
    , gdgDeploymentGroupName

    -- * Response
    , GetDeploymentGroupResponse
    -- ** Response constructor
    , getDeploymentGroupResponse
    -- ** Response lenses
    , gdgrDeploymentGroupInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CodeDeploy.Types

data GetDeploymentGroup = GetDeploymentGroup
    { _gdgApplicationName     :: Text
    , _gdgDeploymentGroupName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetDeploymentGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdgApplicationName' @::@ 'Text'
--
-- * 'gdgDeploymentGroupName' @::@ 'Text'
--
getDeploymentGroup :: Text -- ^ 'gdgApplicationName'
                   -> Text -- ^ 'gdgDeploymentGroupName'
                   -> GetDeploymentGroup
getDeploymentGroup p1 p2 = GetDeploymentGroup
    { _gdgApplicationName     = p1
    , _gdgDeploymentGroupName = p2
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
gdgApplicationName :: Lens' GetDeploymentGroup Text
gdgApplicationName =
    lens _gdgApplicationName (\s a -> s { _gdgApplicationName = a })

-- | The name of an existing deployment group for the specified application.
gdgDeploymentGroupName :: Lens' GetDeploymentGroup Text
gdgDeploymentGroupName =
    lens _gdgDeploymentGroupName (\s a -> s { _gdgDeploymentGroupName = a })

instance ToPath GetDeploymentGroup where
    toPath = const "/"

instance ToQuery GetDeploymentGroup where
    toQuery = const mempty

instance ToHeaders GetDeploymentGroup

instance ToBody GetDeploymentGroup where
    toBody = toBody . encode . _gdgApplicationName

newtype GetDeploymentGroupResponse = GetDeploymentGroupResponse
    { _gdgrDeploymentGroupInfo :: Maybe DeploymentGroupInfo
    } deriving (Eq, Show, Generic)

-- | 'GetDeploymentGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdgrDeploymentGroupInfo' @::@ 'Maybe' 'DeploymentGroupInfo'
--
getDeploymentGroupResponse :: GetDeploymentGroupResponse
getDeploymentGroupResponse = GetDeploymentGroupResponse
    { _gdgrDeploymentGroupInfo = Nothing
    }

-- | Information about the deployment group.
gdgrDeploymentGroupInfo :: Lens' GetDeploymentGroupResponse (Maybe DeploymentGroupInfo)
gdgrDeploymentGroupInfo =
    lens _gdgrDeploymentGroupInfo (\s a -> s { _gdgrDeploymentGroupInfo = a })

instance AWSRequest GetDeploymentGroup where
    type Sv GetDeploymentGroup = CodeDeploy
    type Rs GetDeploymentGroup = GetDeploymentGroupResponse

    request  = post
    response = jsonResponse $ \h o -> GetDeploymentGroupResponse
        <$> o .: "deploymentGroupInfo"

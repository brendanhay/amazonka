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

-- Module      : Network.AWS.CodeDeploy.UpdateDeploymentGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes information about an existing deployment group.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_UpdateDeploymentGroup.html>
module Network.AWS.CodeDeploy.UpdateDeploymentGroup
    (
    -- * Request
      UpdateDeploymentGroup
    -- ** Request constructor
    , updateDeploymentGroup
    -- ** Request lenses
    , udgApplicationName
    , udgAutoScalingGroups
    , udgCurrentDeploymentGroupName
    , udgDeploymentConfigName
    , udgEc2TagFilters
    , udgNewDeploymentGroupName
    , udgServiceRoleArn

    -- * Response
    , UpdateDeploymentGroupResponse
    -- ** Response constructor
    , updateDeploymentGroupResponse
    -- ** Response lenses
    , udgrHooksNotCleanedUp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data UpdateDeploymentGroup = UpdateDeploymentGroup
    { _udgApplicationName            :: Text
    , _udgAutoScalingGroups          :: List "autoScalingGroups" Text
    , _udgCurrentDeploymentGroupName :: Text
    , _udgDeploymentConfigName       :: Maybe Text
    , _udgEc2TagFilters              :: List "ec2TagFilters" EC2TagFilter
    , _udgNewDeploymentGroupName     :: Maybe Text
    , _udgServiceRoleArn             :: Maybe Text
    } deriving (Eq, Show)

-- | 'UpdateDeploymentGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udgApplicationName' @::@ 'Text'
--
-- * 'udgAutoScalingGroups' @::@ ['Text']
--
-- * 'udgCurrentDeploymentGroupName' @::@ 'Text'
--
-- * 'udgDeploymentConfigName' @::@ 'Maybe' 'Text'
--
-- * 'udgEc2TagFilters' @::@ ['EC2TagFilter']
--
-- * 'udgNewDeploymentGroupName' @::@ 'Maybe' 'Text'
--
-- * 'udgServiceRoleArn' @::@ 'Maybe' 'Text'
--
updateDeploymentGroup :: Text -- ^ 'udgApplicationName'
                      -> Text -- ^ 'udgCurrentDeploymentGroupName'
                      -> UpdateDeploymentGroup
updateDeploymentGroup p1 p2 = UpdateDeploymentGroup
    { _udgApplicationName            = p1
    , _udgCurrentDeploymentGroupName = p2
    , _udgNewDeploymentGroupName     = Nothing
    , _udgDeploymentConfigName       = Nothing
    , _udgEc2TagFilters              = mempty
    , _udgAutoScalingGroups          = mempty
    , _udgServiceRoleArn             = Nothing
    }

-- | The application name corresponding to the deployment group to update.
udgApplicationName :: Lens' UpdateDeploymentGroup Text
udgApplicationName =
    lens _udgApplicationName (\s a -> s { _udgApplicationName = a })

-- | The replacement list of Auto Scaling groups to be included in the
-- deployment group, if you want to change them.
udgAutoScalingGroups :: Lens' UpdateDeploymentGroup [Text]
udgAutoScalingGroups =
    lens _udgAutoScalingGroups (\s a -> s { _udgAutoScalingGroups = a })
        . _List

-- | The current name of the existing deployment group.
udgCurrentDeploymentGroupName :: Lens' UpdateDeploymentGroup Text
udgCurrentDeploymentGroupName =
    lens _udgCurrentDeploymentGroupName
        (\s a -> s { _udgCurrentDeploymentGroupName = a })

-- | The replacement deployment configuration name to use, if you want to
-- change it.
udgDeploymentConfigName :: Lens' UpdateDeploymentGroup (Maybe Text)
udgDeploymentConfigName =
    lens _udgDeploymentConfigName (\s a -> s { _udgDeploymentConfigName = a })

-- | The replacement set of Amazon EC2 tags to filter on, if you want to
-- change them.
udgEc2TagFilters :: Lens' UpdateDeploymentGroup [EC2TagFilter]
udgEc2TagFilters = lens _udgEc2TagFilters (\s a -> s { _udgEc2TagFilters = a }) . _List

-- | The new name of the deployment group, if you want to change it.
udgNewDeploymentGroupName :: Lens' UpdateDeploymentGroup (Maybe Text)
udgNewDeploymentGroupName =
    lens _udgNewDeploymentGroupName
        (\s a -> s { _udgNewDeploymentGroupName = a })

-- | A replacement service role's ARN, if you want to change it.
udgServiceRoleArn :: Lens' UpdateDeploymentGroup (Maybe Text)
udgServiceRoleArn =
    lens _udgServiceRoleArn (\s a -> s { _udgServiceRoleArn = a })

newtype UpdateDeploymentGroupResponse = UpdateDeploymentGroupResponse
    { _udgrHooksNotCleanedUp :: List "autoScalingGroups" AutoScalingGroup
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList UpdateDeploymentGroupResponse where
    type Item UpdateDeploymentGroupResponse = AutoScalingGroup

    fromList = UpdateDeploymentGroupResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _udgrHooksNotCleanedUp

-- | 'UpdateDeploymentGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udgrHooksNotCleanedUp' @::@ ['AutoScalingGroup']
--
updateDeploymentGroupResponse :: UpdateDeploymentGroupResponse
updateDeploymentGroupResponse = UpdateDeploymentGroupResponse
    { _udgrHooksNotCleanedUp = mempty
    }

-- | If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the AWS
-- user account. If the output does contain data, AWS CodeDeploy could not
-- remove some Auto Scaling lifecycle event hooks from the AWS user account.
udgrHooksNotCleanedUp :: Lens' UpdateDeploymentGroupResponse [AutoScalingGroup]
udgrHooksNotCleanedUp =
    lens _udgrHooksNotCleanedUp (\s a -> s { _udgrHooksNotCleanedUp = a })
        . _List

instance ToPath UpdateDeploymentGroup where
    toPath = const "/"

instance ToQuery UpdateDeploymentGroup where
    toQuery = const mempty

instance ToHeaders UpdateDeploymentGroup

instance ToJSON UpdateDeploymentGroup where
    toJSON UpdateDeploymentGroup{..} = object
        [ "applicationName"            .= _udgApplicationName
        , "currentDeploymentGroupName" .= _udgCurrentDeploymentGroupName
        , "newDeploymentGroupName"     .= _udgNewDeploymentGroupName
        , "deploymentConfigName"       .= _udgDeploymentConfigName
        , "ec2TagFilters"              .= _udgEc2TagFilters
        , "autoScalingGroups"          .= _udgAutoScalingGroups
        , "serviceRoleArn"             .= _udgServiceRoleArn
        ]

json

instance AWSRequest UpdateDeploymentGroup where
    type Sv UpdateDeploymentGroup = CodeDeploy
    type Rs UpdateDeploymentGroup = UpdateDeploymentGroupResponse

    request  = post "UpdateDeploymentGroup"
    response = jsonResponse

instance FromJSON UpdateDeploymentGroupResponse where
    parseJSON = withObject "UpdateDeploymentGroupResponse" $ \o -> UpdateDeploymentGroupResponse
        <$> o .:  "hooksNotCleanedUp"

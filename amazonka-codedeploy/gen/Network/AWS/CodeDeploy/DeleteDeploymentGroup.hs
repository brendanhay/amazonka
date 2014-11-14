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

-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a deployment group.
module Network.AWS.CodeDeploy.DeleteDeploymentGroup
    (
    -- * Request
      DeleteDeploymentGroup
    -- ** Request constructor
    , deleteDeploymentGroup
    -- ** Request lenses
    , ddgApplicationName
    , ddgDeploymentGroupName

    -- * Response
    , DeleteDeploymentGroupResponse
    -- ** Response constructor
    , deleteDeploymentGroupResponse
    -- ** Response lenses
    , ddgrHooksNotCleanedUp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CodeDeploy.Types

data DeleteDeploymentGroup = DeleteDeploymentGroup
    { _ddgApplicationName     :: Text
    , _ddgDeploymentGroupName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDeploymentGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddgApplicationName' @::@ 'Text'
--
-- * 'ddgDeploymentGroupName' @::@ 'Text'
--
deleteDeploymentGroup :: Text -- ^ 'ddgApplicationName'
                      -> Text -- ^ 'ddgDeploymentGroupName'
                      -> DeleteDeploymentGroup
deleteDeploymentGroup p1 p2 = DeleteDeploymentGroup
    { _ddgApplicationName     = p1
    , _ddgDeploymentGroupName = p2
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
ddgApplicationName :: Lens' DeleteDeploymentGroup Text
ddgApplicationName =
    lens _ddgApplicationName (\s a -> s { _ddgApplicationName = a })

-- | The name of an existing deployment group for the specified application.
ddgDeploymentGroupName :: Lens' DeleteDeploymentGroup Text
ddgDeploymentGroupName =
    lens _ddgDeploymentGroupName (\s a -> s { _ddgDeploymentGroupName = a })

instance ToPath DeleteDeploymentGroup where
    toPath = const "/"

instance ToQuery DeleteDeploymentGroup where
    toQuery = const mempty

instance ToHeaders DeleteDeploymentGroup

instance ToBody DeleteDeploymentGroup where
    toBody = toBody . encode . _ddgApplicationName

newtype DeleteDeploymentGroupResponse = DeleteDeploymentGroupResponse
    { _ddgrHooksNotCleanedUp :: [AutoScalingGroup]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DeleteDeploymentGroupResponse where
    type Item DeleteDeploymentGroupResponse = AutoScalingGroup

    fromList = DeleteDeploymentGroupResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _ddgrHooksNotCleanedUp

-- | 'DeleteDeploymentGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddgrHooksNotCleanedUp' @::@ ['AutoScalingGroup']
--
deleteDeploymentGroupResponse :: DeleteDeploymentGroupResponse
deleteDeploymentGroupResponse = DeleteDeploymentGroupResponse
    { _ddgrHooksNotCleanedUp = mempty
    }

-- | If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- instances in the Auto Scaling. If the output does contain data, AWS
-- CodeDeploy could not remove some Auto Scaling lifecycle event hooks from
-- the instances in the Auto Scaling group.
ddgrHooksNotCleanedUp :: Lens' DeleteDeploymentGroupResponse [AutoScalingGroup]
ddgrHooksNotCleanedUp =
    lens _ddgrHooksNotCleanedUp (\s a -> s { _ddgrHooksNotCleanedUp = a })

instance AWSRequest DeleteDeploymentGroup where
    type Sv DeleteDeploymentGroup = CodeDeploy
    type Rs DeleteDeploymentGroup = DeleteDeploymentGroupResponse

    request  = post
    response = jsonResponse $ \h o -> DeleteDeploymentGroupResponse
        <$> o .: "hooksNotCleanedUp"

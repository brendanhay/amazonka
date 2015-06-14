{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a deployment group.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_DeleteDeploymentGroup.html>
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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'deleteDeploymentGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddgApplicationName'
--
-- * 'ddgDeploymentGroupName'
data DeleteDeploymentGroup = DeleteDeploymentGroup'{_ddgApplicationName :: Text, _ddgDeploymentGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDeploymentGroup' smart constructor.
deleteDeploymentGroup :: Text -> Text -> DeleteDeploymentGroup
deleteDeploymentGroup pApplicationName pDeploymentGroupName = DeleteDeploymentGroup'{_ddgApplicationName = pApplicationName, _ddgDeploymentGroupName = pDeploymentGroupName};

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
ddgApplicationName :: Lens' DeleteDeploymentGroup Text
ddgApplicationName = lens _ddgApplicationName (\ s a -> s{_ddgApplicationName = a});

-- | The name of an existing deployment group for the specified application.
ddgDeploymentGroupName :: Lens' DeleteDeploymentGroup Text
ddgDeploymentGroupName = lens _ddgDeploymentGroupName (\ s a -> s{_ddgDeploymentGroupName = a});

instance AWSRequest DeleteDeploymentGroup where
        type Sv DeleteDeploymentGroup = CodeDeploy
        type Rs DeleteDeploymentGroup =
             DeleteDeploymentGroupResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteDeploymentGroupResponse' <$>
                   x .?> "hooksNotCleanedUp" .!@ mempty)

instance ToHeaders DeleteDeploymentGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.DeleteDeploymentGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDeploymentGroup where
        toJSON DeleteDeploymentGroup'{..}
          = object
              ["applicationName" .= _ddgApplicationName,
               "deploymentGroupName" .= _ddgDeploymentGroupName]

instance ToPath DeleteDeploymentGroup where
        toPath = const "/"

instance ToQuery DeleteDeploymentGroup where
        toQuery = const mempty

-- | /See:/ 'deleteDeploymentGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddgrHooksNotCleanedUp'
newtype DeleteDeploymentGroupResponse = DeleteDeploymentGroupResponse'{_ddgrHooksNotCleanedUp :: Maybe [AutoScalingGroup]} deriving (Eq, Read, Show)

-- | 'DeleteDeploymentGroupResponse' smart constructor.
deleteDeploymentGroupResponse :: DeleteDeploymentGroupResponse
deleteDeploymentGroupResponse = DeleteDeploymentGroupResponse'{_ddgrHooksNotCleanedUp = Nothing};

-- | If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- Amazon EC2 instances in the Auto Scaling. If the output does contain
-- data, AWS CodeDeploy could not remove some Auto Scaling lifecycle event
-- hooks from the Amazon EC2 instances in the Auto Scaling group.
ddgrHooksNotCleanedUp :: Lens' DeleteDeploymentGroupResponse (Maybe [AutoScalingGroup])
ddgrHooksNotCleanedUp = lens _ddgrHooksNotCleanedUp (\ s a -> s{_ddgrHooksNotCleanedUp = a});

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CodeDeploy.UpdateDeploymentGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , udgServiceRoleARN
    , udgDeploymentConfigName
    , udgEc2TagFilters
    , udgNewDeploymentGroupName
    , udgOnPremisesInstanceTagFilters
    , udgAutoScalingGroups
    , udgApplicationName
    , udgCurrentDeploymentGroupName

    -- * Response
    , UpdateDeploymentGroupResponse
    -- ** Response constructor
    , updateDeploymentGroupResponse
    -- ** Response lenses
    , udgrHooksNotCleanedUp
    , udgrStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of an update deployment group operation.
--
-- /See:/ 'updateDeploymentGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udgServiceRoleARN'
--
-- * 'udgDeploymentConfigName'
--
-- * 'udgEc2TagFilters'
--
-- * 'udgNewDeploymentGroupName'
--
-- * 'udgOnPremisesInstanceTagFilters'
--
-- * 'udgAutoScalingGroups'
--
-- * 'udgApplicationName'
--
-- * 'udgCurrentDeploymentGroupName'
data UpdateDeploymentGroup = UpdateDeploymentGroup'
    { _udgServiceRoleARN               :: !(Maybe Text)
    , _udgDeploymentConfigName         :: !(Maybe Text)
    , _udgEc2TagFilters                :: !(Maybe [EC2TagFilter])
    , _udgNewDeploymentGroupName       :: !(Maybe Text)
    , _udgOnPremisesInstanceTagFilters :: !(Maybe [TagFilter])
    , _udgAutoScalingGroups            :: !(Maybe [Text])
    , _udgApplicationName              :: !Text
    , _udgCurrentDeploymentGroupName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDeploymentGroup' smart constructor.
updateDeploymentGroup :: Text -> Text -> UpdateDeploymentGroup
updateDeploymentGroup pApplicationName pCurrentDeploymentGroupName =
    UpdateDeploymentGroup'
    { _udgServiceRoleARN = Nothing
    , _udgDeploymentConfigName = Nothing
    , _udgEc2TagFilters = Nothing
    , _udgNewDeploymentGroupName = Nothing
    , _udgOnPremisesInstanceTagFilters = Nothing
    , _udgAutoScalingGroups = Nothing
    , _udgApplicationName = pApplicationName
    , _udgCurrentDeploymentGroupName = pCurrentDeploymentGroupName
    }

-- | A replacement service role\'s ARN, if you want to change it.
udgServiceRoleARN :: Lens' UpdateDeploymentGroup (Maybe Text)
udgServiceRoleARN = lens _udgServiceRoleARN (\ s a -> s{_udgServiceRoleARN = a});

-- | The replacement deployment configuration name to use, if you want to
-- change it.
udgDeploymentConfigName :: Lens' UpdateDeploymentGroup (Maybe Text)
udgDeploymentConfigName = lens _udgDeploymentConfigName (\ s a -> s{_udgDeploymentConfigName = a});

-- | The replacement set of Amazon EC2 tags to filter on, if you want to
-- change them.
udgEc2TagFilters :: Lens' UpdateDeploymentGroup [EC2TagFilter]
udgEc2TagFilters = lens _udgEc2TagFilters (\ s a -> s{_udgEc2TagFilters = a}) . _Default;

-- | The new name of the deployment group, if you want to change it.
udgNewDeploymentGroupName :: Lens' UpdateDeploymentGroup (Maybe Text)
udgNewDeploymentGroupName = lens _udgNewDeploymentGroupName (\ s a -> s{_udgNewDeploymentGroupName = a});

-- | The replacement set of on-premises instance tags for filter on, if you
-- want to change them.
udgOnPremisesInstanceTagFilters :: Lens' UpdateDeploymentGroup [TagFilter]
udgOnPremisesInstanceTagFilters = lens _udgOnPremisesInstanceTagFilters (\ s a -> s{_udgOnPremisesInstanceTagFilters = a}) . _Default;

-- | The replacement list of Auto Scaling groups to be included in the
-- deployment group, if you want to change them.
udgAutoScalingGroups :: Lens' UpdateDeploymentGroup [Text]
udgAutoScalingGroups = lens _udgAutoScalingGroups (\ s a -> s{_udgAutoScalingGroups = a}) . _Default;

-- | The application name corresponding to the deployment group to update.
udgApplicationName :: Lens' UpdateDeploymentGroup Text
udgApplicationName = lens _udgApplicationName (\ s a -> s{_udgApplicationName = a});

-- | The current name of the existing deployment group.
udgCurrentDeploymentGroupName :: Lens' UpdateDeploymentGroup Text
udgCurrentDeploymentGroupName = lens _udgCurrentDeploymentGroupName (\ s a -> s{_udgCurrentDeploymentGroupName = a});

instance AWSRequest UpdateDeploymentGroup where
        type Sv UpdateDeploymentGroup = CodeDeploy
        type Rs UpdateDeploymentGroup =
             UpdateDeploymentGroupResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateDeploymentGroupResponse' <$>
                   (x .?> "hooksNotCleanedUp" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance ToHeaders UpdateDeploymentGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.UpdateDeploymentGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDeploymentGroup where
        toJSON UpdateDeploymentGroup'{..}
          = object
              ["serviceRoleArn" .= _udgServiceRoleARN,
               "deploymentConfigName" .= _udgDeploymentConfigName,
               "ec2TagFilters" .= _udgEc2TagFilters,
               "newDeploymentGroupName" .=
                 _udgNewDeploymentGroupName,
               "onPremisesInstanceTagFilters" .=
                 _udgOnPremisesInstanceTagFilters,
               "autoScalingGroups" .= _udgAutoScalingGroups,
               "applicationName" .= _udgApplicationName,
               "currentDeploymentGroupName" .=
                 _udgCurrentDeploymentGroupName]

instance ToPath UpdateDeploymentGroup where
        toPath = const "/"

instance ToQuery UpdateDeploymentGroup where
        toQuery = const mempty

-- | Represents the output of an update deployment group operation.
--
-- /See:/ 'updateDeploymentGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udgrHooksNotCleanedUp'
--
-- * 'udgrStatus'
data UpdateDeploymentGroupResponse = UpdateDeploymentGroupResponse'
    { _udgrHooksNotCleanedUp :: !(Maybe [AutoScalingGroup])
    , _udgrStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDeploymentGroupResponse' smart constructor.
updateDeploymentGroupResponse :: Int -> UpdateDeploymentGroupResponse
updateDeploymentGroupResponse pStatus =
    UpdateDeploymentGroupResponse'
    { _udgrHooksNotCleanedUp = Nothing
    , _udgrStatus = pStatus
    }

-- | If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- AWS account. If the output does contain data, AWS CodeDeploy could not
-- remove some Auto Scaling lifecycle event hooks from the AWS account.
udgrHooksNotCleanedUp :: Lens' UpdateDeploymentGroupResponse [AutoScalingGroup]
udgrHooksNotCleanedUp = lens _udgrHooksNotCleanedUp (\ s a -> s{_udgrHooksNotCleanedUp = a}) . _Default;

-- | FIXME: Undocumented member.
udgrStatus :: Lens' UpdateDeploymentGroupResponse Int
udgrStatus = lens _udgrStatus (\ s a -> s{_udgrStatus = a});

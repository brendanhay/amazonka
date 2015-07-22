{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.UpdateDeploymentGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Changes information about an existing deployment group.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_UpdateDeploymentGroup.html>
module Network.AWS.CodeDeploy.UpdateDeploymentGroup
    (
    -- * Request
      UpdateDeploymentGroup
    -- ** Request constructor
    , updateDeploymentGroup
    -- ** Request lenses
    , udgrqServiceRoleARN
    , udgrqDeploymentConfigName
    , udgrqEc2TagFilters
    , udgrqNewDeploymentGroupName
    , udgrqOnPremisesInstanceTagFilters
    , udgrqAutoScalingGroups
    , udgrqApplicationName
    , udgrqCurrentDeploymentGroupName

    -- * Response
    , UpdateDeploymentGroupResponse
    -- ** Response constructor
    , updateDeploymentGroupResponse
    -- ** Response lenses
    , udgrsHooksNotCleanedUp
    , udgrsStatus
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
-- * 'udgrqServiceRoleARN'
--
-- * 'udgrqDeploymentConfigName'
--
-- * 'udgrqEc2TagFilters'
--
-- * 'udgrqNewDeploymentGroupName'
--
-- * 'udgrqOnPremisesInstanceTagFilters'
--
-- * 'udgrqAutoScalingGroups'
--
-- * 'udgrqApplicationName'
--
-- * 'udgrqCurrentDeploymentGroupName'
data UpdateDeploymentGroup = UpdateDeploymentGroup'
    { _udgrqServiceRoleARN               :: !(Maybe Text)
    , _udgrqDeploymentConfigName         :: !(Maybe Text)
    , _udgrqEc2TagFilters                :: !(Maybe [EC2TagFilter])
    , _udgrqNewDeploymentGroupName       :: !(Maybe Text)
    , _udgrqOnPremisesInstanceTagFilters :: !(Maybe [TagFilter])
    , _udgrqAutoScalingGroups            :: !(Maybe [Text])
    , _udgrqApplicationName              :: !Text
    , _udgrqCurrentDeploymentGroupName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDeploymentGroup' smart constructor.
updateDeploymentGroup :: Text -> Text -> UpdateDeploymentGroup
updateDeploymentGroup pApplicationName_ pCurrentDeploymentGroupName_ =
    UpdateDeploymentGroup'
    { _udgrqServiceRoleARN = Nothing
    , _udgrqDeploymentConfigName = Nothing
    , _udgrqEc2TagFilters = Nothing
    , _udgrqNewDeploymentGroupName = Nothing
    , _udgrqOnPremisesInstanceTagFilters = Nothing
    , _udgrqAutoScalingGroups = Nothing
    , _udgrqApplicationName = pApplicationName_
    , _udgrqCurrentDeploymentGroupName = pCurrentDeploymentGroupName_
    }

-- | A replacement service role\'s ARN, if you want to change it.
udgrqServiceRoleARN :: Lens' UpdateDeploymentGroup (Maybe Text)
udgrqServiceRoleARN = lens _udgrqServiceRoleARN (\ s a -> s{_udgrqServiceRoleARN = a});

-- | The replacement deployment configuration name to use, if you want to
-- change it.
udgrqDeploymentConfigName :: Lens' UpdateDeploymentGroup (Maybe Text)
udgrqDeploymentConfigName = lens _udgrqDeploymentConfigName (\ s a -> s{_udgrqDeploymentConfigName = a});

-- | The replacement set of Amazon EC2 tags to filter on, if you want to
-- change them.
udgrqEc2TagFilters :: Lens' UpdateDeploymentGroup [EC2TagFilter]
udgrqEc2TagFilters = lens _udgrqEc2TagFilters (\ s a -> s{_udgrqEc2TagFilters = a}) . _Default;

-- | The new name of the deployment group, if you want to change it.
udgrqNewDeploymentGroupName :: Lens' UpdateDeploymentGroup (Maybe Text)
udgrqNewDeploymentGroupName = lens _udgrqNewDeploymentGroupName (\ s a -> s{_udgrqNewDeploymentGroupName = a});

-- | The replacement set of on-premises instance tags for filter on, if you
-- want to change them.
udgrqOnPremisesInstanceTagFilters :: Lens' UpdateDeploymentGroup [TagFilter]
udgrqOnPremisesInstanceTagFilters = lens _udgrqOnPremisesInstanceTagFilters (\ s a -> s{_udgrqOnPremisesInstanceTagFilters = a}) . _Default;

-- | The replacement list of Auto Scaling groups to be included in the
-- deployment group, if you want to change them.
udgrqAutoScalingGroups :: Lens' UpdateDeploymentGroup [Text]
udgrqAutoScalingGroups = lens _udgrqAutoScalingGroups (\ s a -> s{_udgrqAutoScalingGroups = a}) . _Default;

-- | The application name corresponding to the deployment group to update.
udgrqApplicationName :: Lens' UpdateDeploymentGroup Text
udgrqApplicationName = lens _udgrqApplicationName (\ s a -> s{_udgrqApplicationName = a});

-- | The current name of the existing deployment group.
udgrqCurrentDeploymentGroupName :: Lens' UpdateDeploymentGroup Text
udgrqCurrentDeploymentGroupName = lens _udgrqCurrentDeploymentGroupName (\ s a -> s{_udgrqCurrentDeploymentGroupName = a});

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
              ["serviceRoleArn" .= _udgrqServiceRoleARN,
               "deploymentConfigName" .= _udgrqDeploymentConfigName,
               "ec2TagFilters" .= _udgrqEc2TagFilters,
               "newDeploymentGroupName" .=
                 _udgrqNewDeploymentGroupName,
               "onPremisesInstanceTagFilters" .=
                 _udgrqOnPremisesInstanceTagFilters,
               "autoScalingGroups" .= _udgrqAutoScalingGroups,
               "applicationName" .= _udgrqApplicationName,
               "currentDeploymentGroupName" .=
                 _udgrqCurrentDeploymentGroupName]

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
-- * 'udgrsHooksNotCleanedUp'
--
-- * 'udgrsStatus'
data UpdateDeploymentGroupResponse = UpdateDeploymentGroupResponse'
    { _udgrsHooksNotCleanedUp :: !(Maybe [AutoScalingGroup])
    , _udgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateDeploymentGroupResponse' smart constructor.
updateDeploymentGroupResponse :: Int -> UpdateDeploymentGroupResponse
updateDeploymentGroupResponse pStatus_ =
    UpdateDeploymentGroupResponse'
    { _udgrsHooksNotCleanedUp = Nothing
    , _udgrsStatus = pStatus_
    }

-- | If the output contains no data, and the corresponding deployment group
-- contained at least one Auto Scaling group, AWS CodeDeploy successfully
-- removed all corresponding Auto Scaling lifecycle event hooks from the
-- AWS account. If the output does contain data, AWS CodeDeploy could not
-- remove some Auto Scaling lifecycle event hooks from the AWS account.
udgrsHooksNotCleanedUp :: Lens' UpdateDeploymentGroupResponse [AutoScalingGroup]
udgrsHooksNotCleanedUp = lens _udgrsHooksNotCleanedUp (\ s a -> s{_udgrsHooksNotCleanedUp = a}) . _Default;

-- | FIXME: Undocumented member.
udgrsStatus :: Lens' UpdateDeploymentGroupResponse Int
udgrsStatus = lens _udgrsStatus (\ s a -> s{_udgrsStatus = a});

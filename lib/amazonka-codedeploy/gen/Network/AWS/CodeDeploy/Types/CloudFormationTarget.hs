{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.CloudFormationTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.CloudFormationTarget where

import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the target to be updated by an AWS CloudFormation blue/green deployment. This target type is used for all deployments initiated by a CloudFormation stack update.
--
--
--
-- /See:/ 'cloudFormationTarget' smart constructor.
data CloudFormationTarget = CloudFormationTarget'
  { _cftTargetId ::
      !(Maybe Text),
    _cftStatus :: !(Maybe TargetStatus),
    _cftDeploymentId :: !(Maybe Text),
    _cftResourceType :: !(Maybe Text),
    _cftLastUpdatedAt :: !(Maybe POSIX),
    _cftLifecycleEvents :: !(Maybe [LifecycleEvent]),
    _cftTargetVersionWeight :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudFormationTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cftTargetId' - The unique ID of a deployment target that has a type of @CloudFormationTarget@ .
--
-- * 'cftStatus' - The status of an AWS CloudFormation blue/green deployment's target application.
--
-- * 'cftDeploymentId' - The unique ID of an AWS CloudFormation blue/green deployment.
--
-- * 'cftResourceType' - The resource type for the AWS CloudFormation blue/green deployment.
--
-- * 'cftLastUpdatedAt' - The date and time when the target application was updated by an AWS CloudFormation blue/green deployment.
--
-- * 'cftLifecycleEvents' - The lifecycle events of the AWS CloudFormation blue/green deployment to this target application.
--
-- * 'cftTargetVersionWeight' - The percentage of production traffic that the target version of an AWS CloudFormation blue/green deployment receives.
cloudFormationTarget ::
  CloudFormationTarget
cloudFormationTarget =
  CloudFormationTarget'
    { _cftTargetId = Nothing,
      _cftStatus = Nothing,
      _cftDeploymentId = Nothing,
      _cftResourceType = Nothing,
      _cftLastUpdatedAt = Nothing,
      _cftLifecycleEvents = Nothing,
      _cftTargetVersionWeight = Nothing
    }

-- | The unique ID of a deployment target that has a type of @CloudFormationTarget@ .
cftTargetId :: Lens' CloudFormationTarget (Maybe Text)
cftTargetId = lens _cftTargetId (\s a -> s {_cftTargetId = a})

-- | The status of an AWS CloudFormation blue/green deployment's target application.
cftStatus :: Lens' CloudFormationTarget (Maybe TargetStatus)
cftStatus = lens _cftStatus (\s a -> s {_cftStatus = a})

-- | The unique ID of an AWS CloudFormation blue/green deployment.
cftDeploymentId :: Lens' CloudFormationTarget (Maybe Text)
cftDeploymentId = lens _cftDeploymentId (\s a -> s {_cftDeploymentId = a})

-- | The resource type for the AWS CloudFormation blue/green deployment.
cftResourceType :: Lens' CloudFormationTarget (Maybe Text)
cftResourceType = lens _cftResourceType (\s a -> s {_cftResourceType = a})

-- | The date and time when the target application was updated by an AWS CloudFormation blue/green deployment.
cftLastUpdatedAt :: Lens' CloudFormationTarget (Maybe UTCTime)
cftLastUpdatedAt = lens _cftLastUpdatedAt (\s a -> s {_cftLastUpdatedAt = a}) . mapping _Time

-- | The lifecycle events of the AWS CloudFormation blue/green deployment to this target application.
cftLifecycleEvents :: Lens' CloudFormationTarget [LifecycleEvent]
cftLifecycleEvents = lens _cftLifecycleEvents (\s a -> s {_cftLifecycleEvents = a}) . _Default . _Coerce

-- | The percentage of production traffic that the target version of an AWS CloudFormation blue/green deployment receives.
cftTargetVersionWeight :: Lens' CloudFormationTarget (Maybe Double)
cftTargetVersionWeight = lens _cftTargetVersionWeight (\s a -> s {_cftTargetVersionWeight = a})

instance FromJSON CloudFormationTarget where
  parseJSON =
    withObject
      "CloudFormationTarget"
      ( \x ->
          CloudFormationTarget'
            <$> (x .:? "targetId")
            <*> (x .:? "status")
            <*> (x .:? "deploymentId")
            <*> (x .:? "resourceType")
            <*> (x .:? "lastUpdatedAt")
            <*> (x .:? "lifecycleEvents" .!= mempty)
            <*> (x .:? "targetVersionWeight")
      )

instance Hashable CloudFormationTarget

instance NFData CloudFormationTarget

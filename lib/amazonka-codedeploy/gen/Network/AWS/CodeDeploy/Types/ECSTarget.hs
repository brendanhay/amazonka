{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ECSTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ECSTarget where

import Network.AWS.CodeDeploy.Types.ECSTaskSet
import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the target of an Amazon ECS deployment.
--
--
--
-- /See:/ 'eCSTarget' smart constructor.
data ECSTarget = ECSTarget'
  { _ecstTargetARN :: !(Maybe Text),
    _ecstTargetId :: !(Maybe Text),
    _ecstStatus :: !(Maybe TargetStatus),
    _ecstDeploymentId :: !(Maybe Text),
    _ecstLastUpdatedAt :: !(Maybe POSIX),
    _ecstTaskSetsInfo :: !(Maybe [ECSTaskSet]),
    _ecstLifecycleEvents :: !(Maybe [LifecycleEvent])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ECSTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecstTargetARN' - The Amazon Resource Name (ARN) of the target.
--
-- * 'ecstTargetId' - The unique ID of a deployment target that has a type of @ecsTarget@ .
--
-- * 'ecstStatus' - The status an Amazon ECS deployment's target ECS application.
--
-- * 'ecstDeploymentId' - The unique ID of a deployment.
--
-- * 'ecstLastUpdatedAt' - The date and time when the target Amazon ECS application was updated by a deployment.
--
-- * 'ecstTaskSetsInfo' - The @ECSTaskSet@ objects associated with the ECS target.
--
-- * 'ecstLifecycleEvents' - The lifecycle events of the deployment to this target Amazon ECS application.
eCSTarget ::
  ECSTarget
eCSTarget =
  ECSTarget'
    { _ecstTargetARN = Nothing,
      _ecstTargetId = Nothing,
      _ecstStatus = Nothing,
      _ecstDeploymentId = Nothing,
      _ecstLastUpdatedAt = Nothing,
      _ecstTaskSetsInfo = Nothing,
      _ecstLifecycleEvents = Nothing
    }

-- | The Amazon Resource Name (ARN) of the target.
ecstTargetARN :: Lens' ECSTarget (Maybe Text)
ecstTargetARN = lens _ecstTargetARN (\s a -> s {_ecstTargetARN = a})

-- | The unique ID of a deployment target that has a type of @ecsTarget@ .
ecstTargetId :: Lens' ECSTarget (Maybe Text)
ecstTargetId = lens _ecstTargetId (\s a -> s {_ecstTargetId = a})

-- | The status an Amazon ECS deployment's target ECS application.
ecstStatus :: Lens' ECSTarget (Maybe TargetStatus)
ecstStatus = lens _ecstStatus (\s a -> s {_ecstStatus = a})

-- | The unique ID of a deployment.
ecstDeploymentId :: Lens' ECSTarget (Maybe Text)
ecstDeploymentId = lens _ecstDeploymentId (\s a -> s {_ecstDeploymentId = a})

-- | The date and time when the target Amazon ECS application was updated by a deployment.
ecstLastUpdatedAt :: Lens' ECSTarget (Maybe UTCTime)
ecstLastUpdatedAt = lens _ecstLastUpdatedAt (\s a -> s {_ecstLastUpdatedAt = a}) . mapping _Time

-- | The @ECSTaskSet@ objects associated with the ECS target.
ecstTaskSetsInfo :: Lens' ECSTarget [ECSTaskSet]
ecstTaskSetsInfo = lens _ecstTaskSetsInfo (\s a -> s {_ecstTaskSetsInfo = a}) . _Default . _Coerce

-- | The lifecycle events of the deployment to this target Amazon ECS application.
ecstLifecycleEvents :: Lens' ECSTarget [LifecycleEvent]
ecstLifecycleEvents = lens _ecstLifecycleEvents (\s a -> s {_ecstLifecycleEvents = a}) . _Default . _Coerce

instance FromJSON ECSTarget where
  parseJSON =
    withObject
      "ECSTarget"
      ( \x ->
          ECSTarget'
            <$> (x .:? "targetArn")
            <*> (x .:? "targetId")
            <*> (x .:? "status")
            <*> (x .:? "deploymentId")
            <*> (x .:? "lastUpdatedAt")
            <*> (x .:? "taskSetsInfo" .!= mempty)
            <*> (x .:? "lifecycleEvents" .!= mempty)
      )

instance Hashable ECSTarget

instance NFData ECSTarget

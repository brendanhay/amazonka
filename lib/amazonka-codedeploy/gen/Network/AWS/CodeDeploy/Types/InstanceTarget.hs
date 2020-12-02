{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.InstanceTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.InstanceTarget where

import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetLabel
import Network.AWS.CodeDeploy.Types.TargetStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A target Amazon EC2 or on-premises instance during a deployment that uses the EC2/On-premises compute platform.
--
--
--
-- /See:/ 'instanceTarget' smart constructor.
data InstanceTarget = InstanceTarget'
  { _itTargetARN ::
      !(Maybe Text),
    _itTargetId :: !(Maybe Text),
    _itStatus :: !(Maybe TargetStatus),
    _itDeploymentId :: !(Maybe Text),
    _itInstanceLabel :: !(Maybe TargetLabel),
    _itLastUpdatedAt :: !(Maybe POSIX),
    _itLifecycleEvents :: !(Maybe [LifecycleEvent])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itTargetARN' - The Amazon Resource Name (ARN) of the target.
--
-- * 'itTargetId' - The unique ID of a deployment target that has a type of @instanceTarget@ .
--
-- * 'itStatus' - The status an EC2/On-premises deployment's target instance.
--
-- * 'itDeploymentId' - The unique ID of a deployment.
--
-- * 'itInstanceLabel' - A label that identifies whether the instance is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
--
-- * 'itLastUpdatedAt' - The date and time when the target instance was updated by a deployment.
--
-- * 'itLifecycleEvents' - The lifecycle events of the deployment to this target instance.
instanceTarget ::
  InstanceTarget
instanceTarget =
  InstanceTarget'
    { _itTargetARN = Nothing,
      _itTargetId = Nothing,
      _itStatus = Nothing,
      _itDeploymentId = Nothing,
      _itInstanceLabel = Nothing,
      _itLastUpdatedAt = Nothing,
      _itLifecycleEvents = Nothing
    }

-- | The Amazon Resource Name (ARN) of the target.
itTargetARN :: Lens' InstanceTarget (Maybe Text)
itTargetARN = lens _itTargetARN (\s a -> s {_itTargetARN = a})

-- | The unique ID of a deployment target that has a type of @instanceTarget@ .
itTargetId :: Lens' InstanceTarget (Maybe Text)
itTargetId = lens _itTargetId (\s a -> s {_itTargetId = a})

-- | The status an EC2/On-premises deployment's target instance.
itStatus :: Lens' InstanceTarget (Maybe TargetStatus)
itStatus = lens _itStatus (\s a -> s {_itStatus = a})

-- | The unique ID of a deployment.
itDeploymentId :: Lens' InstanceTarget (Maybe Text)
itDeploymentId = lens _itDeploymentId (\s a -> s {_itDeploymentId = a})

-- | A label that identifies whether the instance is an original target (@BLUE@ ) or a replacement target (@GREEN@ ).
itInstanceLabel :: Lens' InstanceTarget (Maybe TargetLabel)
itInstanceLabel = lens _itInstanceLabel (\s a -> s {_itInstanceLabel = a})

-- | The date and time when the target instance was updated by a deployment.
itLastUpdatedAt :: Lens' InstanceTarget (Maybe UTCTime)
itLastUpdatedAt = lens _itLastUpdatedAt (\s a -> s {_itLastUpdatedAt = a}) . mapping _Time

-- | The lifecycle events of the deployment to this target instance.
itLifecycleEvents :: Lens' InstanceTarget [LifecycleEvent]
itLifecycleEvents = lens _itLifecycleEvents (\s a -> s {_itLifecycleEvents = a}) . _Default . _Coerce

instance FromJSON InstanceTarget where
  parseJSON =
    withObject
      "InstanceTarget"
      ( \x ->
          InstanceTarget'
            <$> (x .:? "targetArn")
            <*> (x .:? "targetId")
            <*> (x .:? "status")
            <*> (x .:? "deploymentId")
            <*> (x .:? "instanceLabel")
            <*> (x .:? "lastUpdatedAt")
            <*> (x .:? "lifecycleEvents" .!= mempty)
      )

instance Hashable InstanceTarget

instance NFData InstanceTarget

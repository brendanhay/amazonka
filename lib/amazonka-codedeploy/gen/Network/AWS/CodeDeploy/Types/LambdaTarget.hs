{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LambdaTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LambdaTarget where

import Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
import Network.AWS.CodeDeploy.Types.LifecycleEvent
import Network.AWS.CodeDeploy.Types.TargetStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the target AWS Lambda function during an AWS Lambda deployment.
--
--
--
-- /See:/ 'lambdaTarget' smart constructor.
data LambdaTarget = LambdaTarget'
  { _ltTargetARN :: !(Maybe Text),
    _ltTargetId :: !(Maybe Text),
    _ltStatus :: !(Maybe TargetStatus),
    _ltDeploymentId :: !(Maybe Text),
    _ltLastUpdatedAt :: !(Maybe POSIX),
    _ltLifecycleEvents :: !(Maybe [LifecycleEvent]),
    _ltLambdaFunctionInfo :: !(Maybe LambdaFunctionInfo)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltTargetARN' - The Amazon Resource Name (ARN) of the target.
--
-- * 'ltTargetId' - The unique ID of a deployment target that has a type of @lambdaTarget@ .
--
-- * 'ltStatus' - The status an AWS Lambda deployment's target Lambda function.
--
-- * 'ltDeploymentId' - The unique ID of a deployment.
--
-- * 'ltLastUpdatedAt' - The date and time when the target Lambda function was updated by a deployment.
--
-- * 'ltLifecycleEvents' - The lifecycle events of the deployment to this target Lambda function.
--
-- * 'ltLambdaFunctionInfo' - A @LambdaFunctionInfo@ object that describes a target Lambda function.
lambdaTarget ::
  LambdaTarget
lambdaTarget =
  LambdaTarget'
    { _ltTargetARN = Nothing,
      _ltTargetId = Nothing,
      _ltStatus = Nothing,
      _ltDeploymentId = Nothing,
      _ltLastUpdatedAt = Nothing,
      _ltLifecycleEvents = Nothing,
      _ltLambdaFunctionInfo = Nothing
    }

-- | The Amazon Resource Name (ARN) of the target.
ltTargetARN :: Lens' LambdaTarget (Maybe Text)
ltTargetARN = lens _ltTargetARN (\s a -> s {_ltTargetARN = a})

-- | The unique ID of a deployment target that has a type of @lambdaTarget@ .
ltTargetId :: Lens' LambdaTarget (Maybe Text)
ltTargetId = lens _ltTargetId (\s a -> s {_ltTargetId = a})

-- | The status an AWS Lambda deployment's target Lambda function.
ltStatus :: Lens' LambdaTarget (Maybe TargetStatus)
ltStatus = lens _ltStatus (\s a -> s {_ltStatus = a})

-- | The unique ID of a deployment.
ltDeploymentId :: Lens' LambdaTarget (Maybe Text)
ltDeploymentId = lens _ltDeploymentId (\s a -> s {_ltDeploymentId = a})

-- | The date and time when the target Lambda function was updated by a deployment.
ltLastUpdatedAt :: Lens' LambdaTarget (Maybe UTCTime)
ltLastUpdatedAt = lens _ltLastUpdatedAt (\s a -> s {_ltLastUpdatedAt = a}) . mapping _Time

-- | The lifecycle events of the deployment to this target Lambda function.
ltLifecycleEvents :: Lens' LambdaTarget [LifecycleEvent]
ltLifecycleEvents = lens _ltLifecycleEvents (\s a -> s {_ltLifecycleEvents = a}) . _Default . _Coerce

-- | A @LambdaFunctionInfo@ object that describes a target Lambda function.
ltLambdaFunctionInfo :: Lens' LambdaTarget (Maybe LambdaFunctionInfo)
ltLambdaFunctionInfo = lens _ltLambdaFunctionInfo (\s a -> s {_ltLambdaFunctionInfo = a})

instance FromJSON LambdaTarget where
  parseJSON =
    withObject
      "LambdaTarget"
      ( \x ->
          LambdaTarget'
            <$> (x .:? "targetArn")
            <*> (x .:? "targetId")
            <*> (x .:? "status")
            <*> (x .:? "deploymentId")
            <*> (x .:? "lastUpdatedAt")
            <*> (x .:? "lifecycleEvents" .!= mempty)
            <*> (x .:? "lambdaFunctionInfo")
      )

instance Hashable LambdaTarget

instance NFData LambdaTarget

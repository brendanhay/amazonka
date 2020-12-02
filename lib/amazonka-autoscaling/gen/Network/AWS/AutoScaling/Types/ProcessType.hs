{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ProcessType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ProcessType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a process type.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-suspend-resume-processes.html#process-types Scaling processes> in the /Amazon EC2 Auto Scaling User Guide/ .
--
--
-- /See:/ 'processType' smart constructor.
newtype ProcessType = ProcessType' {_ptProcessName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptProcessName' - One of the following processes:     * @Launch@      * @Terminate@      * @AddToLoadBalancer@      * @AlarmNotification@      * @AZRebalance@      * @HealthCheck@      * @InstanceRefresh@      * @ReplaceUnhealthy@      * @ScheduledActions@
processType ::
  -- | 'ptProcessName'
  Text ->
  ProcessType
processType pProcessName_ =
  ProcessType' {_ptProcessName = pProcessName_}

-- | One of the following processes:     * @Launch@      * @Terminate@      * @AddToLoadBalancer@      * @AlarmNotification@      * @AZRebalance@      * @HealthCheck@      * @InstanceRefresh@      * @ReplaceUnhealthy@      * @ScheduledActions@
ptProcessName :: Lens' ProcessType Text
ptProcessName = lens _ptProcessName (\s a -> s {_ptProcessName = a})

instance FromXML ProcessType where
  parseXML x = ProcessType' <$> (x .@ "ProcessName")

instance Hashable ProcessType

instance NFData ProcessType

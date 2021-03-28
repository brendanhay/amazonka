{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CloudwatchLogsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CloudwatchLogsAction
  ( CloudwatchLogsAction (..)
  -- * Smart constructor
  , mkCloudwatchLogsAction
  -- * Lenses
  , claRoleArn
  , claLogGroupName
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.LogGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action that sends data to CloudWatch Logs.
--
-- /See:/ 'mkCloudwatchLogsAction' smart constructor.
data CloudwatchLogsAction = CloudwatchLogsAction'
  { roleArn :: Types.AwsArn
    -- ^ The IAM role that allows access to the CloudWatch log.
  , logGroupName :: Types.LogGroupName
    -- ^ The CloudWatch log group to which the action sends data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudwatchLogsAction' value with any optional fields omitted.
mkCloudwatchLogsAction
    :: Types.AwsArn -- ^ 'roleArn'
    -> Types.LogGroupName -- ^ 'logGroupName'
    -> CloudwatchLogsAction
mkCloudwatchLogsAction roleArn logGroupName
  = CloudwatchLogsAction'{roleArn, logGroupName}

-- | The IAM role that allows access to the CloudWatch log.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claRoleArn :: Lens.Lens' CloudwatchLogsAction Types.AwsArn
claRoleArn = Lens.field @"roleArn"
{-# INLINEABLE claRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The CloudWatch log group to which the action sends data.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
claLogGroupName :: Lens.Lens' CloudwatchLogsAction Types.LogGroupName
claLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE claLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

instance Core.FromJSON CloudwatchLogsAction where
        toJSON CloudwatchLogsAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("logGroupName" Core..= logGroupName)])

instance Core.FromJSON CloudwatchLogsAction where
        parseJSON
          = Core.withObject "CloudwatchLogsAction" Core.$
              \ x ->
                CloudwatchLogsAction' Core.<$>
                  (x Core..: "roleArn") Core.<*> x Core..: "logGroupName"

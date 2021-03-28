{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.Action
  ( Action (..)
  -- * Smart constructor
  , mkAction
  -- * Lenses
  , aArguments
  , aCrawlerName
  , aJobName
  , aNotificationProperty
  , aSecurityConfiguration
  , aTimeout
  ) where

import qualified Network.AWS.Glue.Types.CrawlerName as Types
import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Glue.Types.JobName as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.NotificationProperty as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines an action to be initiated by a trigger.
--
-- /See:/ 'mkAction' smart constructor.
data Action = Action'
  { arguments :: Core.Maybe (Core.HashMap Types.GenericString Types.GenericString)
    -- ^ The job arguments used when this trigger fires. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
  , crawlerName :: Core.Maybe Types.CrawlerName
    -- ^ The name of the crawler to be used with this action.
  , jobName :: Core.Maybe Types.JobName
    -- ^ The name of a job to be executed.
  , notificationProperty :: Core.Maybe Types.NotificationProperty
    -- ^ Specifies configuration properties of a job run notification.
  , securityConfiguration :: Core.Maybe Types.NameString
    -- ^ The name of the @SecurityConfiguration@ structure to be used with this action.
  , timeout :: Core.Maybe Core.Natural
    -- ^ The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Action' value with any optional fields omitted.
mkAction
    :: Action
mkAction
  = Action'{arguments = Core.Nothing, crawlerName = Core.Nothing,
            jobName = Core.Nothing, notificationProperty = Core.Nothing,
            securityConfiguration = Core.Nothing, timeout = Core.Nothing}

-- | The job arguments used when this trigger fires. For this job run, they replace the default arguments set in the job definition itself.
--
-- You can specify arguments here that your own job-execution script consumes, as well as arguments that AWS Glue itself consumes.
-- For information about how to specify and consume your own Job arguments, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-python-calling.html Calling AWS Glue APIs in Python> topic in the developer guide.
-- For information about the key-value pairs that AWS Glue consumes to set up your job, see the <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-programming-etl-glue-arguments.html Special Parameters Used by AWS Glue> topic in the developer guide.
--
-- /Note:/ Consider using 'arguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aArguments :: Lens.Lens' Action (Core.Maybe (Core.HashMap Types.GenericString Types.GenericString))
aArguments = Lens.field @"arguments"
{-# INLINEABLE aArguments #-}
{-# DEPRECATED arguments "Use generic-lens or generic-optics with 'arguments' instead"  #-}

-- | The name of the crawler to be used with this action.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCrawlerName :: Lens.Lens' Action (Core.Maybe Types.CrawlerName)
aCrawlerName = Lens.field @"crawlerName"
{-# INLINEABLE aCrawlerName #-}
{-# DEPRECATED crawlerName "Use generic-lens or generic-optics with 'crawlerName' instead"  #-}

-- | The name of a job to be executed.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aJobName :: Lens.Lens' Action (Core.Maybe Types.JobName)
aJobName = Lens.field @"jobName"
{-# INLINEABLE aJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | Specifies configuration properties of a job run notification.
--
-- /Note:/ Consider using 'notificationProperty' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNotificationProperty :: Lens.Lens' Action (Core.Maybe Types.NotificationProperty)
aNotificationProperty = Lens.field @"notificationProperty"
{-# INLINEABLE aNotificationProperty #-}
{-# DEPRECATED notificationProperty "Use generic-lens or generic-optics with 'notificationProperty' instead"  #-}

-- | The name of the @SecurityConfiguration@ structure to be used with this action.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSecurityConfiguration :: Lens.Lens' Action (Core.Maybe Types.NameString)
aSecurityConfiguration = Lens.field @"securityConfiguration"
{-# INLINEABLE aSecurityConfiguration #-}
{-# DEPRECATED securityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead"  #-}

-- | The @JobRun@ timeout in minutes. This is the maximum time that a job run can consume resources before it is terminated and enters @TIMEOUT@ status. The default is 2,880 minutes (48 hours). This overrides the timeout value set in the parent job.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTimeout :: Lens.Lens' Action (Core.Maybe Core.Natural)
aTimeout = Lens.field @"timeout"
{-# INLINEABLE aTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

instance Core.FromJSON Action where
        toJSON Action{..}
          = Core.object
              (Core.catMaybes
                 [("Arguments" Core..=) Core.<$> arguments,
                  ("CrawlerName" Core..=) Core.<$> crawlerName,
                  ("JobName" Core..=) Core.<$> jobName,
                  ("NotificationProperty" Core..=) Core.<$> notificationProperty,
                  ("SecurityConfiguration" Core..=) Core.<$> securityConfiguration,
                  ("Timeout" Core..=) Core.<$> timeout])

instance Core.FromJSON Action where
        parseJSON
          = Core.withObject "Action" Core.$
              \ x ->
                Action' Core.<$>
                  (x Core..:? "Arguments") Core.<*> x Core..:? "CrawlerName" Core.<*>
                    x Core..:? "JobName"
                    Core.<*> x Core..:? "NotificationProperty"
                    Core.<*> x Core..:? "SecurityConfiguration"
                    Core.<*> x Core..:? "Timeout"

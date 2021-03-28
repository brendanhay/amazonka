{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.LogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.LogGroup
  ( LogGroup (..)
  -- * Smart constructor
  , mkLogGroup
  -- * Lenses
  , lgArn
  , lgCreationTime
  , lgKmsKeyId
  , lgLogGroupName
  , lgMetricFilterCount
  , lgRetentionInDays
  , lgStoredBytes
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.Arn as Types
import qualified Network.AWS.CloudWatchLogs.Types.KmsKeyId as Types
import qualified Network.AWS.CloudWatchLogs.Types.LogGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a log group.
--
-- /See:/ 'mkLogGroup' smart constructor.
data LogGroup = LogGroup'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the log group.
  , creationTime :: Core.Maybe Core.Natural
    -- ^ The creation time of the log group, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
  , kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The Amazon Resource Name (ARN) of the CMK to use when encrypting log data.
  , logGroupName :: Core.Maybe Types.LogGroupName
    -- ^ The name of the log group.
  , metricFilterCount :: Core.Maybe Core.Int
    -- ^ The number of metric filters.
  , retentionInDays :: Core.Maybe Core.Int
  , storedBytes :: Core.Maybe Core.Natural
    -- ^ The number of bytes stored.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogGroup' value with any optional fields omitted.
mkLogGroup
    :: LogGroup
mkLogGroup
  = LogGroup'{arn = Core.Nothing, creationTime = Core.Nothing,
              kmsKeyId = Core.Nothing, logGroupName = Core.Nothing,
              metricFilterCount = Core.Nothing, retentionInDays = Core.Nothing,
              storedBytes = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the log group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgArn :: Lens.Lens' LogGroup (Core.Maybe Types.Arn)
lgArn = Lens.field @"arn"
{-# INLINEABLE lgArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The creation time of the log group, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgCreationTime :: Lens.Lens' LogGroup (Core.Maybe Core.Natural)
lgCreationTime = Lens.field @"creationTime"
{-# INLINEABLE lgCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgKmsKeyId :: Lens.Lens' LogGroup (Core.Maybe Types.KmsKeyId)
lgKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE lgKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgLogGroupName :: Lens.Lens' LogGroup (Core.Maybe Types.LogGroupName)
lgLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE lgLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

-- | The number of metric filters.
--
-- /Note:/ Consider using 'metricFilterCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMetricFilterCount :: Lens.Lens' LogGroup (Core.Maybe Core.Int)
lgMetricFilterCount = Lens.field @"metricFilterCount"
{-# INLINEABLE lgMetricFilterCount #-}
{-# DEPRECATED metricFilterCount "Use generic-lens or generic-optics with 'metricFilterCount' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'retentionInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgRetentionInDays :: Lens.Lens' LogGroup (Core.Maybe Core.Int)
lgRetentionInDays = Lens.field @"retentionInDays"
{-# INLINEABLE lgRetentionInDays #-}
{-# DEPRECATED retentionInDays "Use generic-lens or generic-optics with 'retentionInDays' instead"  #-}

-- | The number of bytes stored.
--
-- /Note:/ Consider using 'storedBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgStoredBytes :: Lens.Lens' LogGroup (Core.Maybe Core.Natural)
lgStoredBytes = Lens.field @"storedBytes"
{-# INLINEABLE lgStoredBytes #-}
{-# DEPRECATED storedBytes "Use generic-lens or generic-optics with 'storedBytes' instead"  #-}

instance Core.FromJSON LogGroup where
        parseJSON
          = Core.withObject "LogGroup" Core.$
              \ x ->
                LogGroup' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "creationTime" Core.<*>
                    x Core..:? "kmsKeyId"
                    Core.<*> x Core..:? "logGroupName"
                    Core.<*> x Core..:? "metricFilterCount"
                    Core.<*> x Core..:? "retentionInDays"
                    Core.<*> x Core..:? "storedBytes"

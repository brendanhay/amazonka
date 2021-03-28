{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackDetail
  ( AttackDetail (..)
  -- * Smart constructor
  , mkAttackDetail
  -- * Lenses
  , adAttackCounters
  , adAttackId
  , adAttackProperties
  , adEndTime
  , adMitigations
  , adResourceArn
  , adStartTime
  , adSubResources
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.AttackId as Types
import qualified Network.AWS.Shield.Types.AttackProperty as Types
import qualified Network.AWS.Shield.Types.Mitigation as Types
import qualified Network.AWS.Shield.Types.ResourceArn as Types
import qualified Network.AWS.Shield.Types.SubResourceSummary as Types
import qualified Network.AWS.Shield.Types.SummarizedCounter as Types

-- | The details of a DDoS attack.
--
-- /See:/ 'mkAttackDetail' smart constructor.
data AttackDetail = AttackDetail'
  { attackCounters :: Core.Maybe [Types.SummarizedCounter]
    -- ^ List of counters that describe the attack for the specified time period.
  , attackId :: Core.Maybe Types.AttackId
    -- ^ The unique identifier (ID) of the attack.
  , attackProperties :: Core.Maybe [Types.AttackProperty]
    -- ^ The array of 'AttackProperty' objects.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the attack ended, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
  , mitigations :: Core.Maybe [Types.Mitigation]
    -- ^ List of mitigation actions taken for the attack.
  , resourceArn :: Core.Maybe Types.ResourceArn
    -- ^ The ARN (Amazon Resource Name) of the resource that was attacked.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the attack started, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
  , subResources :: Core.Maybe [Types.SubResourceSummary]
    -- ^ If applicable, additional detail about the resource being attacked, for example, IP address or URL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AttackDetail' value with any optional fields omitted.
mkAttackDetail
    :: AttackDetail
mkAttackDetail
  = AttackDetail'{attackCounters = Core.Nothing,
                  attackId = Core.Nothing, attackProperties = Core.Nothing,
                  endTime = Core.Nothing, mitigations = Core.Nothing,
                  resourceArn = Core.Nothing, startTime = Core.Nothing,
                  subResources = Core.Nothing}

-- | List of counters that describe the attack for the specified time period.
--
-- /Note:/ Consider using 'attackCounters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttackCounters :: Lens.Lens' AttackDetail (Core.Maybe [Types.SummarizedCounter])
adAttackCounters = Lens.field @"attackCounters"
{-# INLINEABLE adAttackCounters #-}
{-# DEPRECATED attackCounters "Use generic-lens or generic-optics with 'attackCounters' instead"  #-}

-- | The unique identifier (ID) of the attack.
--
-- /Note:/ Consider using 'attackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttackId :: Lens.Lens' AttackDetail (Core.Maybe Types.AttackId)
adAttackId = Lens.field @"attackId"
{-# INLINEABLE adAttackId #-}
{-# DEPRECATED attackId "Use generic-lens or generic-optics with 'attackId' instead"  #-}

-- | The array of 'AttackProperty' objects.
--
-- /Note:/ Consider using 'attackProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAttackProperties :: Lens.Lens' AttackDetail (Core.Maybe [Types.AttackProperty])
adAttackProperties = Lens.field @"attackProperties"
{-# INLINEABLE adAttackProperties #-}
{-# DEPRECATED attackProperties "Use generic-lens or generic-optics with 'attackProperties' instead"  #-}

-- | The time the attack ended, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adEndTime :: Lens.Lens' AttackDetail (Core.Maybe Core.NominalDiffTime)
adEndTime = Lens.field @"endTime"
{-# INLINEABLE adEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | List of mitigation actions taken for the attack.
--
-- /Note:/ Consider using 'mitigations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adMitigations :: Lens.Lens' AttackDetail (Core.Maybe [Types.Mitigation])
adMitigations = Lens.field @"mitigations"
{-# INLINEABLE adMitigations #-}
{-# DEPRECATED mitigations "Use generic-lens or generic-optics with 'mitigations' instead"  #-}

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adResourceArn :: Lens.Lens' AttackDetail (Core.Maybe Types.ResourceArn)
adResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE adResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The time the attack started, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adStartTime :: Lens.Lens' AttackDetail (Core.Maybe Core.NominalDiffTime)
adStartTime = Lens.field @"startTime"
{-# INLINEABLE adStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | If applicable, additional detail about the resource being attacked, for example, IP address or URL.
--
-- /Note:/ Consider using 'subResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adSubResources :: Lens.Lens' AttackDetail (Core.Maybe [Types.SubResourceSummary])
adSubResources = Lens.field @"subResources"
{-# INLINEABLE adSubResources #-}
{-# DEPRECATED subResources "Use generic-lens or generic-optics with 'subResources' instead"  #-}

instance Core.FromJSON AttackDetail where
        parseJSON
          = Core.withObject "AttackDetail" Core.$
              \ x ->
                AttackDetail' Core.<$>
                  (x Core..:? "AttackCounters") Core.<*> x Core..:? "AttackId"
                    Core.<*> x Core..:? "AttackProperties"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "Mitigations"
                    Core.<*> x Core..:? "ResourceArn"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "SubResources"

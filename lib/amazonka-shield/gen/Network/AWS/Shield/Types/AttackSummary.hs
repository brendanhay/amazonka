{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackSummary
  ( AttackSummary (..)
  -- * Smart constructor
  , mkAttackSummary
  -- * Lenses
  , asAttackId
  , asAttackVectors
  , asEndTime
  , asResourceArn
  , asStartTime
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.AttackVectorDescription as Types

-- | Summarizes all DDoS attacks for a specified time period.
--
-- /See:/ 'mkAttackSummary' smart constructor.
data AttackSummary = AttackSummary'
  { attackId :: Core.Maybe Core.Text
    -- ^ The unique identifier (ID) of the attack.
  , attackVectors :: Core.Maybe [Types.AttackVectorDescription]
    -- ^ The list of attacks for a specified time period.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The end time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
  , resourceArn :: Core.Maybe Core.Text
    -- ^ The ARN (Amazon Resource Name) of the resource that was attacked.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The start time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AttackSummary' value with any optional fields omitted.
mkAttackSummary
    :: AttackSummary
mkAttackSummary
  = AttackSummary'{attackId = Core.Nothing,
                   attackVectors = Core.Nothing, endTime = Core.Nothing,
                   resourceArn = Core.Nothing, startTime = Core.Nothing}

-- | The unique identifier (ID) of the attack.
--
-- /Note:/ Consider using 'attackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAttackId :: Lens.Lens' AttackSummary (Core.Maybe Core.Text)
asAttackId = Lens.field @"attackId"
{-# INLINEABLE asAttackId #-}
{-# DEPRECATED attackId "Use generic-lens or generic-optics with 'attackId' instead"  #-}

-- | The list of attacks for a specified time period.
--
-- /Note:/ Consider using 'attackVectors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAttackVectors :: Lens.Lens' AttackSummary (Core.Maybe [Types.AttackVectorDescription])
asAttackVectors = Lens.field @"attackVectors"
{-# INLINEABLE asAttackVectors #-}
{-# DEPRECATED attackVectors "Use generic-lens or generic-optics with 'attackVectors' instead"  #-}

-- | The end time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEndTime :: Lens.Lens' AttackSummary (Core.Maybe Core.NominalDiffTime)
asEndTime = Lens.field @"endTime"
{-# INLINEABLE asEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The ARN (Amazon Resource Name) of the resource that was attacked.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asResourceArn :: Lens.Lens' AttackSummary (Core.Maybe Core.Text)
asResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE asResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The start time of the attack, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asStartTime :: Lens.Lens' AttackSummary (Core.Maybe Core.NominalDiffTime)
asStartTime = Lens.field @"startTime"
{-# INLINEABLE asStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

instance Core.FromJSON AttackSummary where
        parseJSON
          = Core.withObject "AttackSummary" Core.$
              \ x ->
                AttackSummary' Core.<$>
                  (x Core..:? "AttackId") Core.<*> x Core..:? "AttackVectors"
                    Core.<*> x Core..:? "EndTime"
                    Core.<*> x Core..:? "ResourceArn"
                    Core.<*> x Core..:? "StartTime"

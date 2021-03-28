{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.Types.RotationRulesType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SecretsManager.Types.RotationRulesType
  ( RotationRulesType (..)
  -- * Smart constructor
  , mkRotationRulesType
  -- * Lenses
  , rrtAutomaticallyAfterDays
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure that defines the rotation configuration for the secret.
--
-- /See:/ 'mkRotationRulesType' smart constructor.
newtype RotationRulesType = RotationRulesType'
  { automaticallyAfterDays :: Core.Maybe Core.Natural
    -- ^ Specifies the number of days between automatic scheduled rotations of the secret.
--
-- Secrets Manager schedules the next rotation when the previous one is complete. Secrets Manager schedules the date by adding the rotation interval (number of days) to the actual date of the last rotation. The service chooses the hour within that 24-hour date window randomly. The minute is also chosen somewhat randomly, but weighted towards the top of the hour and influenced by a variety of factors that help distribute load.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RotationRulesType' value with any optional fields omitted.
mkRotationRulesType
    :: RotationRulesType
mkRotationRulesType
  = RotationRulesType'{automaticallyAfterDays = Core.Nothing}

-- | Specifies the number of days between automatic scheduled rotations of the secret.
--
-- Secrets Manager schedules the next rotation when the previous one is complete. Secrets Manager schedules the date by adding the rotation interval (number of days) to the actual date of the last rotation. The service chooses the hour within that 24-hour date window randomly. The minute is also chosen somewhat randomly, but weighted towards the top of the hour and influenced by a variety of factors that help distribute load.
--
-- /Note:/ Consider using 'automaticallyAfterDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrtAutomaticallyAfterDays :: Lens.Lens' RotationRulesType (Core.Maybe Core.Natural)
rrtAutomaticallyAfterDays = Lens.field @"automaticallyAfterDays"
{-# INLINEABLE rrtAutomaticallyAfterDays #-}
{-# DEPRECATED automaticallyAfterDays "Use generic-lens or generic-optics with 'automaticallyAfterDays' instead"  #-}

instance Core.FromJSON RotationRulesType where
        toJSON RotationRulesType{..}
          = Core.object
              (Core.catMaybes
                 [("AutomaticallyAfterDays" Core..=) Core.<$>
                    automaticallyAfterDays])

instance Core.FromJSON RotationRulesType where
        parseJSON
          = Core.withObject "RotationRulesType" Core.$
              \ x ->
                RotationRulesType' Core.<$> (x Core..:? "AutomaticallyAfterDays")

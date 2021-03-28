{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.PointInTimeRecoverySpecification
  ( PointInTimeRecoverySpecification (..)
  -- * Smart constructor
  , mkPointInTimeRecoverySpecification
  -- * Lenses
  , pitrsPointInTimeRecoveryEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the settings used to enable point in time recovery.
--
-- /See:/ 'mkPointInTimeRecoverySpecification' smart constructor.
newtype PointInTimeRecoverySpecification = PointInTimeRecoverySpecification'
  { pointInTimeRecoveryEnabled :: Core.Bool
    -- ^ Indicates whether point in time recovery is enabled (true) or disabled (false) on the table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PointInTimeRecoverySpecification' value with any optional fields omitted.
mkPointInTimeRecoverySpecification
    :: Core.Bool -- ^ 'pointInTimeRecoveryEnabled'
    -> PointInTimeRecoverySpecification
mkPointInTimeRecoverySpecification pointInTimeRecoveryEnabled
  = PointInTimeRecoverySpecification'{pointInTimeRecoveryEnabled}

-- | Indicates whether point in time recovery is enabled (true) or disabled (false) on the table.
--
-- /Note:/ Consider using 'pointInTimeRecoveryEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pitrsPointInTimeRecoveryEnabled :: Lens.Lens' PointInTimeRecoverySpecification Core.Bool
pitrsPointInTimeRecoveryEnabled = Lens.field @"pointInTimeRecoveryEnabled"
{-# INLINEABLE pitrsPointInTimeRecoveryEnabled #-}
{-# DEPRECATED pointInTimeRecoveryEnabled "Use generic-lens or generic-optics with 'pointInTimeRecoveryEnabled' instead"  #-}

instance Core.FromJSON PointInTimeRecoverySpecification where
        toJSON PointInTimeRecoverySpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("PointInTimeRecoveryEnabled" Core..= pointInTimeRecoveryEnabled)])

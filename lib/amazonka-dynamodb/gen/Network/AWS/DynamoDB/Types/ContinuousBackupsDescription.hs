{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ContinuousBackupsDescription
  ( ContinuousBackupsDescription (..)
  -- * Smart constructor
  , mkContinuousBackupsDescription
  -- * Lenses
  , cbdContinuousBackupsStatus
  , cbdPointInTimeRecoveryDescription
  ) where

import qualified Network.AWS.DynamoDB.Types.ContinuousBackupsStatus as Types
import qualified Network.AWS.DynamoDB.Types.PointInTimeRecoveryDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the continuous backups and point in time recovery settings on the table.
--
-- /See:/ 'mkContinuousBackupsDescription' smart constructor.
data ContinuousBackupsDescription = ContinuousBackupsDescription'
  { continuousBackupsStatus :: Types.ContinuousBackupsStatus
    -- ^ @ContinuousBackupsStatus@ can be one of the following states: ENABLED, DISABLED
  , pointInTimeRecoveryDescription :: Core.Maybe Types.PointInTimeRecoveryDescription
    -- ^ The description of the point in time recovery settings applied to the table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ContinuousBackupsDescription' value with any optional fields omitted.
mkContinuousBackupsDescription
    :: Types.ContinuousBackupsStatus -- ^ 'continuousBackupsStatus'
    -> ContinuousBackupsDescription
mkContinuousBackupsDescription continuousBackupsStatus
  = ContinuousBackupsDescription'{continuousBackupsStatus,
                                  pointInTimeRecoveryDescription = Core.Nothing}

-- | @ContinuousBackupsStatus@ can be one of the following states: ENABLED, DISABLED
--
-- /Note:/ Consider using 'continuousBackupsStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbdContinuousBackupsStatus :: Lens.Lens' ContinuousBackupsDescription Types.ContinuousBackupsStatus
cbdContinuousBackupsStatus = Lens.field @"continuousBackupsStatus"
{-# INLINEABLE cbdContinuousBackupsStatus #-}
{-# DEPRECATED continuousBackupsStatus "Use generic-lens or generic-optics with 'continuousBackupsStatus' instead"  #-}

-- | The description of the point in time recovery settings applied to the table.
--
-- /Note:/ Consider using 'pointInTimeRecoveryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbdPointInTimeRecoveryDescription :: Lens.Lens' ContinuousBackupsDescription (Core.Maybe Types.PointInTimeRecoveryDescription)
cbdPointInTimeRecoveryDescription = Lens.field @"pointInTimeRecoveryDescription"
{-# INLINEABLE cbdPointInTimeRecoveryDescription #-}
{-# DEPRECATED pointInTimeRecoveryDescription "Use generic-lens or generic-optics with 'pointInTimeRecoveryDescription' instead"  #-}

instance Core.FromJSON ContinuousBackupsDescription where
        parseJSON
          = Core.withObject "ContinuousBackupsDescription" Core.$
              \ x ->
                ContinuousBackupsDescription' Core.<$>
                  (x Core..: "ContinuousBackupsStatus") Core.<*>
                    x Core..:? "PointInTimeRecoveryDescription"

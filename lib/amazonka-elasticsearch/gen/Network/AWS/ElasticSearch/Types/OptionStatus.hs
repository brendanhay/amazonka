{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OptionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OptionStatus
  ( OptionStatus (..),

    -- * Smart constructor
    mkOptionStatus,

    -- * Lenses
    osCreationDate,
    osUpdateDate,
    osState,
    osPendingDeletion,
    osUpdateVersion,
  )
where

import qualified Network.AWS.ElasticSearch.Types.OptionState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides the current status of the entity.
--
-- /See:/ 'mkOptionStatus' smart constructor.
data OptionStatus = OptionStatus'
  { -- | Timestamp which tells the creation date for the entity.
    creationDate :: Core.NominalDiffTime,
    -- | Timestamp which tells the last updated time for the entity.
    updateDate :: Core.NominalDiffTime,
    -- | Provides the @OptionState@ for the Elasticsearch domain.
    state :: Types.OptionState,
    -- | Indicates whether the Elasticsearch domain is being deleted.
    pendingDeletion :: Core.Maybe Core.Bool,
    -- | Specifies the latest version for the entity.
    updateVersion :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OptionStatus' value with any optional fields omitted.
mkOptionStatus ::
  -- | 'creationDate'
  Core.NominalDiffTime ->
  -- | 'updateDate'
  Core.NominalDiffTime ->
  -- | 'state'
  Types.OptionState ->
  OptionStatus
mkOptionStatus creationDate updateDate state =
  OptionStatus'
    { creationDate,
      updateDate,
      state,
      pendingDeletion = Core.Nothing,
      updateVersion = Core.Nothing
    }

-- | Timestamp which tells the creation date for the entity.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCreationDate :: Lens.Lens' OptionStatus Core.NominalDiffTime
osCreationDate = Lens.field @"creationDate"
{-# DEPRECATED osCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Timestamp which tells the last updated time for the entity.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateDate :: Lens.Lens' OptionStatus Core.NominalDiffTime
osUpdateDate = Lens.field @"updateDate"
{-# DEPRECATED osUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | Provides the @OptionState@ for the Elasticsearch domain.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osState :: Lens.Lens' OptionStatus Types.OptionState
osState = Lens.field @"state"
{-# DEPRECATED osState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Indicates whether the Elasticsearch domain is being deleted.
--
-- /Note:/ Consider using 'pendingDeletion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPendingDeletion :: Lens.Lens' OptionStatus (Core.Maybe Core.Bool)
osPendingDeletion = Lens.field @"pendingDeletion"
{-# DEPRECATED osPendingDeletion "Use generic-lens or generic-optics with 'pendingDeletion' instead." #-}

-- | Specifies the latest version for the entity.
--
-- /Note:/ Consider using 'updateVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUpdateVersion :: Lens.Lens' OptionStatus (Core.Maybe Core.Natural)
osUpdateVersion = Lens.field @"updateVersion"
{-# DEPRECATED osUpdateVersion "Use generic-lens or generic-optics with 'updateVersion' instead." #-}

instance Core.FromJSON OptionStatus where
  parseJSON =
    Core.withObject "OptionStatus" Core.$
      \x ->
        OptionStatus'
          Core.<$> (x Core..: "CreationDate")
          Core.<*> (x Core..: "UpdateDate")
          Core.<*> (x Core..: "State")
          Core.<*> (x Core..:? "PendingDeletion")
          Core.<*> (x Core..:? "UpdateVersion")

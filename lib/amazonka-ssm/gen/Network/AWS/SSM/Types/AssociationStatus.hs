{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationStatus
  ( AssociationStatus (..),

    -- * Smart constructor
    mkAssociationStatus,

    -- * Lenses
    asDate,
    asName,
    asMessage,
    asAdditionalInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationStatusName as Types
import qualified Network.AWS.SSM.Types.StatusAdditionalInfo as Types
import qualified Network.AWS.SSM.Types.StatusMessage as Types

-- | Describes an association status.
--
-- /See:/ 'mkAssociationStatus' smart constructor.
data AssociationStatus = AssociationStatus'
  { -- | The date when the status changed.
    date :: Core.NominalDiffTime,
    -- | The status.
    name :: Types.AssociationStatusName,
    -- | The reason for the status.
    message :: Types.StatusMessage,
    -- | A user-defined string.
    additionalInfo :: Core.Maybe Types.StatusAdditionalInfo
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AssociationStatus' value with any optional fields omitted.
mkAssociationStatus ::
  -- | 'date'
  Core.NominalDiffTime ->
  -- | 'name'
  Types.AssociationStatusName ->
  -- | 'message'
  Types.StatusMessage ->
  AssociationStatus
mkAssociationStatus date name message =
  AssociationStatus'
    { date,
      name,
      message,
      additionalInfo = Core.Nothing
    }

-- | The date when the status changed.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDate :: Lens.Lens' AssociationStatus Core.NominalDiffTime
asDate = Lens.field @"date"
{-# DEPRECATED asDate "Use generic-lens or generic-optics with 'date' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asName :: Lens.Lens' AssociationStatus Types.AssociationStatusName
asName = Lens.field @"name"
{-# DEPRECATED asName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The reason for the status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMessage :: Lens.Lens' AssociationStatus Types.StatusMessage
asMessage = Lens.field @"message"
{-# DEPRECATED asMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A user-defined string.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAdditionalInfo :: Lens.Lens' AssociationStatus (Core.Maybe Types.StatusAdditionalInfo)
asAdditionalInfo = Lens.field @"additionalInfo"
{-# DEPRECATED asAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

instance Core.FromJSON AssociationStatus where
  toJSON AssociationStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Date" Core..= date),
            Core.Just ("Name" Core..= name),
            Core.Just ("Message" Core..= message),
            ("AdditionalInfo" Core..=) Core.<$> additionalInfo
          ]
      )

instance Core.FromJSON AssociationStatus where
  parseJSON =
    Core.withObject "AssociationStatus" Core.$
      \x ->
        AssociationStatus'
          Core.<$> (x Core..: "Date")
          Core.<*> (x Core..: "Name")
          Core.<*> (x Core..: "Message")
          Core.<*> (x Core..:? "AdditionalInfo")

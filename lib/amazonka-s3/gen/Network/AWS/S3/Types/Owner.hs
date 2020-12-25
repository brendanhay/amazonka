{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Owner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Owner
  ( Owner (..),

    -- * Smart constructor
    mkOwner,

    -- * Lenses
    oDisplayName,
    oID,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.DisplayName as Types
import qualified Network.AWS.S3.Types.ID as Types

-- | Container for the owner's display name and ID.
--
-- /See:/ 'mkOwner' smart constructor.
data Owner = Owner'
  { -- | Container for the display name of the owner.
    displayName :: Core.Maybe Types.DisplayName,
    -- | Container for the ID of the owner.
    id :: Core.Maybe Types.ID
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Owner' value with any optional fields omitted.
mkOwner ::
  Owner
mkOwner = Owner' {displayName = Core.Nothing, id = Core.Nothing}

-- | Container for the display name of the owner.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDisplayName :: Lens.Lens' Owner (Core.Maybe Types.DisplayName)
oDisplayName = Lens.field @"displayName"
{-# DEPRECATED oDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | Container for the ID of the owner.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oID :: Lens.Lens' Owner (Core.Maybe Types.ID)
oID = Lens.field @"id"
{-# DEPRECATED oID "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.ToXML Owner where
  toXML Owner {..} =
    Core.toXMLNode "DisplayName" Core.<$> displayName
      Core.<> Core.toXMLNode "ID" Core.<$> id

instance Core.FromXML Owner where
  parseXML x =
    Owner'
      Core.<$> (x Core..@? "DisplayName") Core.<*> (x Core..@? "ID")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeFilter
  ( HandshakeFilter (..),

    -- * Smart constructor
    mkHandshakeFilter,

    -- * Lenses
    hfActionType,
    hfParentHandshakeId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.ActionType as Types
import qualified Network.AWS.Organizations.Types.ParentHandshakeId as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the criteria that are used to select the handshakes for the operation.
--
-- /See:/ 'mkHandshakeFilter' smart constructor.
data HandshakeFilter = HandshakeFilter'
  { -- | Specifies the type of handshake action.
    --
    -- If you specify @ActionType@ , you cannot also specify @ParentHandshakeId@ .
    actionType :: Core.Maybe Types.ActionType,
    -- | Specifies the parent handshake. Only used for handshake types that are a child of another type.
    --
    -- If you specify @ParentHandshakeId@ , you cannot also specify @ActionType@ .
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
    parentHandshakeId :: Core.Maybe Types.ParentHandshakeId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HandshakeFilter' value with any optional fields omitted.
mkHandshakeFilter ::
  HandshakeFilter
mkHandshakeFilter =
  HandshakeFilter'
    { actionType = Core.Nothing,
      parentHandshakeId = Core.Nothing
    }

-- | Specifies the type of handshake action.
--
-- If you specify @ActionType@ , you cannot also specify @ParentHandshakeId@ .
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hfActionType :: Lens.Lens' HandshakeFilter (Core.Maybe Types.ActionType)
hfActionType = Lens.field @"actionType"
{-# DEPRECATED hfActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | Specifies the parent handshake. Only used for handshake types that are a child of another type.
--
-- If you specify @ParentHandshakeId@ , you cannot also specify @ActionType@ .
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'parentHandshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hfParentHandshakeId :: Lens.Lens' HandshakeFilter (Core.Maybe Types.ParentHandshakeId)
hfParentHandshakeId = Lens.field @"parentHandshakeId"
{-# DEPRECATED hfParentHandshakeId "Use generic-lens or generic-optics with 'parentHandshakeId' instead." #-}

instance Core.FromJSON HandshakeFilter where
  toJSON HandshakeFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("ActionType" Core..=) Core.<$> actionType,
            ("ParentHandshakeId" Core..=) Core.<$> parentHandshakeId
          ]
      )

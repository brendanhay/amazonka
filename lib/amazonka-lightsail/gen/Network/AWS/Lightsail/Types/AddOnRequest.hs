{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AddOnRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOnRequest
  ( AddOnRequest (..),

    -- * Smart constructor
    mkAddOnRequest,

    -- * Lenses
    aorAddOnType,
    aorAutoSnapshotAddOnRequest,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.AddOnType as Types
import qualified Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a request to enable, modify, or disable an add-on for an Amazon Lightsail resource.
--
-- /See:/ 'mkAddOnRequest' smart constructor.
data AddOnRequest = AddOnRequest'
  { -- | The add-on type.
    addOnType :: Types.AddOnType,
    -- | An object that represents additional parameters when enabling or modifying the automatic snapshot add-on.
    autoSnapshotAddOnRequest :: Core.Maybe Types.AutoSnapshotAddOnRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddOnRequest' value with any optional fields omitted.
mkAddOnRequest ::
  -- | 'addOnType'
  Types.AddOnType ->
  AddOnRequest
mkAddOnRequest addOnType =
  AddOnRequest' {addOnType, autoSnapshotAddOnRequest = Core.Nothing}

-- | The add-on type.
--
-- /Note:/ Consider using 'addOnType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aorAddOnType :: Lens.Lens' AddOnRequest Types.AddOnType
aorAddOnType = Lens.field @"addOnType"
{-# DEPRECATED aorAddOnType "Use generic-lens or generic-optics with 'addOnType' instead." #-}

-- | An object that represents additional parameters when enabling or modifying the automatic snapshot add-on.
--
-- /Note:/ Consider using 'autoSnapshotAddOnRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aorAutoSnapshotAddOnRequest :: Lens.Lens' AddOnRequest (Core.Maybe Types.AutoSnapshotAddOnRequest)
aorAutoSnapshotAddOnRequest = Lens.field @"autoSnapshotAddOnRequest"
{-# DEPRECATED aorAutoSnapshotAddOnRequest "Use generic-lens or generic-optics with 'autoSnapshotAddOnRequest' instead." #-}

instance Core.FromJSON AddOnRequest where
  toJSON AddOnRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("addOnType" Core..= addOnType),
            ("autoSnapshotAddOnRequest" Core..=)
              Core.<$> autoSnapshotAddOnRequest
          ]
      )

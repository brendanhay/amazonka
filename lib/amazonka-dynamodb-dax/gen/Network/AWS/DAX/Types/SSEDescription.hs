{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SSEDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SSEDescription
  ( SSEDescription (..),

    -- * Smart constructor
    mkSSEDescription,

    -- * Lenses
    ssedStatus,
  )
where

import qualified Network.AWS.DAX.Types.SSEStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The description of the server-side encryption status on the specified DAX cluster.
--
-- /See:/ 'mkSSEDescription' smart constructor.
newtype SSEDescription = SSEDescription'
  { -- | The current state of server-side encryption:
    --
    --
    --     * @ENABLING@ - Server-side encryption is being enabled.
    --
    --
    --     * @ENABLED@ - Server-side encryption is enabled.
    --
    --
    --     * @DISABLING@ - Server-side encryption is being disabled.
    --
    --
    --     * @DISABLED@ - Server-side encryption is disabled.
    status :: Core.Maybe Types.SSEStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SSEDescription' value with any optional fields omitted.
mkSSEDescription ::
  SSEDescription
mkSSEDescription = SSEDescription' {status = Core.Nothing}

-- | The current state of server-side encryption:
--
--
--     * @ENABLING@ - Server-side encryption is being enabled.
--
--
--     * @ENABLED@ - Server-side encryption is enabled.
--
--
--     * @DISABLING@ - Server-side encryption is being disabled.
--
--
--     * @DISABLED@ - Server-side encryption is disabled.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssedStatus :: Lens.Lens' SSEDescription (Core.Maybe Types.SSEStatus)
ssedStatus = Lens.field @"status"
{-# DEPRECATED ssedStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON SSEDescription where
  parseJSON =
    Core.withObject "SSEDescription" Core.$
      \x -> SSEDescription' Core.<$> (x Core..:? "Status")

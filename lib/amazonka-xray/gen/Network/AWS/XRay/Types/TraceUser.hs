{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TraceUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TraceUser
  ( TraceUser (..),

    -- * Smart constructor
    mkTraceUser,

    -- * Lenses
    tuServiceIds,
    tuUserName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.ServiceId as Types
import qualified Network.AWS.XRay.Types.String as Types

-- | Information about a user recorded in segment documents.
--
-- /See:/ 'mkTraceUser' smart constructor.
data TraceUser = TraceUser'
  { -- | Services that the user's request hit.
    serviceIds :: Core.Maybe [Types.ServiceId],
    -- | The user's name.
    userName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TraceUser' value with any optional fields omitted.
mkTraceUser ::
  TraceUser
mkTraceUser =
  TraceUser' {serviceIds = Core.Nothing, userName = Core.Nothing}

-- | Services that the user's request hit.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuServiceIds :: Lens.Lens' TraceUser (Core.Maybe [Types.ServiceId])
tuServiceIds = Lens.field @"serviceIds"
{-# DEPRECATED tuServiceIds "Use generic-lens or generic-optics with 'serviceIds' instead." #-}

-- | The user's name.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tuUserName :: Lens.Lens' TraceUser (Core.Maybe Types.String)
tuUserName = Lens.field @"userName"
{-# DEPRECATED tuUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.FromJSON TraceUser where
  parseJSON =
    Core.withObject "TraceUser" Core.$
      \x ->
        TraceUser'
          Core.<$> (x Core..:? "ServiceIds") Core.<*> (x Core..:? "UserName")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.BackendConnectionErrors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.BackendConnectionErrors
  ( BackendConnectionErrors (..),

    -- * Smart constructor
    mkBackendConnectionErrors,

    -- * Lenses
    bceConnectionRefusedCount,
    bceHTTPCode4XXCount,
    bceHTTPCode5XXCount,
    bceOtherCount,
    bceTimeoutCount,
    bceUnknownHostCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- |
--
-- /See:/ 'mkBackendConnectionErrors' smart constructor.
data BackendConnectionErrors = BackendConnectionErrors'
  { -- |
    connectionRefusedCount :: Core.Maybe Core.Int,
    -- |
    hTTPCode4XXCount :: Core.Maybe Core.Int,
    -- |
    hTTPCode5XXCount :: Core.Maybe Core.Int,
    -- |
    otherCount :: Core.Maybe Core.Int,
    -- |
    timeoutCount :: Core.Maybe Core.Int,
    -- |
    unknownHostCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BackendConnectionErrors' value with any optional fields omitted.
mkBackendConnectionErrors ::
  BackendConnectionErrors
mkBackendConnectionErrors =
  BackendConnectionErrors'
    { connectionRefusedCount = Core.Nothing,
      hTTPCode4XXCount = Core.Nothing,
      hTTPCode5XXCount = Core.Nothing,
      otherCount = Core.Nothing,
      timeoutCount = Core.Nothing,
      unknownHostCount = Core.Nothing
    }

-- |
--
-- /Note:/ Consider using 'connectionRefusedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceConnectionRefusedCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
bceConnectionRefusedCount = Lens.field @"connectionRefusedCount"
{-# DEPRECATED bceConnectionRefusedCount "Use generic-lens or generic-optics with 'connectionRefusedCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'hTTPCode4XXCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceHTTPCode4XXCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
bceHTTPCode4XXCount = Lens.field @"hTTPCode4XXCount"
{-# DEPRECATED bceHTTPCode4XXCount "Use generic-lens or generic-optics with 'hTTPCode4XXCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'hTTPCode5XXCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceHTTPCode5XXCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
bceHTTPCode5XXCount = Lens.field @"hTTPCode5XXCount"
{-# DEPRECATED bceHTTPCode5XXCount "Use generic-lens or generic-optics with 'hTTPCode5XXCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'otherCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceOtherCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
bceOtherCount = Lens.field @"otherCount"
{-# DEPRECATED bceOtherCount "Use generic-lens or generic-optics with 'otherCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'timeoutCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceTimeoutCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
bceTimeoutCount = Lens.field @"timeoutCount"
{-# DEPRECATED bceTimeoutCount "Use generic-lens or generic-optics with 'timeoutCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'unknownHostCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceUnknownHostCount :: Lens.Lens' BackendConnectionErrors (Core.Maybe Core.Int)
bceUnknownHostCount = Lens.field @"unknownHostCount"
{-# DEPRECATED bceUnknownHostCount "Use generic-lens or generic-optics with 'unknownHostCount' instead." #-}

instance Core.FromJSON BackendConnectionErrors where
  toJSON BackendConnectionErrors {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConnectionRefusedCount" Core..=)
              Core.<$> connectionRefusedCount,
            ("HTTPCode4XXCount" Core..=) Core.<$> hTTPCode4XXCount,
            ("HTTPCode5XXCount" Core..=) Core.<$> hTTPCode5XXCount,
            ("OtherCount" Core..=) Core.<$> otherCount,
            ("TimeoutCount" Core..=) Core.<$> timeoutCount,
            ("UnknownHostCount" Core..=) Core.<$> unknownHostCount
          ]
      )

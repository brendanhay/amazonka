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
    bceOtherCount,
    bceTimeoutCount,
    bceHTTPCode5XXCount,
    bceConnectionRefusedCount,
    bceHTTPCode4XXCount,
    bceUnknownHostCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkBackendConnectionErrors' smart constructor.
data BackendConnectionErrors = BackendConnectionErrors'
  { -- |
    otherCount :: Lude.Maybe Lude.Int,
    -- |
    timeoutCount :: Lude.Maybe Lude.Int,
    -- |
    hTTPCode5XXCount :: Lude.Maybe Lude.Int,
    -- |
    connectionRefusedCount :: Lude.Maybe Lude.Int,
    -- |
    hTTPCode4XXCount :: Lude.Maybe Lude.Int,
    -- |
    unknownHostCount :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackendConnectionErrors' with the minimum fields required to make a request.
--
-- * 'otherCount' -
-- * 'timeoutCount' -
-- * 'hTTPCode5XXCount' -
-- * 'connectionRefusedCount' -
-- * 'hTTPCode4XXCount' -
-- * 'unknownHostCount' -
mkBackendConnectionErrors ::
  BackendConnectionErrors
mkBackendConnectionErrors =
  BackendConnectionErrors'
    { otherCount = Lude.Nothing,
      timeoutCount = Lude.Nothing,
      hTTPCode5XXCount = Lude.Nothing,
      connectionRefusedCount = Lude.Nothing,
      hTTPCode4XXCount = Lude.Nothing,
      unknownHostCount = Lude.Nothing
    }

-- |
--
-- /Note:/ Consider using 'otherCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceOtherCount :: Lens.Lens' BackendConnectionErrors (Lude.Maybe Lude.Int)
bceOtherCount = Lens.lens (otherCount :: BackendConnectionErrors -> Lude.Maybe Lude.Int) (\s a -> s {otherCount = a} :: BackendConnectionErrors)
{-# DEPRECATED bceOtherCount "Use generic-lens or generic-optics with 'otherCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'timeoutCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceTimeoutCount :: Lens.Lens' BackendConnectionErrors (Lude.Maybe Lude.Int)
bceTimeoutCount = Lens.lens (timeoutCount :: BackendConnectionErrors -> Lude.Maybe Lude.Int) (\s a -> s {timeoutCount = a} :: BackendConnectionErrors)
{-# DEPRECATED bceTimeoutCount "Use generic-lens or generic-optics with 'timeoutCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'hTTPCode5XXCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceHTTPCode5XXCount :: Lens.Lens' BackendConnectionErrors (Lude.Maybe Lude.Int)
bceHTTPCode5XXCount = Lens.lens (hTTPCode5XXCount :: BackendConnectionErrors -> Lude.Maybe Lude.Int) (\s a -> s {hTTPCode5XXCount = a} :: BackendConnectionErrors)
{-# DEPRECATED bceHTTPCode5XXCount "Use generic-lens or generic-optics with 'hTTPCode5XXCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'connectionRefusedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceConnectionRefusedCount :: Lens.Lens' BackendConnectionErrors (Lude.Maybe Lude.Int)
bceConnectionRefusedCount = Lens.lens (connectionRefusedCount :: BackendConnectionErrors -> Lude.Maybe Lude.Int) (\s a -> s {connectionRefusedCount = a} :: BackendConnectionErrors)
{-# DEPRECATED bceConnectionRefusedCount "Use generic-lens or generic-optics with 'connectionRefusedCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'hTTPCode4XXCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceHTTPCode4XXCount :: Lens.Lens' BackendConnectionErrors (Lude.Maybe Lude.Int)
bceHTTPCode4XXCount = Lens.lens (hTTPCode4XXCount :: BackendConnectionErrors -> Lude.Maybe Lude.Int) (\s a -> s {hTTPCode4XXCount = a} :: BackendConnectionErrors)
{-# DEPRECATED bceHTTPCode4XXCount "Use generic-lens or generic-optics with 'hTTPCode4XXCount' instead." #-}

-- |
--
-- /Note:/ Consider using 'unknownHostCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bceUnknownHostCount :: Lens.Lens' BackendConnectionErrors (Lude.Maybe Lude.Int)
bceUnknownHostCount = Lens.lens (unknownHostCount :: BackendConnectionErrors -> Lude.Maybe Lude.Int) (\s a -> s {unknownHostCount = a} :: BackendConnectionErrors)
{-# DEPRECATED bceUnknownHostCount "Use generic-lens or generic-optics with 'unknownHostCount' instead." #-}

instance Lude.ToJSON BackendConnectionErrors where
  toJSON BackendConnectionErrors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OtherCount" Lude..=) Lude.<$> otherCount,
            ("TimeoutCount" Lude..=) Lude.<$> timeoutCount,
            ("HTTPCode5XXCount" Lude..=) Lude.<$> hTTPCode5XXCount,
            ("ConnectionRefusedCount" Lude..=) Lude.<$> connectionRefusedCount,
            ("HTTPCode4XXCount" Lude..=) Lude.<$> hTTPCode4XXCount,
            ("UnknownHostCount" Lude..=) Lude.<$> unknownHostCount
          ]
      )

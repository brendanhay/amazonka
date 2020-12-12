{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureOption
  ( CaptureOption (..),

    -- * Smart constructor
    mkCaptureOption,

    -- * Lenses
    coCaptureMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CaptureMode

-- |
--
-- /See:/ 'mkCaptureOption' smart constructor.
newtype CaptureOption = CaptureOption' {captureMode :: CaptureMode}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptureOption' with the minimum fields required to make a request.
--
-- * 'captureMode' -
mkCaptureOption ::
  -- | 'captureMode'
  CaptureMode ->
  CaptureOption
mkCaptureOption pCaptureMode_ =
  CaptureOption' {captureMode = pCaptureMode_}

-- |
--
-- /Note:/ Consider using 'captureMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCaptureMode :: Lens.Lens' CaptureOption CaptureMode
coCaptureMode = Lens.lens (captureMode :: CaptureOption -> CaptureMode) (\s a -> s {captureMode = a} :: CaptureOption)
{-# DEPRECATED coCaptureMode "Use generic-lens or generic-optics with 'captureMode' instead." #-}

instance Lude.FromJSON CaptureOption where
  parseJSON =
    Lude.withObject
      "CaptureOption"
      (\x -> CaptureOption' Lude.<$> (x Lude..: "CaptureMode"))

instance Lude.ToJSON CaptureOption where
  toJSON CaptureOption' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("CaptureMode" Lude..= captureMode)])

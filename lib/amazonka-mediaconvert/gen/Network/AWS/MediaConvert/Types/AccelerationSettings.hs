{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AccelerationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AccelerationSettings
  ( AccelerationSettings (..),

    -- * Smart constructor
    mkAccelerationSettings,

    -- * Lenses
    asMode,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AccelerationMode
import qualified Network.AWS.Prelude as Lude

-- | Accelerated transcoding can significantly speed up jobs with long, visually complex content.
--
-- /See:/ 'mkAccelerationSettings' smart constructor.
newtype AccelerationSettings = AccelerationSettings'
  { -- | Specify the conditions when the service will run your job with accelerated transcoding.
    mode :: AccelerationMode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccelerationSettings' with the minimum fields required to make a request.
--
-- * 'mode' - Specify the conditions when the service will run your job with accelerated transcoding.
mkAccelerationSettings ::
  -- | 'mode'
  AccelerationMode ->
  AccelerationSettings
mkAccelerationSettings pMode_ =
  AccelerationSettings' {mode = pMode_}

-- | Specify the conditions when the service will run your job with accelerated transcoding.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMode :: Lens.Lens' AccelerationSettings AccelerationMode
asMode = Lens.lens (mode :: AccelerationSettings -> AccelerationMode) (\s a -> s {mode = a} :: AccelerationSettings)
{-# DEPRECATED asMode "Use generic-lens or generic-optics with 'mode' instead." #-}

instance Lude.FromJSON AccelerationSettings where
  parseJSON =
    Lude.withObject
      "AccelerationSettings"
      (\x -> AccelerationSettings' Lude.<$> (x Lude..: "mode"))

instance Lude.ToJSON AccelerationSettings where
  toJSON AccelerationSettings' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("mode" Lude..= mode)])

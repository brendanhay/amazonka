-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageGroupSettings
  ( MediaPackageGroupSettings (..),

    -- * Smart constructor
    mkMediaPackageGroupSettings,

    -- * Lenses
    mpgsDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import qualified Network.AWS.Prelude as Lude

-- | Media Package Group Settings
--
-- /See:/ 'mkMediaPackageGroupSettings' smart constructor.
newtype MediaPackageGroupSettings = MediaPackageGroupSettings'
  { destination ::
      OutputLocationRef
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MediaPackageGroupSettings' with the minimum fields required to make a request.
--
-- * 'destination' - MediaPackage channel destination.
mkMediaPackageGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  MediaPackageGroupSettings
mkMediaPackageGroupSettings pDestination_ =
  MediaPackageGroupSettings' {destination = pDestination_}

-- | MediaPackage channel destination.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpgsDestination :: Lens.Lens' MediaPackageGroupSettings OutputLocationRef
mpgsDestination = Lens.lens (destination :: MediaPackageGroupSettings -> OutputLocationRef) (\s a -> s {destination = a} :: MediaPackageGroupSettings)
{-# DEPRECATED mpgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromJSON MediaPackageGroupSettings where
  parseJSON =
    Lude.withObject
      "MediaPackageGroupSettings"
      ( \x ->
          MediaPackageGroupSettings' Lude.<$> (x Lude..: "destination")
      )

instance Lude.ToJSON MediaPackageGroupSettings where
  toJSON MediaPackageGroupSettings' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("destination" Lude..= destination)])

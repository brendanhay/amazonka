{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35Descriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35Descriptor
  ( Scte35Descriptor (..),

    -- * Smart constructor
    mkScte35Descriptor,

    -- * Lenses
    sdScte35DescriptorSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35DescriptorSettings
import qualified Network.AWS.Prelude as Lude

-- | Holds one set of SCTE-35 Descriptor Settings.
--
-- /See:/ 'mkScte35Descriptor' smart constructor.
newtype Scte35Descriptor = Scte35Descriptor'
  { -- | SCTE-35 Descriptor Settings.
    scte35DescriptorSettings :: Scte35DescriptorSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scte35Descriptor' with the minimum fields required to make a request.
--
-- * 'scte35DescriptorSettings' - SCTE-35 Descriptor Settings.
mkScte35Descriptor ::
  -- | 'scte35DescriptorSettings'
  Scte35DescriptorSettings ->
  Scte35Descriptor
mkScte35Descriptor pScte35DescriptorSettings_ =
  Scte35Descriptor'
    { scte35DescriptorSettings =
        pScte35DescriptorSettings_
    }

-- | SCTE-35 Descriptor Settings.
--
-- /Note:/ Consider using 'scte35DescriptorSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdScte35DescriptorSettings :: Lens.Lens' Scte35Descriptor Scte35DescriptorSettings
sdScte35DescriptorSettings = Lens.lens (scte35DescriptorSettings :: Scte35Descriptor -> Scte35DescriptorSettings) (\s a -> s {scte35DescriptorSettings = a} :: Scte35Descriptor)
{-# DEPRECATED sdScte35DescriptorSettings "Use generic-lens or generic-optics with 'scte35DescriptorSettings' instead." #-}

instance Lude.FromJSON Scte35Descriptor where
  parseJSON =
    Lude.withObject
      "Scte35Descriptor"
      ( \x ->
          Scte35Descriptor' Lude.<$> (x Lude..: "scte35DescriptorSettings")
      )

instance Lude.ToJSON Scte35Descriptor where
  toJSON Scte35Descriptor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("scte35DescriptorSettings" Lude..= scte35DescriptorSettings)
          ]
      )

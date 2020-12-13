{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ArchiveContainerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ArchiveContainerSettings
  ( ArchiveContainerSettings (..),

    -- * Smart constructor
    mkArchiveContainerSettings,

    -- * Lenses
    acsM2tsSettings,
    acsRawSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.M2tsSettings
import Network.AWS.MediaLive.Types.RawSettings
import qualified Network.AWS.Prelude as Lude

-- | Archive Container Settings
--
-- /See:/ 'mkArchiveContainerSettings' smart constructor.
data ArchiveContainerSettings = ArchiveContainerSettings'
  { m2tsSettings :: Lude.Maybe M2tsSettings,
    rawSettings :: Lude.Maybe RawSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArchiveContainerSettings' with the minimum fields required to make a request.
--
-- * 'm2tsSettings' -
-- * 'rawSettings' -
mkArchiveContainerSettings ::
  ArchiveContainerSettings
mkArchiveContainerSettings =
  ArchiveContainerSettings'
    { m2tsSettings = Lude.Nothing,
      rawSettings = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'm2tsSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsM2tsSettings :: Lens.Lens' ArchiveContainerSettings (Lude.Maybe M2tsSettings)
acsM2tsSettings = Lens.lens (m2tsSettings :: ArchiveContainerSettings -> Lude.Maybe M2tsSettings) (\s a -> s {m2tsSettings = a} :: ArchiveContainerSettings)
{-# DEPRECATED acsM2tsSettings "Use generic-lens or generic-optics with 'm2tsSettings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'rawSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acsRawSettings :: Lens.Lens' ArchiveContainerSettings (Lude.Maybe RawSettings)
acsRawSettings = Lens.lens (rawSettings :: ArchiveContainerSettings -> Lude.Maybe RawSettings) (\s a -> s {rawSettings = a} :: ArchiveContainerSettings)
{-# DEPRECATED acsRawSettings "Use generic-lens or generic-optics with 'rawSettings' instead." #-}

instance Lude.FromJSON ArchiveContainerSettings where
  parseJSON =
    Lude.withObject
      "ArchiveContainerSettings"
      ( \x ->
          ArchiveContainerSettings'
            Lude.<$> (x Lude..:? "m2tsSettings") Lude.<*> (x Lude..:? "rawSettings")
      )

instance Lude.ToJSON ArchiveContainerSettings where
  toJSON ArchiveContainerSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("m2tsSettings" Lude..=) Lude.<$> m2tsSettings,
            ("rawSettings" Lude..=) Lude.<$> rawSettings
          ]
      )

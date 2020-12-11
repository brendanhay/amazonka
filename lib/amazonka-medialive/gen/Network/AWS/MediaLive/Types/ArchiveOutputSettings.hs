-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ArchiveOutputSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ArchiveOutputSettings
  ( ArchiveOutputSettings (..),

    -- * Smart constructor
    mkArchiveOutputSettings,

    -- * Lenses
    aosExtension,
    aosNameModifier,
    aosContainerSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.ArchiveContainerSettings
import qualified Network.AWS.Prelude as Lude

-- | Archive Output Settings
--
-- /See:/ 'mkArchiveOutputSettings' smart constructor.
data ArchiveOutputSettings = ArchiveOutputSettings'
  { extension ::
      Lude.Maybe Lude.Text,
    nameModifier :: Lude.Maybe Lude.Text,
    containerSettings :: ArchiveContainerSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArchiveOutputSettings' with the minimum fields required to make a request.
--
-- * 'containerSettings' - Settings specific to the container type of the file.
-- * 'extension' - Output file extension. If excluded, this will be auto-selected from the container type.
-- * 'nameModifier' - String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
mkArchiveOutputSettings ::
  -- | 'containerSettings'
  ArchiveContainerSettings ->
  ArchiveOutputSettings
mkArchiveOutputSettings pContainerSettings_ =
  ArchiveOutputSettings'
    { extension = Lude.Nothing,
      nameModifier = Lude.Nothing,
      containerSettings = pContainerSettings_
    }

-- | Output file extension. If excluded, this will be auto-selected from the container type.
--
-- /Note:/ Consider using 'extension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosExtension :: Lens.Lens' ArchiveOutputSettings (Lude.Maybe Lude.Text)
aosExtension = Lens.lens (extension :: ArchiveOutputSettings -> Lude.Maybe Lude.Text) (\s a -> s {extension = a} :: ArchiveOutputSettings)
{-# DEPRECATED aosExtension "Use generic-lens or generic-optics with 'extension' instead." #-}

-- | String concatenated to the end of the destination filename.  Required for multiple outputs of the same type.
--
-- /Note:/ Consider using 'nameModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosNameModifier :: Lens.Lens' ArchiveOutputSettings (Lude.Maybe Lude.Text)
aosNameModifier = Lens.lens (nameModifier :: ArchiveOutputSettings -> Lude.Maybe Lude.Text) (\s a -> s {nameModifier = a} :: ArchiveOutputSettings)
{-# DEPRECATED aosNameModifier "Use generic-lens or generic-optics with 'nameModifier' instead." #-}

-- | Settings specific to the container type of the file.
--
-- /Note:/ Consider using 'containerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aosContainerSettings :: Lens.Lens' ArchiveOutputSettings ArchiveContainerSettings
aosContainerSettings = Lens.lens (containerSettings :: ArchiveOutputSettings -> ArchiveContainerSettings) (\s a -> s {containerSettings = a} :: ArchiveOutputSettings)
{-# DEPRECATED aosContainerSettings "Use generic-lens or generic-optics with 'containerSettings' instead." #-}

instance Lude.FromJSON ArchiveOutputSettings where
  parseJSON =
    Lude.withObject
      "ArchiveOutputSettings"
      ( \x ->
          ArchiveOutputSettings'
            Lude.<$> (x Lude..:? "extension")
            Lude.<*> (x Lude..:? "nameModifier")
            Lude.<*> (x Lude..: "containerSettings")
      )

instance Lude.ToJSON ArchiveOutputSettings where
  toJSON ArchiveOutputSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("extension" Lude..=) Lude.<$> extension,
            ("nameModifier" Lude..=) Lude.<$> nameModifier,
            Lude.Just ("containerSettings" Lude..= containerSettings)
          ]
      )

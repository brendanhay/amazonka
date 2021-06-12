{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputDestination where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
import Network.AWS.MediaLive.Types.MultiplexProgramChannelDestinationSettings
import Network.AWS.MediaLive.Types.OutputDestinationSettings

-- | Placeholder documentation for OutputDestination
--
-- /See:/ 'newOutputDestination' smart constructor.
data OutputDestination = OutputDestination'
  { -- | Destination settings for a MediaPackage output; one destination for both
    -- encoders.
    mediaPackageSettings :: Core.Maybe [MediaPackageOutputDestinationSettings],
    -- | User-specified id. This is used in an output group or an output.
    id :: Core.Maybe Core.Text,
    -- | Destination settings for a Multiplex output; one destination for both
    -- encoders.
    multiplexSettings :: Core.Maybe MultiplexProgramChannelDestinationSettings,
    -- | Destination settings for a standard output; one destination for each
    -- redundant encoder.
    settings :: Core.Maybe [OutputDestinationSettings]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutputDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaPackageSettings', 'outputDestination_mediaPackageSettings' - Destination settings for a MediaPackage output; one destination for both
-- encoders.
--
-- 'id', 'outputDestination_id' - User-specified id. This is used in an output group or an output.
--
-- 'multiplexSettings', 'outputDestination_multiplexSettings' - Destination settings for a Multiplex output; one destination for both
-- encoders.
--
-- 'settings', 'outputDestination_settings' - Destination settings for a standard output; one destination for each
-- redundant encoder.
newOutputDestination ::
  OutputDestination
newOutputDestination =
  OutputDestination'
    { mediaPackageSettings =
        Core.Nothing,
      id = Core.Nothing,
      multiplexSettings = Core.Nothing,
      settings = Core.Nothing
    }

-- | Destination settings for a MediaPackage output; one destination for both
-- encoders.
outputDestination_mediaPackageSettings :: Lens.Lens' OutputDestination (Core.Maybe [MediaPackageOutputDestinationSettings])
outputDestination_mediaPackageSettings = Lens.lens (\OutputDestination' {mediaPackageSettings} -> mediaPackageSettings) (\s@OutputDestination' {} a -> s {mediaPackageSettings = a} :: OutputDestination) Core.. Lens.mapping Lens._Coerce

-- | User-specified id. This is used in an output group or an output.
outputDestination_id :: Lens.Lens' OutputDestination (Core.Maybe Core.Text)
outputDestination_id = Lens.lens (\OutputDestination' {id} -> id) (\s@OutputDestination' {} a -> s {id = a} :: OutputDestination)

-- | Destination settings for a Multiplex output; one destination for both
-- encoders.
outputDestination_multiplexSettings :: Lens.Lens' OutputDestination (Core.Maybe MultiplexProgramChannelDestinationSettings)
outputDestination_multiplexSettings = Lens.lens (\OutputDestination' {multiplexSettings} -> multiplexSettings) (\s@OutputDestination' {} a -> s {multiplexSettings = a} :: OutputDestination)

-- | Destination settings for a standard output; one destination for each
-- redundant encoder.
outputDestination_settings :: Lens.Lens' OutputDestination (Core.Maybe [OutputDestinationSettings])
outputDestination_settings = Lens.lens (\OutputDestination' {settings} -> settings) (\s@OutputDestination' {} a -> s {settings = a} :: OutputDestination) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON OutputDestination where
  parseJSON =
    Core.withObject
      "OutputDestination"
      ( \x ->
          OutputDestination'
            Core.<$> ( x Core..:? "mediaPackageSettings"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "multiplexSettings")
            Core.<*> (x Core..:? "settings" Core..!= Core.mempty)
      )

instance Core.Hashable OutputDestination

instance Core.NFData OutputDestination

instance Core.ToJSON OutputDestination where
  toJSON OutputDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("mediaPackageSettings" Core..=)
              Core.<$> mediaPackageSettings,
            ("id" Core..=) Core.<$> id,
            ("multiplexSettings" Core..=)
              Core.<$> multiplexSettings,
            ("settings" Core..=) Core.<$> settings
          ]
      )

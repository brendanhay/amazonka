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
-- Module      : Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.StaticImageDeactivateScheduleActionSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings for the action to deactivate the image in a specific layer.
--
-- /See:/ 'newStaticImageDeactivateScheduleActionSettings' smart constructor.
data StaticImageDeactivateScheduleActionSettings = StaticImageDeactivateScheduleActionSettings'
  { -- | The image overlay layer to deactivate, 0 to 7. Default is 0.
    layer :: Core.Maybe Core.Natural,
    -- | The time in milliseconds for the image to fade out. Default is 0 (no
    -- fade-out).
    fadeOut :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StaticImageDeactivateScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layer', 'staticImageDeactivateScheduleActionSettings_layer' - The image overlay layer to deactivate, 0 to 7. Default is 0.
--
-- 'fadeOut', 'staticImageDeactivateScheduleActionSettings_fadeOut' - The time in milliseconds for the image to fade out. Default is 0 (no
-- fade-out).
newStaticImageDeactivateScheduleActionSettings ::
  StaticImageDeactivateScheduleActionSettings
newStaticImageDeactivateScheduleActionSettings =
  StaticImageDeactivateScheduleActionSettings'
    { layer =
        Core.Nothing,
      fadeOut = Core.Nothing
    }

-- | The image overlay layer to deactivate, 0 to 7. Default is 0.
staticImageDeactivateScheduleActionSettings_layer :: Lens.Lens' StaticImageDeactivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageDeactivateScheduleActionSettings_layer = Lens.lens (\StaticImageDeactivateScheduleActionSettings' {layer} -> layer) (\s@StaticImageDeactivateScheduleActionSettings' {} a -> s {layer = a} :: StaticImageDeactivateScheduleActionSettings)

-- | The time in milliseconds for the image to fade out. Default is 0 (no
-- fade-out).
staticImageDeactivateScheduleActionSettings_fadeOut :: Lens.Lens' StaticImageDeactivateScheduleActionSettings (Core.Maybe Core.Natural)
staticImageDeactivateScheduleActionSettings_fadeOut = Lens.lens (\StaticImageDeactivateScheduleActionSettings' {fadeOut} -> fadeOut) (\s@StaticImageDeactivateScheduleActionSettings' {} a -> s {fadeOut = a} :: StaticImageDeactivateScheduleActionSettings)

instance
  Core.FromJSON
    StaticImageDeactivateScheduleActionSettings
  where
  parseJSON =
    Core.withObject
      "StaticImageDeactivateScheduleActionSettings"
      ( \x ->
          StaticImageDeactivateScheduleActionSettings'
            Core.<$> (x Core..:? "layer") Core.<*> (x Core..:? "fadeOut")
      )

instance
  Core.Hashable
    StaticImageDeactivateScheduleActionSettings

instance
  Core.NFData
    StaticImageDeactivateScheduleActionSettings

instance
  Core.ToJSON
    StaticImageDeactivateScheduleActionSettings
  where
  toJSON
    StaticImageDeactivateScheduleActionSettings' {..} =
      Core.object
        ( Core.catMaybes
            [ ("layer" Core..=) Core.<$> layer,
              ("fadeOut" Core..=) Core.<$> fadeOut
            ]
        )

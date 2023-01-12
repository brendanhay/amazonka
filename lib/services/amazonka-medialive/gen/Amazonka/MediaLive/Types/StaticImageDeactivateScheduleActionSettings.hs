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
-- Module      : Amazonka.MediaLive.Types.StaticImageDeactivateScheduleActionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.StaticImageDeactivateScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for the action to deactivate the image in a specific layer.
--
-- /See:/ 'newStaticImageDeactivateScheduleActionSettings' smart constructor.
data StaticImageDeactivateScheduleActionSettings = StaticImageDeactivateScheduleActionSettings'
  { -- | The time in milliseconds for the image to fade out. Default is 0 (no
    -- fade-out).
    fadeOut :: Prelude.Maybe Prelude.Natural,
    -- | The image overlay layer to deactivate, 0 to 7. Default is 0.
    layer :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticImageDeactivateScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fadeOut', 'staticImageDeactivateScheduleActionSettings_fadeOut' - The time in milliseconds for the image to fade out. Default is 0 (no
-- fade-out).
--
-- 'layer', 'staticImageDeactivateScheduleActionSettings_layer' - The image overlay layer to deactivate, 0 to 7. Default is 0.
newStaticImageDeactivateScheduleActionSettings ::
  StaticImageDeactivateScheduleActionSettings
newStaticImageDeactivateScheduleActionSettings =
  StaticImageDeactivateScheduleActionSettings'
    { fadeOut =
        Prelude.Nothing,
      layer = Prelude.Nothing
    }

-- | The time in milliseconds for the image to fade out. Default is 0 (no
-- fade-out).
staticImageDeactivateScheduleActionSettings_fadeOut :: Lens.Lens' StaticImageDeactivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageDeactivateScheduleActionSettings_fadeOut = Lens.lens (\StaticImageDeactivateScheduleActionSettings' {fadeOut} -> fadeOut) (\s@StaticImageDeactivateScheduleActionSettings' {} a -> s {fadeOut = a} :: StaticImageDeactivateScheduleActionSettings)

-- | The image overlay layer to deactivate, 0 to 7. Default is 0.
staticImageDeactivateScheduleActionSettings_layer :: Lens.Lens' StaticImageDeactivateScheduleActionSettings (Prelude.Maybe Prelude.Natural)
staticImageDeactivateScheduleActionSettings_layer = Lens.lens (\StaticImageDeactivateScheduleActionSettings' {layer} -> layer) (\s@StaticImageDeactivateScheduleActionSettings' {} a -> s {layer = a} :: StaticImageDeactivateScheduleActionSettings)

instance
  Data.FromJSON
    StaticImageDeactivateScheduleActionSettings
  where
  parseJSON =
    Data.withObject
      "StaticImageDeactivateScheduleActionSettings"
      ( \x ->
          StaticImageDeactivateScheduleActionSettings'
            Prelude.<$> (x Data..:? "fadeOut")
              Prelude.<*> (x Data..:? "layer")
      )

instance
  Prelude.Hashable
    StaticImageDeactivateScheduleActionSettings
  where
  hashWithSalt
    _salt
    StaticImageDeactivateScheduleActionSettings' {..} =
      _salt `Prelude.hashWithSalt` fadeOut
        `Prelude.hashWithSalt` layer

instance
  Prelude.NFData
    StaticImageDeactivateScheduleActionSettings
  where
  rnf StaticImageDeactivateScheduleActionSettings' {..} =
    Prelude.rnf fadeOut `Prelude.seq` Prelude.rnf layer

instance
  Data.ToJSON
    StaticImageDeactivateScheduleActionSettings
  where
  toJSON
    StaticImageDeactivateScheduleActionSettings' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("fadeOut" Data..=) Prelude.<$> fadeOut,
              ("layer" Data..=) Prelude.<$> layer
            ]
        )

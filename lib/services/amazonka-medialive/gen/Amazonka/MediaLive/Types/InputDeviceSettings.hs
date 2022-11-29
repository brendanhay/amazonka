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
-- Module      : Amazonka.MediaLive.Types.InputDeviceSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDeviceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings for an input device.
--
-- /See:/ 'newInputDeviceSettings' smart constructor.
data InputDeviceSettings = InputDeviceSettings'
  { -- | The unique ID for the device.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDeviceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'inputDeviceSettings_id' - The unique ID for the device.
newInputDeviceSettings ::
  InputDeviceSettings
newInputDeviceSettings =
  InputDeviceSettings' {id = Prelude.Nothing}

-- | The unique ID for the device.
inputDeviceSettings_id :: Lens.Lens' InputDeviceSettings (Prelude.Maybe Prelude.Text)
inputDeviceSettings_id = Lens.lens (\InputDeviceSettings' {id} -> id) (\s@InputDeviceSettings' {} a -> s {id = a} :: InputDeviceSettings)

instance Core.FromJSON InputDeviceSettings where
  parseJSON =
    Core.withObject
      "InputDeviceSettings"
      ( \x ->
          InputDeviceSettings' Prelude.<$> (x Core..:? "id")
      )

instance Prelude.Hashable InputDeviceSettings where
  hashWithSalt _salt InputDeviceSettings' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData InputDeviceSettings where
  rnf InputDeviceSettings' {..} = Prelude.rnf id

instance Core.ToJSON InputDeviceSettings where
  toJSON InputDeviceSettings' {..} =
    Core.object
      (Prelude.catMaybes [("id" Core..=) Prelude.<$> id])

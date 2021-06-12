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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings for an input device.
--
-- /See:/ 'newInputDeviceSettings' smart constructor.
data InputDeviceSettings = InputDeviceSettings'
  { -- | The unique ID for the device.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  InputDeviceSettings' {id = Core.Nothing}

-- | The unique ID for the device.
inputDeviceSettings_id :: Lens.Lens' InputDeviceSettings (Core.Maybe Core.Text)
inputDeviceSettings_id = Lens.lens (\InputDeviceSettings' {id} -> id) (\s@InputDeviceSettings' {} a -> s {id = a} :: InputDeviceSettings)

instance Core.FromJSON InputDeviceSettings where
  parseJSON =
    Core.withObject
      "InputDeviceSettings"
      ( \x ->
          InputDeviceSettings' Core.<$> (x Core..:? "id")
      )

instance Core.Hashable InputDeviceSettings

instance Core.NFData InputDeviceSettings

instance Core.ToJSON InputDeviceSettings where
  toJSON InputDeviceSettings' {..} =
    Core.object
      (Core.catMaybes [("id" Core..=) Core.<$> id])

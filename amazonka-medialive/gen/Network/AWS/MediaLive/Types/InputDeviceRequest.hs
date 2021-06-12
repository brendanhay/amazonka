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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Settings for an input device.
--
-- /See:/ 'newInputDeviceRequest' smart constructor.
data InputDeviceRequest = InputDeviceRequest'
  { -- | The unique ID for the device.
    id :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputDeviceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'inputDeviceRequest_id' - The unique ID for the device.
newInputDeviceRequest ::
  InputDeviceRequest
newInputDeviceRequest =
  InputDeviceRequest' {id = Core.Nothing}

-- | The unique ID for the device.
inputDeviceRequest_id :: Lens.Lens' InputDeviceRequest (Core.Maybe Core.Text)
inputDeviceRequest_id = Lens.lens (\InputDeviceRequest' {id} -> id) (\s@InputDeviceRequest' {} a -> s {id = a} :: InputDeviceRequest)

instance Core.Hashable InputDeviceRequest

instance Core.NFData InputDeviceRequest

instance Core.ToJSON InputDeviceRequest where
  toJSON InputDeviceRequest' {..} =
    Core.object
      (Core.catMaybes [("id" Core..=) Core.<$> id])

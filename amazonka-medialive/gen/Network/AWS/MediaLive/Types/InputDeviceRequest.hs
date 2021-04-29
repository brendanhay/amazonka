{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for an input device.
--
-- /See:/ 'newInputDeviceRequest' smart constructor.
data InputDeviceRequest = InputDeviceRequest'
  { -- | The unique ID for the device.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  InputDeviceRequest' {id = Prelude.Nothing}

-- | The unique ID for the device.
inputDeviceRequest_id :: Lens.Lens' InputDeviceRequest (Prelude.Maybe Prelude.Text)
inputDeviceRequest_id = Lens.lens (\InputDeviceRequest' {id} -> id) (\s@InputDeviceRequest' {} a -> s {id = a} :: InputDeviceRequest)

instance Prelude.Hashable InputDeviceRequest

instance Prelude.NFData InputDeviceRequest

instance Prelude.ToJSON InputDeviceRequest where
  toJSON InputDeviceRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("id" Prelude..=) Prelude.<$> id]
      )

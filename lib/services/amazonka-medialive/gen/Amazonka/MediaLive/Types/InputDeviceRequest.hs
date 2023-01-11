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
-- Module      : Amazonka.MediaLive.Types.InputDeviceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDeviceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for an input device.
--
-- /See:/ 'newInputDeviceRequest' smart constructor.
data InputDeviceRequest = InputDeviceRequest'
  { -- | The unique ID for the device.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.Hashable InputDeviceRequest where
  hashWithSalt _salt InputDeviceRequest' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData InputDeviceRequest where
  rnf InputDeviceRequest' {..} = Prelude.rnf id

instance Data.ToJSON InputDeviceRequest where
  toJSON InputDeviceRequest' {..} =
    Data.object
      (Prelude.catMaybes [("id" Data..=) Prelude.<$> id])

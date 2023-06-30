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
-- Module      : Amazonka.ECS.Types.PlatformDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.PlatformDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.PlatformDeviceType
import qualified Amazonka.Prelude as Prelude

-- | The devices that are available on the container instance. The only
-- supported device type is a GPU.
--
-- /See:/ 'newPlatformDevice' smart constructor.
data PlatformDevice = PlatformDevice'
  { -- | The ID for the GPUs on the container instance. The available GPU IDs can
    -- also be obtained on the container instance in the
    -- @\/var\/lib\/ecs\/gpu\/nvidia_gpu_info.json@ file.
    id :: Prelude.Text,
    -- | The type of device that\'s available on the container instance. The only
    -- supported value is @GPU@.
    type' :: PlatformDeviceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlatformDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'platformDevice_id' - The ID for the GPUs on the container instance. The available GPU IDs can
-- also be obtained on the container instance in the
-- @\/var\/lib\/ecs\/gpu\/nvidia_gpu_info.json@ file.
--
-- 'type'', 'platformDevice_type' - The type of device that\'s available on the container instance. The only
-- supported value is @GPU@.
newPlatformDevice ::
  -- | 'id'
  Prelude.Text ->
  -- | 'type''
  PlatformDeviceType ->
  PlatformDevice
newPlatformDevice pId_ pType_ =
  PlatformDevice' {id = pId_, type' = pType_}

-- | The ID for the GPUs on the container instance. The available GPU IDs can
-- also be obtained on the container instance in the
-- @\/var\/lib\/ecs\/gpu\/nvidia_gpu_info.json@ file.
platformDevice_id :: Lens.Lens' PlatformDevice Prelude.Text
platformDevice_id = Lens.lens (\PlatformDevice' {id} -> id) (\s@PlatformDevice' {} a -> s {id = a} :: PlatformDevice)

-- | The type of device that\'s available on the container instance. The only
-- supported value is @GPU@.
platformDevice_type :: Lens.Lens' PlatformDevice PlatformDeviceType
platformDevice_type = Lens.lens (\PlatformDevice' {type'} -> type') (\s@PlatformDevice' {} a -> s {type' = a} :: PlatformDevice)

instance Prelude.Hashable PlatformDevice where
  hashWithSalt _salt PlatformDevice' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PlatformDevice where
  rnf PlatformDevice' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON PlatformDevice where
  toJSON PlatformDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Data..= id),
            Prelude.Just ("type" Data..= type')
          ]
      )

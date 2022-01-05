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
-- Module      : Amazonka.Snowball.Types.OnDeviceServiceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.OnDeviceServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.NFSOnDeviceServiceConfiguration

-- | An object that represents metadata and configuration settings for
-- services on an AWS Snow Family device.
--
-- /See:/ 'newOnDeviceServiceConfiguration' smart constructor.
data OnDeviceServiceConfiguration = OnDeviceServiceConfiguration'
  { -- | Represents the NFS service on a Snow Family device.
    nFSOnDeviceService :: Prelude.Maybe NFSOnDeviceServiceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnDeviceServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nFSOnDeviceService', 'onDeviceServiceConfiguration_nFSOnDeviceService' - Represents the NFS service on a Snow Family device.
newOnDeviceServiceConfiguration ::
  OnDeviceServiceConfiguration
newOnDeviceServiceConfiguration =
  OnDeviceServiceConfiguration'
    { nFSOnDeviceService =
        Prelude.Nothing
    }

-- | Represents the NFS service on a Snow Family device.
onDeviceServiceConfiguration_nFSOnDeviceService :: Lens.Lens' OnDeviceServiceConfiguration (Prelude.Maybe NFSOnDeviceServiceConfiguration)
onDeviceServiceConfiguration_nFSOnDeviceService = Lens.lens (\OnDeviceServiceConfiguration' {nFSOnDeviceService} -> nFSOnDeviceService) (\s@OnDeviceServiceConfiguration' {} a -> s {nFSOnDeviceService = a} :: OnDeviceServiceConfiguration)

instance Core.FromJSON OnDeviceServiceConfiguration where
  parseJSON =
    Core.withObject
      "OnDeviceServiceConfiguration"
      ( \x ->
          OnDeviceServiceConfiguration'
            Prelude.<$> (x Core..:? "NFSOnDeviceService")
      )

instance
  Prelude.Hashable
    OnDeviceServiceConfiguration
  where
  hashWithSalt _salt OnDeviceServiceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` nFSOnDeviceService

instance Prelude.NFData OnDeviceServiceConfiguration where
  rnf OnDeviceServiceConfiguration' {..} =
    Prelude.rnf nFSOnDeviceService

instance Core.ToJSON OnDeviceServiceConfiguration where
  toJSON OnDeviceServiceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NFSOnDeviceService" Core..=)
              Prelude.<$> nFSOnDeviceService
          ]
      )

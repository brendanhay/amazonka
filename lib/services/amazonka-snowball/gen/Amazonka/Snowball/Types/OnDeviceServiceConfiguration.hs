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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.OnDeviceServiceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.NFSOnDeviceServiceConfiguration
import Amazonka.Snowball.Types.TGWOnDeviceServiceConfiguration

-- | An object that represents the metadata and configuration settings for
-- services on an Amazon Web Services Snow Family device.
--
-- /See:/ 'newOnDeviceServiceConfiguration' smart constructor.
data OnDeviceServiceConfiguration = OnDeviceServiceConfiguration'
  { -- | Represents the NFS (Network File System) service on a Snow Family
    -- device.
    nFSOnDeviceService :: Prelude.Maybe NFSOnDeviceServiceConfiguration,
    -- | Represents the Storage Gateway service Tape Gateway type on a Snow
    -- Family device.
    tGWOnDeviceService :: Prelude.Maybe TGWOnDeviceServiceConfiguration
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
-- 'nFSOnDeviceService', 'onDeviceServiceConfiguration_nFSOnDeviceService' - Represents the NFS (Network File System) service on a Snow Family
-- device.
--
-- 'tGWOnDeviceService', 'onDeviceServiceConfiguration_tGWOnDeviceService' - Represents the Storage Gateway service Tape Gateway type on a Snow
-- Family device.
newOnDeviceServiceConfiguration ::
  OnDeviceServiceConfiguration
newOnDeviceServiceConfiguration =
  OnDeviceServiceConfiguration'
    { nFSOnDeviceService =
        Prelude.Nothing,
      tGWOnDeviceService = Prelude.Nothing
    }

-- | Represents the NFS (Network File System) service on a Snow Family
-- device.
onDeviceServiceConfiguration_nFSOnDeviceService :: Lens.Lens' OnDeviceServiceConfiguration (Prelude.Maybe NFSOnDeviceServiceConfiguration)
onDeviceServiceConfiguration_nFSOnDeviceService = Lens.lens (\OnDeviceServiceConfiguration' {nFSOnDeviceService} -> nFSOnDeviceService) (\s@OnDeviceServiceConfiguration' {} a -> s {nFSOnDeviceService = a} :: OnDeviceServiceConfiguration)

-- | Represents the Storage Gateway service Tape Gateway type on a Snow
-- Family device.
onDeviceServiceConfiguration_tGWOnDeviceService :: Lens.Lens' OnDeviceServiceConfiguration (Prelude.Maybe TGWOnDeviceServiceConfiguration)
onDeviceServiceConfiguration_tGWOnDeviceService = Lens.lens (\OnDeviceServiceConfiguration' {tGWOnDeviceService} -> tGWOnDeviceService) (\s@OnDeviceServiceConfiguration' {} a -> s {tGWOnDeviceService = a} :: OnDeviceServiceConfiguration)

instance Data.FromJSON OnDeviceServiceConfiguration where
  parseJSON =
    Data.withObject
      "OnDeviceServiceConfiguration"
      ( \x ->
          OnDeviceServiceConfiguration'
            Prelude.<$> (x Data..:? "NFSOnDeviceService")
            Prelude.<*> (x Data..:? "TGWOnDeviceService")
      )

instance
  Prelude.Hashable
    OnDeviceServiceConfiguration
  where
  hashWithSalt _salt OnDeviceServiceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` nFSOnDeviceService
      `Prelude.hashWithSalt` tGWOnDeviceService

instance Prelude.NFData OnDeviceServiceConfiguration where
  rnf OnDeviceServiceConfiguration' {..} =
    Prelude.rnf nFSOnDeviceService
      `Prelude.seq` Prelude.rnf tGWOnDeviceService

instance Data.ToJSON OnDeviceServiceConfiguration where
  toJSON OnDeviceServiceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NFSOnDeviceService" Data..=)
              Prelude.<$> nFSOnDeviceService,
            ("TGWOnDeviceService" Data..=)
              Prelude.<$> tGWOnDeviceService
          ]
      )

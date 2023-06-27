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
import Amazonka.Snowball.Types.EKSOnDeviceServiceConfiguration
import Amazonka.Snowball.Types.NFSOnDeviceServiceConfiguration
import Amazonka.Snowball.Types.S3OnDeviceServiceConfiguration
import Amazonka.Snowball.Types.TGWOnDeviceServiceConfiguration

-- | An object that represents the metadata and configuration settings for
-- services on an Amazon Web Services Snow Family device.
--
-- /See:/ 'newOnDeviceServiceConfiguration' smart constructor.
data OnDeviceServiceConfiguration = OnDeviceServiceConfiguration'
  { -- | The configuration of EKS Anywhere on the Snow Family device.
    eKSOnDeviceService :: Prelude.Maybe EKSOnDeviceServiceConfiguration,
    -- | Represents the NFS (Network File System) service on a Snow Family
    -- device.
    nFSOnDeviceService :: Prelude.Maybe NFSOnDeviceServiceConfiguration,
    -- | Configuration for Amazon S3 compatible storage on Snow family devices.
    s3OnDeviceService :: Prelude.Maybe S3OnDeviceServiceConfiguration,
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
-- 'eKSOnDeviceService', 'onDeviceServiceConfiguration_eKSOnDeviceService' - The configuration of EKS Anywhere on the Snow Family device.
--
-- 'nFSOnDeviceService', 'onDeviceServiceConfiguration_nFSOnDeviceService' - Represents the NFS (Network File System) service on a Snow Family
-- device.
--
-- 's3OnDeviceService', 'onDeviceServiceConfiguration_s3OnDeviceService' - Configuration for Amazon S3 compatible storage on Snow family devices.
--
-- 'tGWOnDeviceService', 'onDeviceServiceConfiguration_tGWOnDeviceService' - Represents the Storage Gateway service Tape Gateway type on a Snow
-- Family device.
newOnDeviceServiceConfiguration ::
  OnDeviceServiceConfiguration
newOnDeviceServiceConfiguration =
  OnDeviceServiceConfiguration'
    { eKSOnDeviceService =
        Prelude.Nothing,
      nFSOnDeviceService = Prelude.Nothing,
      s3OnDeviceService = Prelude.Nothing,
      tGWOnDeviceService = Prelude.Nothing
    }

-- | The configuration of EKS Anywhere on the Snow Family device.
onDeviceServiceConfiguration_eKSOnDeviceService :: Lens.Lens' OnDeviceServiceConfiguration (Prelude.Maybe EKSOnDeviceServiceConfiguration)
onDeviceServiceConfiguration_eKSOnDeviceService = Lens.lens (\OnDeviceServiceConfiguration' {eKSOnDeviceService} -> eKSOnDeviceService) (\s@OnDeviceServiceConfiguration' {} a -> s {eKSOnDeviceService = a} :: OnDeviceServiceConfiguration)

-- | Represents the NFS (Network File System) service on a Snow Family
-- device.
onDeviceServiceConfiguration_nFSOnDeviceService :: Lens.Lens' OnDeviceServiceConfiguration (Prelude.Maybe NFSOnDeviceServiceConfiguration)
onDeviceServiceConfiguration_nFSOnDeviceService = Lens.lens (\OnDeviceServiceConfiguration' {nFSOnDeviceService} -> nFSOnDeviceService) (\s@OnDeviceServiceConfiguration' {} a -> s {nFSOnDeviceService = a} :: OnDeviceServiceConfiguration)

-- | Configuration for Amazon S3 compatible storage on Snow family devices.
onDeviceServiceConfiguration_s3OnDeviceService :: Lens.Lens' OnDeviceServiceConfiguration (Prelude.Maybe S3OnDeviceServiceConfiguration)
onDeviceServiceConfiguration_s3OnDeviceService = Lens.lens (\OnDeviceServiceConfiguration' {s3OnDeviceService} -> s3OnDeviceService) (\s@OnDeviceServiceConfiguration' {} a -> s {s3OnDeviceService = a} :: OnDeviceServiceConfiguration)

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
            Prelude.<$> (x Data..:? "EKSOnDeviceService")
            Prelude.<*> (x Data..:? "NFSOnDeviceService")
            Prelude.<*> (x Data..:? "S3OnDeviceService")
            Prelude.<*> (x Data..:? "TGWOnDeviceService")
      )

instance
  Prelude.Hashable
    OnDeviceServiceConfiguration
  where
  hashWithSalt _salt OnDeviceServiceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` eKSOnDeviceService
      `Prelude.hashWithSalt` nFSOnDeviceService
      `Prelude.hashWithSalt` s3OnDeviceService
      `Prelude.hashWithSalt` tGWOnDeviceService

instance Prelude.NFData OnDeviceServiceConfiguration where
  rnf OnDeviceServiceConfiguration' {..} =
    Prelude.rnf eKSOnDeviceService
      `Prelude.seq` Prelude.rnf nFSOnDeviceService
      `Prelude.seq` Prelude.rnf s3OnDeviceService
      `Prelude.seq` Prelude.rnf tGWOnDeviceService

instance Data.ToJSON OnDeviceServiceConfiguration where
  toJSON OnDeviceServiceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EKSOnDeviceService" Data..=)
              Prelude.<$> eKSOnDeviceService,
            ("NFSOnDeviceService" Data..=)
              Prelude.<$> nFSOnDeviceService,
            ("S3OnDeviceService" Data..=)
              Prelude.<$> s3OnDeviceService,
            ("TGWOnDeviceService" Data..=)
              Prelude.<$> tGWOnDeviceService
          ]
      )

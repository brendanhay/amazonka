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
-- Module      : Amazonka.SecurityLake.Types.DataLakeAutoEnableNewAccountConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeAutoEnableNewAccountConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AwsLogSourceResource

-- | Automatically enable new organization accounts as member accounts from
-- an Amazon Security Lake administrator account.
--
-- /See:/ 'newDataLakeAutoEnableNewAccountConfiguration' smart constructor.
data DataLakeAutoEnableNewAccountConfiguration = DataLakeAutoEnableNewAccountConfiguration'
  { -- | The Amazon Web Services Regions where Security Lake is automatically
    -- enabled.
    region :: Prelude.Text,
    -- | The Amazon Web Services sources that are automatically enabled in
    -- Security Lake.
    sources :: [AwsLogSourceResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeAutoEnableNewAccountConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'dataLakeAutoEnableNewAccountConfiguration_region' - The Amazon Web Services Regions where Security Lake is automatically
-- enabled.
--
-- 'sources', 'dataLakeAutoEnableNewAccountConfiguration_sources' - The Amazon Web Services sources that are automatically enabled in
-- Security Lake.
newDataLakeAutoEnableNewAccountConfiguration ::
  -- | 'region'
  Prelude.Text ->
  DataLakeAutoEnableNewAccountConfiguration
newDataLakeAutoEnableNewAccountConfiguration pRegion_ =
  DataLakeAutoEnableNewAccountConfiguration'
    { region =
        pRegion_,
      sources = Prelude.mempty
    }

-- | The Amazon Web Services Regions where Security Lake is automatically
-- enabled.
dataLakeAutoEnableNewAccountConfiguration_region :: Lens.Lens' DataLakeAutoEnableNewAccountConfiguration Prelude.Text
dataLakeAutoEnableNewAccountConfiguration_region = Lens.lens (\DataLakeAutoEnableNewAccountConfiguration' {region} -> region) (\s@DataLakeAutoEnableNewAccountConfiguration' {} a -> s {region = a} :: DataLakeAutoEnableNewAccountConfiguration)

-- | The Amazon Web Services sources that are automatically enabled in
-- Security Lake.
dataLakeAutoEnableNewAccountConfiguration_sources :: Lens.Lens' DataLakeAutoEnableNewAccountConfiguration [AwsLogSourceResource]
dataLakeAutoEnableNewAccountConfiguration_sources = Lens.lens (\DataLakeAutoEnableNewAccountConfiguration' {sources} -> sources) (\s@DataLakeAutoEnableNewAccountConfiguration' {} a -> s {sources = a} :: DataLakeAutoEnableNewAccountConfiguration) Prelude.. Lens.coerced

instance
  Data.FromJSON
    DataLakeAutoEnableNewAccountConfiguration
  where
  parseJSON =
    Data.withObject
      "DataLakeAutoEnableNewAccountConfiguration"
      ( \x ->
          DataLakeAutoEnableNewAccountConfiguration'
            Prelude.<$> (x Data..: "region")
            Prelude.<*> (x Data..:? "sources" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    DataLakeAutoEnableNewAccountConfiguration
  where
  hashWithSalt
    _salt
    DataLakeAutoEnableNewAccountConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` region
        `Prelude.hashWithSalt` sources

instance
  Prelude.NFData
    DataLakeAutoEnableNewAccountConfiguration
  where
  rnf DataLakeAutoEnableNewAccountConfiguration' {..} =
    Prelude.rnf region
      `Prelude.seq` Prelude.rnf sources

instance
  Data.ToJSON
    DataLakeAutoEnableNewAccountConfiguration
  where
  toJSON DataLakeAutoEnableNewAccountConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("region" Data..= region),
            Prelude.Just ("sources" Data..= sources)
          ]
      )

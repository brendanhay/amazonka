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
-- Module      : Amazonka.DataBrew.Types.Input
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Input where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.DataCatalogInputDefinition
import Amazonka.DataBrew.Types.DatabaseInputDefinition
import Amazonka.DataBrew.Types.Metadata
import Amazonka.DataBrew.Types.S3Location
import qualified Amazonka.Prelude as Prelude

-- | Represents information on how DataBrew can find data, in either the Glue
-- Data Catalog or Amazon S3.
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | The Glue Data Catalog parameters for the data.
    dataCatalogInputDefinition :: Prelude.Maybe DataCatalogInputDefinition,
    -- | Connection information for dataset input files stored in a database.
    databaseInputDefinition :: Prelude.Maybe DatabaseInputDefinition,
    -- | Contains additional resource information needed for specific datasets.
    metadata :: Prelude.Maybe Metadata,
    -- | The Amazon S3 location where the data is stored.
    s3InputDefinition :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataCatalogInputDefinition', 'input_dataCatalogInputDefinition' - The Glue Data Catalog parameters for the data.
--
-- 'databaseInputDefinition', 'input_databaseInputDefinition' - Connection information for dataset input files stored in a database.
--
-- 'metadata', 'input_metadata' - Contains additional resource information needed for specific datasets.
--
-- 's3InputDefinition', 'input_s3InputDefinition' - The Amazon S3 location where the data is stored.
newInput ::
  Input
newInput =
  Input'
    { dataCatalogInputDefinition =
        Prelude.Nothing,
      databaseInputDefinition = Prelude.Nothing,
      metadata = Prelude.Nothing,
      s3InputDefinition = Prelude.Nothing
    }

-- | The Glue Data Catalog parameters for the data.
input_dataCatalogInputDefinition :: Lens.Lens' Input (Prelude.Maybe DataCatalogInputDefinition)
input_dataCatalogInputDefinition = Lens.lens (\Input' {dataCatalogInputDefinition} -> dataCatalogInputDefinition) (\s@Input' {} a -> s {dataCatalogInputDefinition = a} :: Input)

-- | Connection information for dataset input files stored in a database.
input_databaseInputDefinition :: Lens.Lens' Input (Prelude.Maybe DatabaseInputDefinition)
input_databaseInputDefinition = Lens.lens (\Input' {databaseInputDefinition} -> databaseInputDefinition) (\s@Input' {} a -> s {databaseInputDefinition = a} :: Input)

-- | Contains additional resource information needed for specific datasets.
input_metadata :: Lens.Lens' Input (Prelude.Maybe Metadata)
input_metadata = Lens.lens (\Input' {metadata} -> metadata) (\s@Input' {} a -> s {metadata = a} :: Input)

-- | The Amazon S3 location where the data is stored.
input_s3InputDefinition :: Lens.Lens' Input (Prelude.Maybe S3Location)
input_s3InputDefinition = Lens.lens (\Input' {s3InputDefinition} -> s3InputDefinition) (\s@Input' {} a -> s {s3InputDefinition = a} :: Input)

instance Data.FromJSON Input where
  parseJSON =
    Data.withObject
      "Input"
      ( \x ->
          Input'
            Prelude.<$> (x Data..:? "DataCatalogInputDefinition")
            Prelude.<*> (x Data..:? "DatabaseInputDefinition")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "S3InputDefinition")
      )

instance Prelude.Hashable Input where
  hashWithSalt _salt Input' {..} =
    _salt
      `Prelude.hashWithSalt` dataCatalogInputDefinition
      `Prelude.hashWithSalt` databaseInputDefinition
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` s3InputDefinition

instance Prelude.NFData Input where
  rnf Input' {..} =
    Prelude.rnf dataCatalogInputDefinition
      `Prelude.seq` Prelude.rnf databaseInputDefinition
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf s3InputDefinition

instance Data.ToJSON Input where
  toJSON Input' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataCatalogInputDefinition" Data..=)
              Prelude.<$> dataCatalogInputDefinition,
            ("DatabaseInputDefinition" Data..=)
              Prelude.<$> databaseInputDefinition,
            ("Metadata" Data..=) Prelude.<$> metadata,
            ("S3InputDefinition" Data..=)
              Prelude.<$> s3InputDefinition
          ]
      )

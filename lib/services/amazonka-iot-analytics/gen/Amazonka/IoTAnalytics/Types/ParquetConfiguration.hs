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
-- Module      : Amazonka.IoTAnalytics.Types.ParquetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ParquetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.SchemaDefinition
import qualified Amazonka.Prelude as Prelude

-- | Contains the configuration information of the Parquet format.
--
-- /See:/ 'newParquetConfiguration' smart constructor.
data ParquetConfiguration = ParquetConfiguration'
  { -- | Information needed to define a schema.
    schemaDefinition :: Prelude.Maybe SchemaDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParquetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaDefinition', 'parquetConfiguration_schemaDefinition' - Information needed to define a schema.
newParquetConfiguration ::
  ParquetConfiguration
newParquetConfiguration =
  ParquetConfiguration'
    { schemaDefinition =
        Prelude.Nothing
    }

-- | Information needed to define a schema.
parquetConfiguration_schemaDefinition :: Lens.Lens' ParquetConfiguration (Prelude.Maybe SchemaDefinition)
parquetConfiguration_schemaDefinition = Lens.lens (\ParquetConfiguration' {schemaDefinition} -> schemaDefinition) (\s@ParquetConfiguration' {} a -> s {schemaDefinition = a} :: ParquetConfiguration)

instance Data.FromJSON ParquetConfiguration where
  parseJSON =
    Data.withObject
      "ParquetConfiguration"
      ( \x ->
          ParquetConfiguration'
            Prelude.<$> (x Data..:? "schemaDefinition")
      )

instance Prelude.Hashable ParquetConfiguration where
  hashWithSalt _salt ParquetConfiguration' {..} =
    _salt `Prelude.hashWithSalt` schemaDefinition

instance Prelude.NFData ParquetConfiguration where
  rnf ParquetConfiguration' {..} =
    Prelude.rnf schemaDefinition

instance Data.ToJSON ParquetConfiguration where
  toJSON ParquetConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("schemaDefinition" Data..=)
              Prelude.<$> schemaDefinition
          ]
      )

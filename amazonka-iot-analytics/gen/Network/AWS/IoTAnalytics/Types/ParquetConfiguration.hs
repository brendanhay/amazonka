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
-- Module      : Network.AWS.IoTAnalytics.Types.ParquetConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ParquetConfiguration where

import Network.AWS.IoTAnalytics.Types.SchemaDefinition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the configuration information of the Parquet format.
--
-- /See:/ 'newParquetConfiguration' smart constructor.
data ParquetConfiguration = ParquetConfiguration'
  { -- | Information needed to define a schema.
    schemaDefinition :: Prelude.Maybe SchemaDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON ParquetConfiguration where
  parseJSON =
    Prelude.withObject
      "ParquetConfiguration"
      ( \x ->
          ParquetConfiguration'
            Prelude.<$> (x Prelude..:? "schemaDefinition")
      )

instance Prelude.Hashable ParquetConfiguration

instance Prelude.NFData ParquetConfiguration

instance Prelude.ToJSON ParquetConfiguration where
  toJSON ParquetConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("schemaDefinition" Prelude..=)
              Prelude.<$> schemaDefinition
          ]
      )

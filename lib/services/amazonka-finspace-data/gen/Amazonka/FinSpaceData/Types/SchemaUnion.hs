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
-- Module      : Amazonka.FinSpaceData.Types.SchemaUnion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.SchemaUnion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpaceData.Types.SchemaDefinition
import qualified Amazonka.Prelude as Prelude

-- | A union of schema types.
--
-- /See:/ 'newSchemaUnion' smart constructor.
data SchemaUnion = SchemaUnion'
  { -- | The configuration for a schema on a tabular Dataset.
    tabularSchemaConfig :: Prelude.Maybe SchemaDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaUnion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tabularSchemaConfig', 'schemaUnion_tabularSchemaConfig' - The configuration for a schema on a tabular Dataset.
newSchemaUnion ::
  SchemaUnion
newSchemaUnion =
  SchemaUnion' {tabularSchemaConfig = Prelude.Nothing}

-- | The configuration for a schema on a tabular Dataset.
schemaUnion_tabularSchemaConfig :: Lens.Lens' SchemaUnion (Prelude.Maybe SchemaDefinition)
schemaUnion_tabularSchemaConfig = Lens.lens (\SchemaUnion' {tabularSchemaConfig} -> tabularSchemaConfig) (\s@SchemaUnion' {} a -> s {tabularSchemaConfig = a} :: SchemaUnion)

instance Core.FromJSON SchemaUnion where
  parseJSON =
    Core.withObject
      "SchemaUnion"
      ( \x ->
          SchemaUnion'
            Prelude.<$> (x Core..:? "tabularSchemaConfig")
      )

instance Prelude.Hashable SchemaUnion where
  hashWithSalt _salt SchemaUnion' {..} =
    _salt `Prelude.hashWithSalt` tabularSchemaConfig

instance Prelude.NFData SchemaUnion where
  rnf SchemaUnion' {..} =
    Prelude.rnf tabularSchemaConfig

instance Core.ToJSON SchemaUnion where
  toJSON SchemaUnion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tabularSchemaConfig" Core..=)
              Prelude.<$> tabularSchemaConfig
          ]
      )

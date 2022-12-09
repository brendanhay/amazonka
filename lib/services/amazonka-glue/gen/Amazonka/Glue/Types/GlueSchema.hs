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
-- Module      : Amazonka.Glue.Types.GlueSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GlueSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueStudioSchemaColumn
import qualified Amazonka.Prelude as Prelude

-- | Specifies a user-defined schema when a schema cannot be determined by
-- Glue.
--
-- /See:/ 'newGlueSchema' smart constructor.
data GlueSchema = GlueSchema'
  { -- | Specifies the column definitions that make up a Glue schema.
    columns :: Prelude.Maybe [GlueStudioSchemaColumn]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlueSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columns', 'glueSchema_columns' - Specifies the column definitions that make up a Glue schema.
newGlueSchema ::
  GlueSchema
newGlueSchema =
  GlueSchema' {columns = Prelude.Nothing}

-- | Specifies the column definitions that make up a Glue schema.
glueSchema_columns :: Lens.Lens' GlueSchema (Prelude.Maybe [GlueStudioSchemaColumn])
glueSchema_columns = Lens.lens (\GlueSchema' {columns} -> columns) (\s@GlueSchema' {} a -> s {columns = a} :: GlueSchema) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GlueSchema where
  parseJSON =
    Data.withObject
      "GlueSchema"
      ( \x ->
          GlueSchema'
            Prelude.<$> (x Data..:? "Columns" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable GlueSchema where
  hashWithSalt _salt GlueSchema' {..} =
    _salt `Prelude.hashWithSalt` columns

instance Prelude.NFData GlueSchema where
  rnf GlueSchema' {..} = Prelude.rnf columns

instance Data.ToJSON GlueSchema where
  toJSON GlueSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Columns" Data..=) Prelude.<$> columns]
      )

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
-- Module      : Network.AWS.IoTAnalytics.Types.SchemaDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.SchemaDefinition where

import Network.AWS.IoTAnalytics.Types.Column
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information needed to define a schema.
--
-- /See:/ 'newSchemaDefinition' smart constructor.
data SchemaDefinition = SchemaDefinition'
  { -- | Specifies one or more columns that store your data.
    --
    -- Each schema can have up to 100 columns. Each column can have up to 100
    -- nested types
    columns :: Prelude.Maybe [Column]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SchemaDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columns', 'schemaDefinition_columns' - Specifies one or more columns that store your data.
--
-- Each schema can have up to 100 columns. Each column can have up to 100
-- nested types
newSchemaDefinition ::
  SchemaDefinition
newSchemaDefinition =
  SchemaDefinition' {columns = Prelude.Nothing}

-- | Specifies one or more columns that store your data.
--
-- Each schema can have up to 100 columns. Each column can have up to 100
-- nested types
schemaDefinition_columns :: Lens.Lens' SchemaDefinition (Prelude.Maybe [Column])
schemaDefinition_columns = Lens.lens (\SchemaDefinition' {columns} -> columns) (\s@SchemaDefinition' {} a -> s {columns = a} :: SchemaDefinition) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON SchemaDefinition where
  parseJSON =
    Prelude.withObject
      "SchemaDefinition"
      ( \x ->
          SchemaDefinition'
            Prelude.<$> (x Prelude..:? "columns" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable SchemaDefinition

instance Prelude.NFData SchemaDefinition

instance Prelude.ToJSON SchemaDefinition where
  toJSON SchemaDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("columns" Prelude..=) Prelude.<$> columns]
      )

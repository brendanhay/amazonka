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

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.Column
import qualified Network.AWS.Lens as Lens

-- | Information needed to define a schema.
--
-- /See:/ 'newSchemaDefinition' smart constructor.
data SchemaDefinition = SchemaDefinition'
  { -- | Specifies one or more columns that store your data.
    --
    -- Each schema can have up to 100 columns. Each column can have up to 100
    -- nested types
    columns :: Core.Maybe [Column]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  SchemaDefinition' {columns = Core.Nothing}

-- | Specifies one or more columns that store your data.
--
-- Each schema can have up to 100 columns. Each column can have up to 100
-- nested types
schemaDefinition_columns :: Lens.Lens' SchemaDefinition (Core.Maybe [Column])
schemaDefinition_columns = Lens.lens (\SchemaDefinition' {columns} -> columns) (\s@SchemaDefinition' {} a -> s {columns = a} :: SchemaDefinition) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON SchemaDefinition where
  parseJSON =
    Core.withObject
      "SchemaDefinition"
      ( \x ->
          SchemaDefinition'
            Core.<$> (x Core..:? "columns" Core..!= Core.mempty)
      )

instance Core.Hashable SchemaDefinition

instance Core.NFData SchemaDefinition

instance Core.ToJSON SchemaDefinition where
  toJSON SchemaDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [("columns" Core..=) Core.<$> columns]
      )

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
-- Module      : Amazonka.Glue.Types.DataSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueTable
import qualified Amazonka.Prelude as Prelude

-- | A data source (an Glue table) for which you want data quality results.
--
-- /See:/ 'newDataSource' smart constructor.
data DataSource = DataSource'
  { -- | An Glue table.
    glueTable :: GlueTable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glueTable', 'dataSource_glueTable' - An Glue table.
newDataSource ::
  -- | 'glueTable'
  GlueTable ->
  DataSource
newDataSource pGlueTable_ =
  DataSource' {glueTable = pGlueTable_}

-- | An Glue table.
dataSource_glueTable :: Lens.Lens' DataSource GlueTable
dataSource_glueTable = Lens.lens (\DataSource' {glueTable} -> glueTable) (\s@DataSource' {} a -> s {glueTable = a} :: DataSource)

instance Data.FromJSON DataSource where
  parseJSON =
    Data.withObject
      "DataSource"
      ( \x ->
          DataSource' Prelude.<$> (x Data..: "GlueTable")
      )

instance Prelude.Hashable DataSource where
  hashWithSalt _salt DataSource' {..} =
    _salt `Prelude.hashWithSalt` glueTable

instance Prelude.NFData DataSource where
  rnf DataSource' {..} = Prelude.rnf glueTable

instance Data.ToJSON DataSource where
  toJSON DataSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GlueTable" Data..= glueTable)]
      )

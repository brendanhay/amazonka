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
-- Module      : Amazonka.Glue.Types.SparkSQL
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SparkSQL where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.GlueSchema
import Amazonka.Glue.Types.SqlAlias
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform where you enter a SQL query using Spark SQL syntax
-- to transform the data. The output is a single @DynamicFrame@.
--
-- /See:/ 'newSparkSQL' smart constructor.
data SparkSQL = SparkSQL'
  { -- | Specifies the data schema for the SparkSQL transform.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names. You can associate a
    -- table name with each input node to use in the SQL query. The name you
    -- choose must meet the Spark SQL naming restrictions.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | A SQL query that must use Spark SQL syntax and return a single data set.
    sqlQuery :: Prelude.Text,
    -- | A list of aliases. An alias allows you to specify what name to use in
    -- the SQL for a given input. For example, you have a datasource named
    -- \"MyDataSource\". If you specify @From@ as MyDataSource, and @Alias@ as
    -- SqlName, then in your SQL you can do:
    --
    -- @select * from SqlName@
    --
    -- and that gets data from MyDataSource.
    sqlAliases :: [SqlAlias]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SparkSQL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSchemas', 'sparkSQL_outputSchemas' - Specifies the data schema for the SparkSQL transform.
--
-- 'name', 'sparkSQL_name' - The name of the transform node.
--
-- 'inputs', 'sparkSQL_inputs' - The data inputs identified by their node names. You can associate a
-- table name with each input node to use in the SQL query. The name you
-- choose must meet the Spark SQL naming restrictions.
--
-- 'sqlQuery', 'sparkSQL_sqlQuery' - A SQL query that must use Spark SQL syntax and return a single data set.
--
-- 'sqlAliases', 'sparkSQL_sqlAliases' - A list of aliases. An alias allows you to specify what name to use in
-- the SQL for a given input. For example, you have a datasource named
-- \"MyDataSource\". If you specify @From@ as MyDataSource, and @Alias@ as
-- SqlName, then in your SQL you can do:
--
-- @select * from SqlName@
--
-- and that gets data from MyDataSource.
newSparkSQL ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'sqlQuery'
  Prelude.Text ->
  SparkSQL
newSparkSQL pName_ pInputs_ pSqlQuery_ =
  SparkSQL'
    { outputSchemas = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      sqlQuery = pSqlQuery_,
      sqlAliases = Prelude.mempty
    }

-- | Specifies the data schema for the SparkSQL transform.
sparkSQL_outputSchemas :: Lens.Lens' SparkSQL (Prelude.Maybe [GlueSchema])
sparkSQL_outputSchemas = Lens.lens (\SparkSQL' {outputSchemas} -> outputSchemas) (\s@SparkSQL' {} a -> s {outputSchemas = a} :: SparkSQL) Prelude.. Lens.mapping Lens.coerced

-- | The name of the transform node.
sparkSQL_name :: Lens.Lens' SparkSQL Prelude.Text
sparkSQL_name = Lens.lens (\SparkSQL' {name} -> name) (\s@SparkSQL' {} a -> s {name = a} :: SparkSQL)

-- | The data inputs identified by their node names. You can associate a
-- table name with each input node to use in the SQL query. The name you
-- choose must meet the Spark SQL naming restrictions.
sparkSQL_inputs :: Lens.Lens' SparkSQL (Prelude.NonEmpty Prelude.Text)
sparkSQL_inputs = Lens.lens (\SparkSQL' {inputs} -> inputs) (\s@SparkSQL' {} a -> s {inputs = a} :: SparkSQL) Prelude.. Lens.coerced

-- | A SQL query that must use Spark SQL syntax and return a single data set.
sparkSQL_sqlQuery :: Lens.Lens' SparkSQL Prelude.Text
sparkSQL_sqlQuery = Lens.lens (\SparkSQL' {sqlQuery} -> sqlQuery) (\s@SparkSQL' {} a -> s {sqlQuery = a} :: SparkSQL)

-- | A list of aliases. An alias allows you to specify what name to use in
-- the SQL for a given input. For example, you have a datasource named
-- \"MyDataSource\". If you specify @From@ as MyDataSource, and @Alias@ as
-- SqlName, then in your SQL you can do:
--
-- @select * from SqlName@
--
-- and that gets data from MyDataSource.
sparkSQL_sqlAliases :: Lens.Lens' SparkSQL [SqlAlias]
sparkSQL_sqlAliases = Lens.lens (\SparkSQL' {sqlAliases} -> sqlAliases) (\s@SparkSQL' {} a -> s {sqlAliases = a} :: SparkSQL) Prelude.. Lens.coerced

instance Core.FromJSON SparkSQL where
  parseJSON =
    Core.withObject
      "SparkSQL"
      ( \x ->
          SparkSQL'
            Prelude.<$> (x Core..:? "OutputSchemas" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Inputs")
            Prelude.<*> (x Core..: "SqlQuery")
            Prelude.<*> (x Core..:? "SqlAliases" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SparkSQL where
  hashWithSalt _salt SparkSQL' {..} =
    _salt `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` sqlQuery
      `Prelude.hashWithSalt` sqlAliases

instance Prelude.NFData SparkSQL where
  rnf SparkSQL' {..} =
    Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf sqlQuery
      `Prelude.seq` Prelude.rnf sqlAliases

instance Core.ToJSON SparkSQL where
  toJSON SparkSQL' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OutputSchemas" Core..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Inputs" Core..= inputs),
            Prelude.Just ("SqlQuery" Core..= sqlQuery),
            Prelude.Just ("SqlAliases" Core..= sqlAliases)
          ]
      )

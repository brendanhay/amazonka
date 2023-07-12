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
-- Module      : Amazonka.Glue.Types.JdbcTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JdbcTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.JdbcMetadataEntry
import qualified Amazonka.Prelude as Prelude

-- | Specifies a JDBC data store to crawl.
--
-- /See:/ 'newJdbcTarget' smart constructor.
data JdbcTarget = JdbcTarget'
  { -- | The name of the connection to use to connect to the JDBC target.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | Specify a value of @RAWTYPES@ or @COMMENTS@ to enable additional
    -- metadata in table responses. @RAWTYPES@ provides the native-level
    -- datatype. @COMMENTS@ provides comments associated with a column or table
    -- in the database.
    --
    -- If you do not need additional metadata, keep the field empty.
    enableAdditionalMetadata :: Prelude.Maybe [JdbcMetadataEntry],
    -- | A list of glob patterns used to exclude from the crawl. For more
    -- information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
    exclusions :: Prelude.Maybe [Prelude.Text],
    -- | The path of the JDBC target.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JdbcTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionName', 'jdbcTarget_connectionName' - The name of the connection to use to connect to the JDBC target.
--
-- 'enableAdditionalMetadata', 'jdbcTarget_enableAdditionalMetadata' - Specify a value of @RAWTYPES@ or @COMMENTS@ to enable additional
-- metadata in table responses. @RAWTYPES@ provides the native-level
-- datatype. @COMMENTS@ provides comments associated with a column or table
-- in the database.
--
-- If you do not need additional metadata, keep the field empty.
--
-- 'exclusions', 'jdbcTarget_exclusions' - A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
--
-- 'path', 'jdbcTarget_path' - The path of the JDBC target.
newJdbcTarget ::
  JdbcTarget
newJdbcTarget =
  JdbcTarget'
    { connectionName = Prelude.Nothing,
      enableAdditionalMetadata = Prelude.Nothing,
      exclusions = Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | The name of the connection to use to connect to the JDBC target.
jdbcTarget_connectionName :: Lens.Lens' JdbcTarget (Prelude.Maybe Prelude.Text)
jdbcTarget_connectionName = Lens.lens (\JdbcTarget' {connectionName} -> connectionName) (\s@JdbcTarget' {} a -> s {connectionName = a} :: JdbcTarget)

-- | Specify a value of @RAWTYPES@ or @COMMENTS@ to enable additional
-- metadata in table responses. @RAWTYPES@ provides the native-level
-- datatype. @COMMENTS@ provides comments associated with a column or table
-- in the database.
--
-- If you do not need additional metadata, keep the field empty.
jdbcTarget_enableAdditionalMetadata :: Lens.Lens' JdbcTarget (Prelude.Maybe [JdbcMetadataEntry])
jdbcTarget_enableAdditionalMetadata = Lens.lens (\JdbcTarget' {enableAdditionalMetadata} -> enableAdditionalMetadata) (\s@JdbcTarget' {} a -> s {enableAdditionalMetadata = a} :: JdbcTarget) Prelude.. Lens.mapping Lens.coerced

-- | A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
jdbcTarget_exclusions :: Lens.Lens' JdbcTarget (Prelude.Maybe [Prelude.Text])
jdbcTarget_exclusions = Lens.lens (\JdbcTarget' {exclusions} -> exclusions) (\s@JdbcTarget' {} a -> s {exclusions = a} :: JdbcTarget) Prelude.. Lens.mapping Lens.coerced

-- | The path of the JDBC target.
jdbcTarget_path :: Lens.Lens' JdbcTarget (Prelude.Maybe Prelude.Text)
jdbcTarget_path = Lens.lens (\JdbcTarget' {path} -> path) (\s@JdbcTarget' {} a -> s {path = a} :: JdbcTarget)

instance Data.FromJSON JdbcTarget where
  parseJSON =
    Data.withObject
      "JdbcTarget"
      ( \x ->
          JdbcTarget'
            Prelude.<$> (x Data..:? "ConnectionName")
            Prelude.<*> ( x
                            Data..:? "EnableAdditionalMetadata"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Exclusions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Path")
      )

instance Prelude.Hashable JdbcTarget where
  hashWithSalt _salt JdbcTarget' {..} =
    _salt
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` enableAdditionalMetadata
      `Prelude.hashWithSalt` exclusions
      `Prelude.hashWithSalt` path

instance Prelude.NFData JdbcTarget where
  rnf JdbcTarget' {..} =
    Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf enableAdditionalMetadata
      `Prelude.seq` Prelude.rnf exclusions
      `Prelude.seq` Prelude.rnf path

instance Data.ToJSON JdbcTarget where
  toJSON JdbcTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionName" Data..=)
              Prelude.<$> connectionName,
            ("EnableAdditionalMetadata" Data..=)
              Prelude.<$> enableAdditionalMetadata,
            ("Exclusions" Data..=) Prelude.<$> exclusions,
            ("Path" Data..=) Prelude.<$> path
          ]
      )

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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JdbcTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a JDBC data store to crawl.
--
-- /See:/ 'newJdbcTarget' smart constructor.
data JdbcTarget = JdbcTarget'
  { -- | The path of the JDBC target.
    path :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection to use to connect to the JDBC target.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | A list of glob patterns used to exclude from the crawl. For more
    -- information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
    exclusions :: Prelude.Maybe [Prelude.Text]
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
-- 'path', 'jdbcTarget_path' - The path of the JDBC target.
--
-- 'connectionName', 'jdbcTarget_connectionName' - The name of the connection to use to connect to the JDBC target.
--
-- 'exclusions', 'jdbcTarget_exclusions' - A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
newJdbcTarget ::
  JdbcTarget
newJdbcTarget =
  JdbcTarget'
    { path = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      exclusions = Prelude.Nothing
    }

-- | The path of the JDBC target.
jdbcTarget_path :: Lens.Lens' JdbcTarget (Prelude.Maybe Prelude.Text)
jdbcTarget_path = Lens.lens (\JdbcTarget' {path} -> path) (\s@JdbcTarget' {} a -> s {path = a} :: JdbcTarget)

-- | The name of the connection to use to connect to the JDBC target.
jdbcTarget_connectionName :: Lens.Lens' JdbcTarget (Prelude.Maybe Prelude.Text)
jdbcTarget_connectionName = Lens.lens (\JdbcTarget' {connectionName} -> connectionName) (\s@JdbcTarget' {} a -> s {connectionName = a} :: JdbcTarget)

-- | A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
jdbcTarget_exclusions :: Lens.Lens' JdbcTarget (Prelude.Maybe [Prelude.Text])
jdbcTarget_exclusions = Lens.lens (\JdbcTarget' {exclusions} -> exclusions) (\s@JdbcTarget' {} a -> s {exclusions = a} :: JdbcTarget) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON JdbcTarget where
  parseJSON =
    Core.withObject
      "JdbcTarget"
      ( \x ->
          JdbcTarget'
            Prelude.<$> (x Core..:? "Path")
            Prelude.<*> (x Core..:? "ConnectionName")
            Prelude.<*> (x Core..:? "Exclusions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable JdbcTarget

instance Prelude.NFData JdbcTarget

instance Core.ToJSON JdbcTarget where
  toJSON JdbcTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Path" Core..=) Prelude.<$> path,
            ("ConnectionName" Core..=)
              Prelude.<$> connectionName,
            ("Exclusions" Core..=) Prelude.<$> exclusions
          ]
      )

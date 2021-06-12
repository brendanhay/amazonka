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
-- Module      : Network.AWS.Glue.Types.JdbcTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JdbcTarget where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies a JDBC data store to crawl.
--
-- /See:/ 'newJdbcTarget' smart constructor.
data JdbcTarget = JdbcTarget'
  { -- | The name of the connection to use to connect to the JDBC target.
    connectionName :: Core.Maybe Core.Text,
    -- | A list of glob patterns used to exclude from the crawl. For more
    -- information, see
    -- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
    exclusions :: Core.Maybe [Core.Text],
    -- | The path of the JDBC target.
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'exclusions', 'jdbcTarget_exclusions' - A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
--
-- 'path', 'jdbcTarget_path' - The path of the JDBC target.
newJdbcTarget ::
  JdbcTarget
newJdbcTarget =
  JdbcTarget'
    { connectionName = Core.Nothing,
      exclusions = Core.Nothing,
      path = Core.Nothing
    }

-- | The name of the connection to use to connect to the JDBC target.
jdbcTarget_connectionName :: Lens.Lens' JdbcTarget (Core.Maybe Core.Text)
jdbcTarget_connectionName = Lens.lens (\JdbcTarget' {connectionName} -> connectionName) (\s@JdbcTarget' {} a -> s {connectionName = a} :: JdbcTarget)

-- | A list of glob patterns used to exclude from the crawl. For more
-- information, see
-- <https://docs.aws.amazon.com/glue/latest/dg/add-crawler.html Catalog Tables with a Crawler>.
jdbcTarget_exclusions :: Lens.Lens' JdbcTarget (Core.Maybe [Core.Text])
jdbcTarget_exclusions = Lens.lens (\JdbcTarget' {exclusions} -> exclusions) (\s@JdbcTarget' {} a -> s {exclusions = a} :: JdbcTarget) Core.. Lens.mapping Lens._Coerce

-- | The path of the JDBC target.
jdbcTarget_path :: Lens.Lens' JdbcTarget (Core.Maybe Core.Text)
jdbcTarget_path = Lens.lens (\JdbcTarget' {path} -> path) (\s@JdbcTarget' {} a -> s {path = a} :: JdbcTarget)

instance Core.FromJSON JdbcTarget where
  parseJSON =
    Core.withObject
      "JdbcTarget"
      ( \x ->
          JdbcTarget'
            Core.<$> (x Core..:? "ConnectionName")
            Core.<*> (x Core..:? "Exclusions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Path")
      )

instance Core.Hashable JdbcTarget

instance Core.NFData JdbcTarget

instance Core.ToJSON JdbcTarget where
  toJSON JdbcTarget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConnectionName" Core..=) Core.<$> connectionName,
            ("Exclusions" Core..=) Core.<$> exclusions,
            ("Path" Core..=) Core.<$> path
          ]
      )

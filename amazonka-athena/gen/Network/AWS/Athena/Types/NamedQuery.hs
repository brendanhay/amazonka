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
-- Module      : Network.AWS.Athena.Types.NamedQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.NamedQuery where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A query, where @QueryString@ is the list of SQL query statements that
-- comprise the query.
--
-- /See:/ 'newNamedQuery' smart constructor.
data NamedQuery = NamedQuery'
  { -- | The unique identifier of the query.
    namedQueryId :: Core.Maybe Core.Text,
    -- | The name of the workgroup that contains the named query.
    workGroup :: Core.Maybe Core.Text,
    -- | The query description.
    description :: Core.Maybe Core.Text,
    -- | The query name.
    name :: Core.Text,
    -- | The database to which the query belongs.
    database :: Core.Text,
    -- | The SQL query statements that comprise the query.
    queryString :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NamedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQueryId', 'namedQuery_namedQueryId' - The unique identifier of the query.
--
-- 'workGroup', 'namedQuery_workGroup' - The name of the workgroup that contains the named query.
--
-- 'description', 'namedQuery_description' - The query description.
--
-- 'name', 'namedQuery_name' - The query name.
--
-- 'database', 'namedQuery_database' - The database to which the query belongs.
--
-- 'queryString', 'namedQuery_queryString' - The SQL query statements that comprise the query.
newNamedQuery ::
  -- | 'name'
  Core.Text ->
  -- | 'database'
  Core.Text ->
  -- | 'queryString'
  Core.Text ->
  NamedQuery
newNamedQuery pName_ pDatabase_ pQueryString_ =
  NamedQuery'
    { namedQueryId = Core.Nothing,
      workGroup = Core.Nothing,
      description = Core.Nothing,
      name = pName_,
      database = pDatabase_,
      queryString = pQueryString_
    }

-- | The unique identifier of the query.
namedQuery_namedQueryId :: Lens.Lens' NamedQuery (Core.Maybe Core.Text)
namedQuery_namedQueryId = Lens.lens (\NamedQuery' {namedQueryId} -> namedQueryId) (\s@NamedQuery' {} a -> s {namedQueryId = a} :: NamedQuery)

-- | The name of the workgroup that contains the named query.
namedQuery_workGroup :: Lens.Lens' NamedQuery (Core.Maybe Core.Text)
namedQuery_workGroup = Lens.lens (\NamedQuery' {workGroup} -> workGroup) (\s@NamedQuery' {} a -> s {workGroup = a} :: NamedQuery)

-- | The query description.
namedQuery_description :: Lens.Lens' NamedQuery (Core.Maybe Core.Text)
namedQuery_description = Lens.lens (\NamedQuery' {description} -> description) (\s@NamedQuery' {} a -> s {description = a} :: NamedQuery)

-- | The query name.
namedQuery_name :: Lens.Lens' NamedQuery Core.Text
namedQuery_name = Lens.lens (\NamedQuery' {name} -> name) (\s@NamedQuery' {} a -> s {name = a} :: NamedQuery)

-- | The database to which the query belongs.
namedQuery_database :: Lens.Lens' NamedQuery Core.Text
namedQuery_database = Lens.lens (\NamedQuery' {database} -> database) (\s@NamedQuery' {} a -> s {database = a} :: NamedQuery)

-- | The SQL query statements that comprise the query.
namedQuery_queryString :: Lens.Lens' NamedQuery Core.Text
namedQuery_queryString = Lens.lens (\NamedQuery' {queryString} -> queryString) (\s@NamedQuery' {} a -> s {queryString = a} :: NamedQuery)

instance Core.FromJSON NamedQuery where
  parseJSON =
    Core.withObject
      "NamedQuery"
      ( \x ->
          NamedQuery'
            Core.<$> (x Core..:? "NamedQueryId")
            Core.<*> (x Core..:? "WorkGroup")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Database")
            Core.<*> (x Core..: "QueryString")
      )

instance Core.Hashable NamedQuery

instance Core.NFData NamedQuery

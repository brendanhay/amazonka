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
-- Module      : Amazonka.Kendra.Types.DataSourceGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data source information for user context filtering.
--
-- /See:/ 'newDataSourceGroup' smart constructor.
data DataSourceGroup = DataSourceGroup'
  { -- | The identifier of the group you want to add to your list of groups. This
    -- is for filtering search results based on the groups\' access to
    -- documents.
    groupId :: Prelude.Text,
    -- | The identifier of the data source group you want to add to your list of
    -- data source groups. This is for filtering search results based on the
    -- groups\' access to documents in that data source.
    dataSourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'dataSourceGroup_groupId' - The identifier of the group you want to add to your list of groups. This
-- is for filtering search results based on the groups\' access to
-- documents.
--
-- 'dataSourceId', 'dataSourceGroup_dataSourceId' - The identifier of the data source group you want to add to your list of
-- data source groups. This is for filtering search results based on the
-- groups\' access to documents in that data source.
newDataSourceGroup ::
  -- | 'groupId'
  Prelude.Text ->
  -- | 'dataSourceId'
  Prelude.Text ->
  DataSourceGroup
newDataSourceGroup pGroupId_ pDataSourceId_ =
  DataSourceGroup'
    { groupId = pGroupId_,
      dataSourceId = pDataSourceId_
    }

-- | The identifier of the group you want to add to your list of groups. This
-- is for filtering search results based on the groups\' access to
-- documents.
dataSourceGroup_groupId :: Lens.Lens' DataSourceGroup Prelude.Text
dataSourceGroup_groupId = Lens.lens (\DataSourceGroup' {groupId} -> groupId) (\s@DataSourceGroup' {} a -> s {groupId = a} :: DataSourceGroup)

-- | The identifier of the data source group you want to add to your list of
-- data source groups. This is for filtering search results based on the
-- groups\' access to documents in that data source.
dataSourceGroup_dataSourceId :: Lens.Lens' DataSourceGroup Prelude.Text
dataSourceGroup_dataSourceId = Lens.lens (\DataSourceGroup' {dataSourceId} -> dataSourceId) (\s@DataSourceGroup' {} a -> s {dataSourceId = a} :: DataSourceGroup)

instance Prelude.Hashable DataSourceGroup where
  hashWithSalt _salt DataSourceGroup' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` dataSourceId

instance Prelude.NFData DataSourceGroup where
  rnf DataSourceGroup' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf dataSourceId

instance Data.ToJSON DataSourceGroup where
  toJSON DataSourceGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("DataSourceId" Data..= dataSourceId)
          ]
      )

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
-- Module      : Amazonka.Kendra.Types.MemberGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.MemberGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The sub groups that belong to a group.
--
-- /See:/ 'newMemberGroup' smart constructor.
data MemberGroup = MemberGroup'
  { -- | The identifier of the data source for the sub group you want to map to a
    -- group.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the sub group you want to map to a group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'memberGroup_dataSourceId' - The identifier of the data source for the sub group you want to map to a
-- group.
--
-- 'groupId', 'memberGroup_groupId' - The identifier of the sub group you want to map to a group.
newMemberGroup ::
  -- | 'groupId'
  Prelude.Text ->
  MemberGroup
newMemberGroup pGroupId_ =
  MemberGroup'
    { dataSourceId = Prelude.Nothing,
      groupId = pGroupId_
    }

-- | The identifier of the data source for the sub group you want to map to a
-- group.
memberGroup_dataSourceId :: Lens.Lens' MemberGroup (Prelude.Maybe Prelude.Text)
memberGroup_dataSourceId = Lens.lens (\MemberGroup' {dataSourceId} -> dataSourceId) (\s@MemberGroup' {} a -> s {dataSourceId = a} :: MemberGroup)

-- | The identifier of the sub group you want to map to a group.
memberGroup_groupId :: Lens.Lens' MemberGroup Prelude.Text
memberGroup_groupId = Lens.lens (\MemberGroup' {groupId} -> groupId) (\s@MemberGroup' {} a -> s {groupId = a} :: MemberGroup)

instance Prelude.Hashable MemberGroup where
  hashWithSalt _salt MemberGroup' {..} =
    _salt `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData MemberGroup where
  rnf MemberGroup' {..} =
    Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToJSON MemberGroup where
  toJSON MemberGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceId" Data..=) Prelude.<$> dataSourceId,
            Prelude.Just ("GroupId" Data..= groupId)
          ]
      )

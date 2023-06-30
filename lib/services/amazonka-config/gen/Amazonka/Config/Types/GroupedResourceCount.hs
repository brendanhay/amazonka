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
-- Module      : Amazonka.Config.Types.GroupedResourceCount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.GroupedResourceCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The count of resources that are grouped by the group name.
--
-- /See:/ 'newGroupedResourceCount' smart constructor.
data GroupedResourceCount = GroupedResourceCount'
  { -- | The name of the group that can be region, account ID, or resource type.
    -- For example, region1, region2 if the region was chosen as @GroupByKey@.
    groupName :: Prelude.Text,
    -- | The number of resources in the group.
    resourceCount :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupedResourceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'groupedResourceCount_groupName' - The name of the group that can be region, account ID, or resource type.
-- For example, region1, region2 if the region was chosen as @GroupByKey@.
--
-- 'resourceCount', 'groupedResourceCount_resourceCount' - The number of resources in the group.
newGroupedResourceCount ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'resourceCount'
  Prelude.Integer ->
  GroupedResourceCount
newGroupedResourceCount pGroupName_ pResourceCount_ =
  GroupedResourceCount'
    { groupName = pGroupName_,
      resourceCount = pResourceCount_
    }

-- | The name of the group that can be region, account ID, or resource type.
-- For example, region1, region2 if the region was chosen as @GroupByKey@.
groupedResourceCount_groupName :: Lens.Lens' GroupedResourceCount Prelude.Text
groupedResourceCount_groupName = Lens.lens (\GroupedResourceCount' {groupName} -> groupName) (\s@GroupedResourceCount' {} a -> s {groupName = a} :: GroupedResourceCount)

-- | The number of resources in the group.
groupedResourceCount_resourceCount :: Lens.Lens' GroupedResourceCount Prelude.Integer
groupedResourceCount_resourceCount = Lens.lens (\GroupedResourceCount' {resourceCount} -> resourceCount) (\s@GroupedResourceCount' {} a -> s {resourceCount = a} :: GroupedResourceCount)

instance Data.FromJSON GroupedResourceCount where
  parseJSON =
    Data.withObject
      "GroupedResourceCount"
      ( \x ->
          GroupedResourceCount'
            Prelude.<$> (x Data..: "GroupName")
            Prelude.<*> (x Data..: "ResourceCount")
      )

instance Prelude.Hashable GroupedResourceCount where
  hashWithSalt _salt GroupedResourceCount' {..} =
    _salt
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` resourceCount

instance Prelude.NFData GroupedResourceCount where
  rnf GroupedResourceCount' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf resourceCount

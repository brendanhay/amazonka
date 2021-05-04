{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ResourceGroups.Types.GroupQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.GroupQuery where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ResourceGroups.Types.ResourceQuery

-- | A mapping of a query attached to a resource group that determines the
-- AWS resources that are members of the group.
--
-- /See:/ 'newGroupQuery' smart constructor.
data GroupQuery = GroupQuery'
  { -- | The name of the resource group that is associated with the specified
    -- resource query.
    groupName :: Prelude.Text,
    -- | The resource query that determines which AWS resources are members of
    -- the associated resource group.
    resourceQuery :: ResourceQuery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GroupQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'groupQuery_groupName' - The name of the resource group that is associated with the specified
-- resource query.
--
-- 'resourceQuery', 'groupQuery_resourceQuery' - The resource query that determines which AWS resources are members of
-- the associated resource group.
newGroupQuery ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'resourceQuery'
  ResourceQuery ->
  GroupQuery
newGroupQuery pGroupName_ pResourceQuery_ =
  GroupQuery'
    { groupName = pGroupName_,
      resourceQuery = pResourceQuery_
    }

-- | The name of the resource group that is associated with the specified
-- resource query.
groupQuery_groupName :: Lens.Lens' GroupQuery Prelude.Text
groupQuery_groupName = Lens.lens (\GroupQuery' {groupName} -> groupName) (\s@GroupQuery' {} a -> s {groupName = a} :: GroupQuery)

-- | The resource query that determines which AWS resources are members of
-- the associated resource group.
groupQuery_resourceQuery :: Lens.Lens' GroupQuery ResourceQuery
groupQuery_resourceQuery = Lens.lens (\GroupQuery' {resourceQuery} -> resourceQuery) (\s@GroupQuery' {} a -> s {resourceQuery = a} :: GroupQuery)

instance Prelude.FromJSON GroupQuery where
  parseJSON =
    Prelude.withObject
      "GroupQuery"
      ( \x ->
          GroupQuery'
            Prelude.<$> (x Prelude..: "GroupName")
            Prelude.<*> (x Prelude..: "ResourceQuery")
      )

instance Prelude.Hashable GroupQuery

instance Prelude.NFData GroupQuery

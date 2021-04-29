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
-- Module      : Network.AWS.ResourceGroups.Types.Group
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.Group where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A resource group that contains AWS resources. You can assign resources
-- to the group by associating either of the following elements with the
-- group:
--
-- -   ResourceQuery - Use a resource query to specify a set of tag keys
--     and values. All resources in the same AWS Region and AWS account
--     that have those keys with the same values are included in the group.
--     You can add a resource query when you create the group, or later by
--     using the PutGroupConfiguration operation.
--
-- -   GroupConfiguration - Use a service configuration to associate the
--     group with an AWS service. The configuration specifies which
--     resource types can be included in the group.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The description of the resource group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource group.
    groupArn :: Prelude.Text,
    -- | The name of the resource group.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'group_description' - The description of the resource group.
--
-- 'groupArn', 'group_groupArn' - The ARN of the resource group.
--
-- 'name', 'group_name' - The name of the resource group.
newGroup ::
  -- | 'groupArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Group
newGroup pGroupArn_ pName_ =
  Group'
    { description = Prelude.Nothing,
      groupArn = pGroupArn_,
      name = pName_
    }

-- | The description of the resource group.
group_description :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_description = Lens.lens (\Group' {description} -> description) (\s@Group' {} a -> s {description = a} :: Group)

-- | The ARN of the resource group.
group_groupArn :: Lens.Lens' Group Prelude.Text
group_groupArn = Lens.lens (\Group' {groupArn} -> groupArn) (\s@Group' {} a -> s {groupArn = a} :: Group)

-- | The name of the resource group.
group_name :: Lens.Lens' Group Prelude.Text
group_name = Lens.lens (\Group' {name} -> name) (\s@Group' {} a -> s {name = a} :: Group)

instance Prelude.FromJSON Group where
  parseJSON =
    Prelude.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..: "GroupArn")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable Group

instance Prelude.NFData Group

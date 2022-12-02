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
-- Module      : Amazonka.QuickSight.Types.Group
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Group where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A /group/ in Amazon QuickSight consists of a set of users. You can use
-- groups to make it easier to manage access and security.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The principal ID of the group.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The group description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principalId', 'group_principalId' - The principal ID of the group.
--
-- 'arn', 'group_arn' - The Amazon Resource Name (ARN) for the group.
--
-- 'groupName', 'group_groupName' - The name of the group.
--
-- 'description', 'group_description' - The group description.
newGroup ::
  Group
newGroup =
  Group'
    { principalId = Prelude.Nothing,
      arn = Prelude.Nothing,
      groupName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The principal ID of the group.
group_principalId :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_principalId = Lens.lens (\Group' {principalId} -> principalId) (\s@Group' {} a -> s {principalId = a} :: Group)

-- | The Amazon Resource Name (ARN) for the group.
group_arn :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_arn = Lens.lens (\Group' {arn} -> arn) (\s@Group' {} a -> s {arn = a} :: Group)

-- | The name of the group.
group_groupName :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_groupName = Lens.lens (\Group' {groupName} -> groupName) (\s@Group' {} a -> s {groupName = a} :: Group)

-- | The group description.
group_description :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_description = Lens.lens (\Group' {description} -> description) (\s@Group' {} a -> s {description = a} :: Group)

instance Data.FromJSON Group where
  parseJSON =
    Data.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Data..:? "PrincipalId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "GroupName")
            Prelude.<*> (x Data..:? "Description")
      )

instance Prelude.Hashable Group where
  hashWithSalt _salt Group' {..} =
    _salt `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` description

instance Prelude.NFData Group where
  rnf Group' {..} =
    Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf description

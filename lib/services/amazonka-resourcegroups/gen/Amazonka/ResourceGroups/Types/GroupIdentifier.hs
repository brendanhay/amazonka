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
-- Module      : Amazonka.ResourceGroups.Types.GroupIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.GroupIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The unique identifiers for a resource group.
--
-- /See:/ 'newGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { -- | The ARN of the resource group.
    groupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource group.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupArn', 'groupIdentifier_groupArn' - The ARN of the resource group.
--
-- 'groupName', 'groupIdentifier_groupName' - The name of the resource group.
newGroupIdentifier ::
  GroupIdentifier
newGroupIdentifier =
  GroupIdentifier'
    { groupArn = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The ARN of the resource group.
groupIdentifier_groupArn :: Lens.Lens' GroupIdentifier (Prelude.Maybe Prelude.Text)
groupIdentifier_groupArn = Lens.lens (\GroupIdentifier' {groupArn} -> groupArn) (\s@GroupIdentifier' {} a -> s {groupArn = a} :: GroupIdentifier)

-- | The name of the resource group.
groupIdentifier_groupName :: Lens.Lens' GroupIdentifier (Prelude.Maybe Prelude.Text)
groupIdentifier_groupName = Lens.lens (\GroupIdentifier' {groupName} -> groupName) (\s@GroupIdentifier' {} a -> s {groupName = a} :: GroupIdentifier)

instance Data.FromJSON GroupIdentifier where
  parseJSON =
    Data.withObject
      "GroupIdentifier"
      ( \x ->
          GroupIdentifier'
            Prelude.<$> (x Data..:? "GroupArn")
            Prelude.<*> (x Data..:? "GroupName")
      )

instance Prelude.Hashable GroupIdentifier where
  hashWithSalt _salt GroupIdentifier' {..} =
    _salt `Prelude.hashWithSalt` groupArn
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData GroupIdentifier where
  rnf GroupIdentifier' {..} =
    Prelude.rnf groupArn
      `Prelude.seq` Prelude.rnf groupName

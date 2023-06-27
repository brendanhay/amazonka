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
-- Module      : Amazonka.IoT.Types.GroupNameAndArn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.GroupNameAndArn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name and ARN of a group.
--
-- /See:/ 'newGroupNameAndArn' smart constructor.
data GroupNameAndArn = GroupNameAndArn'
  { -- | The group ARN.
    groupArn :: Prelude.Maybe Prelude.Text,
    -- | The group name.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupNameAndArn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupArn', 'groupNameAndArn_groupArn' - The group ARN.
--
-- 'groupName', 'groupNameAndArn_groupName' - The group name.
newGroupNameAndArn ::
  GroupNameAndArn
newGroupNameAndArn =
  GroupNameAndArn'
    { groupArn = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The group ARN.
groupNameAndArn_groupArn :: Lens.Lens' GroupNameAndArn (Prelude.Maybe Prelude.Text)
groupNameAndArn_groupArn = Lens.lens (\GroupNameAndArn' {groupArn} -> groupArn) (\s@GroupNameAndArn' {} a -> s {groupArn = a} :: GroupNameAndArn)

-- | The group name.
groupNameAndArn_groupName :: Lens.Lens' GroupNameAndArn (Prelude.Maybe Prelude.Text)
groupNameAndArn_groupName = Lens.lens (\GroupNameAndArn' {groupName} -> groupName) (\s@GroupNameAndArn' {} a -> s {groupName = a} :: GroupNameAndArn)

instance Data.FromJSON GroupNameAndArn where
  parseJSON =
    Data.withObject
      "GroupNameAndArn"
      ( \x ->
          GroupNameAndArn'
            Prelude.<$> (x Data..:? "groupArn")
            Prelude.<*> (x Data..:? "groupName")
      )

instance Prelude.Hashable GroupNameAndArn where
  hashWithSalt _salt GroupNameAndArn' {..} =
    _salt
      `Prelude.hashWithSalt` groupArn
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData GroupNameAndArn where
  rnf GroupNameAndArn' {..} =
    Prelude.rnf groupArn
      `Prelude.seq` Prelude.rnf groupName

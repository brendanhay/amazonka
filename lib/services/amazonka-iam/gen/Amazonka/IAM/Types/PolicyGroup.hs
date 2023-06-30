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
-- Module      : Amazonka.IAM.Types.PolicyGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.PolicyGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a group that a managed policy is attached to.
--
-- This data type is used as a response element in the
-- ListEntitiesForPolicy operation.
--
-- For more information about managed policies, refer to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- /See:/ 'newPolicyGroup' smart constructor.
data PolicyGroup = PolicyGroup'
  { -- | The stable and unique string identifying the group. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The name (friendly name, not ARN) identifying the group.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'policyGroup_groupId' - The stable and unique string identifying the group. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'groupName', 'policyGroup_groupName' - The name (friendly name, not ARN) identifying the group.
newPolicyGroup ::
  PolicyGroup
newPolicyGroup =
  PolicyGroup'
    { groupId = Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | The stable and unique string identifying the group. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
policyGroup_groupId :: Lens.Lens' PolicyGroup (Prelude.Maybe Prelude.Text)
policyGroup_groupId = Lens.lens (\PolicyGroup' {groupId} -> groupId) (\s@PolicyGroup' {} a -> s {groupId = a} :: PolicyGroup)

-- | The name (friendly name, not ARN) identifying the group.
policyGroup_groupName :: Lens.Lens' PolicyGroup (Prelude.Maybe Prelude.Text)
policyGroup_groupName = Lens.lens (\PolicyGroup' {groupName} -> groupName) (\s@PolicyGroup' {} a -> s {groupName = a} :: PolicyGroup)

instance Data.FromXML PolicyGroup where
  parseXML x =
    PolicyGroup'
      Prelude.<$> (x Data..@? "GroupId")
      Prelude.<*> (x Data..@? "GroupName")

instance Prelude.Hashable PolicyGroup where
  hashWithSalt _salt PolicyGroup' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupName

instance Prelude.NFData PolicyGroup where
  rnf PolicyGroup' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName

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
-- Module      : Amazonka.IAM.Types.GroupDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.GroupDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IAM.Types.AttachedPolicy
import Amazonka.IAM.Types.PolicyDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an IAM group, including all of the group\'s
-- policies.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newGroupDetail' smart constructor.
data GroupDetail = GroupDetail'
  { arn :: Prelude.Maybe Prelude.Text,
    -- | The path to the group. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text,
    -- | The friendly name that identifies the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | A list of the managed policies attached to the group.
    attachedManagedPolicies :: Prelude.Maybe [AttachedPolicy],
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- group was created.
    createDate :: Prelude.Maybe Core.ISO8601,
    -- | The stable and unique string identifying the group. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | A list of the inline policies embedded in the group.
    groupPolicyList :: Prelude.Maybe [PolicyDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'groupDetail_arn' - Undocumented member.
--
-- 'path', 'groupDetail_path' - The path to the group. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'groupName', 'groupDetail_groupName' - The friendly name that identifies the group.
--
-- 'attachedManagedPolicies', 'groupDetail_attachedManagedPolicies' - A list of the managed policies attached to the group.
--
-- 'createDate', 'groupDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
--
-- 'groupId', 'groupDetail_groupId' - The stable and unique string identifying the group. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'groupPolicyList', 'groupDetail_groupPolicyList' - A list of the inline policies embedded in the group.
newGroupDetail ::
  GroupDetail
newGroupDetail =
  GroupDetail'
    { arn = Prelude.Nothing,
      path = Prelude.Nothing,
      groupName = Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing,
      createDate = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupPolicyList = Prelude.Nothing
    }

-- | Undocumented member.
groupDetail_arn :: Lens.Lens' GroupDetail (Prelude.Maybe Prelude.Text)
groupDetail_arn = Lens.lens (\GroupDetail' {arn} -> arn) (\s@GroupDetail' {} a -> s {arn = a} :: GroupDetail)

-- | The path to the group. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
groupDetail_path :: Lens.Lens' GroupDetail (Prelude.Maybe Prelude.Text)
groupDetail_path = Lens.lens (\GroupDetail' {path} -> path) (\s@GroupDetail' {} a -> s {path = a} :: GroupDetail)

-- | The friendly name that identifies the group.
groupDetail_groupName :: Lens.Lens' GroupDetail (Prelude.Maybe Prelude.Text)
groupDetail_groupName = Lens.lens (\GroupDetail' {groupName} -> groupName) (\s@GroupDetail' {} a -> s {groupName = a} :: GroupDetail)

-- | A list of the managed policies attached to the group.
groupDetail_attachedManagedPolicies :: Lens.Lens' GroupDetail (Prelude.Maybe [AttachedPolicy])
groupDetail_attachedManagedPolicies = Lens.lens (\GroupDetail' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@GroupDetail' {} a -> s {attachedManagedPolicies = a} :: GroupDetail) Prelude.. Lens.mapping Lens.coerced

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
groupDetail_createDate :: Lens.Lens' GroupDetail (Prelude.Maybe Prelude.UTCTime)
groupDetail_createDate = Lens.lens (\GroupDetail' {createDate} -> createDate) (\s@GroupDetail' {} a -> s {createDate = a} :: GroupDetail) Prelude.. Lens.mapping Core._Time

-- | The stable and unique string identifying the group. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
groupDetail_groupId :: Lens.Lens' GroupDetail (Prelude.Maybe Prelude.Text)
groupDetail_groupId = Lens.lens (\GroupDetail' {groupId} -> groupId) (\s@GroupDetail' {} a -> s {groupId = a} :: GroupDetail)

-- | A list of the inline policies embedded in the group.
groupDetail_groupPolicyList :: Lens.Lens' GroupDetail (Prelude.Maybe [PolicyDetail])
groupDetail_groupPolicyList = Lens.lens (\GroupDetail' {groupPolicyList} -> groupPolicyList) (\s@GroupDetail' {} a -> s {groupPolicyList = a} :: GroupDetail) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML GroupDetail where
  parseXML x =
    GroupDetail'
      Prelude.<$> (x Core..@? "Arn")
      Prelude.<*> (x Core..@? "Path")
      Prelude.<*> (x Core..@? "GroupName")
      Prelude.<*> ( x Core..@? "AttachedManagedPolicies"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "CreateDate")
      Prelude.<*> (x Core..@? "GroupId")
      Prelude.<*> ( x Core..@? "GroupPolicyList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable GroupDetail where
  hashWithSalt _salt GroupDetail' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` groupPolicyList

instance Prelude.NFData GroupDetail where
  rnf GroupDetail' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupPolicyList

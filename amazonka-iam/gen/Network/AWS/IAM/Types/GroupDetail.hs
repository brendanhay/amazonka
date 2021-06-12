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
-- Module      : Network.AWS.IAM.Types.GroupDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.GroupDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.PolicyDetail
import qualified Network.AWS.Lens as Lens

-- | Contains information about an IAM group, including all of the group\'s
-- policies.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newGroupDetail' smart constructor.
data GroupDetail = GroupDetail'
  { -- | A list of the managed policies attached to the group.
    attachedManagedPolicies :: Core.Maybe [AttachedPolicy],
    -- | The friendly name that identifies the group.
    groupName :: Core.Maybe Core.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- group was created.
    createDate :: Core.Maybe Core.ISO8601,
    arn :: Core.Maybe Core.Text,
    -- | The stable and unique string identifying the group. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    groupId :: Core.Maybe Core.Text,
    -- | A list of the inline policies embedded in the group.
    groupPolicyList :: Core.Maybe [PolicyDetail],
    -- | The path to the group. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedManagedPolicies', 'groupDetail_attachedManagedPolicies' - A list of the managed policies attached to the group.
--
-- 'groupName', 'groupDetail_groupName' - The friendly name that identifies the group.
--
-- 'createDate', 'groupDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
--
-- 'arn', 'groupDetail_arn' - Undocumented member.
--
-- 'groupId', 'groupDetail_groupId' - The stable and unique string identifying the group. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'groupPolicyList', 'groupDetail_groupPolicyList' - A list of the inline policies embedded in the group.
--
-- 'path', 'groupDetail_path' - The path to the group. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
newGroupDetail ::
  GroupDetail
newGroupDetail =
  GroupDetail'
    { attachedManagedPolicies =
        Core.Nothing,
      groupName = Core.Nothing,
      createDate = Core.Nothing,
      arn = Core.Nothing,
      groupId = Core.Nothing,
      groupPolicyList = Core.Nothing,
      path = Core.Nothing
    }

-- | A list of the managed policies attached to the group.
groupDetail_attachedManagedPolicies :: Lens.Lens' GroupDetail (Core.Maybe [AttachedPolicy])
groupDetail_attachedManagedPolicies = Lens.lens (\GroupDetail' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@GroupDetail' {} a -> s {attachedManagedPolicies = a} :: GroupDetail) Core.. Lens.mapping Lens._Coerce

-- | The friendly name that identifies the group.
groupDetail_groupName :: Lens.Lens' GroupDetail (Core.Maybe Core.Text)
groupDetail_groupName = Lens.lens (\GroupDetail' {groupName} -> groupName) (\s@GroupDetail' {} a -> s {groupName = a} :: GroupDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- group was created.
groupDetail_createDate :: Lens.Lens' GroupDetail (Core.Maybe Core.UTCTime)
groupDetail_createDate = Lens.lens (\GroupDetail' {createDate} -> createDate) (\s@GroupDetail' {} a -> s {createDate = a} :: GroupDetail) Core.. Lens.mapping Core._Time

-- | Undocumented member.
groupDetail_arn :: Lens.Lens' GroupDetail (Core.Maybe Core.Text)
groupDetail_arn = Lens.lens (\GroupDetail' {arn} -> arn) (\s@GroupDetail' {} a -> s {arn = a} :: GroupDetail)

-- | The stable and unique string identifying the group. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
groupDetail_groupId :: Lens.Lens' GroupDetail (Core.Maybe Core.Text)
groupDetail_groupId = Lens.lens (\GroupDetail' {groupId} -> groupId) (\s@GroupDetail' {} a -> s {groupId = a} :: GroupDetail)

-- | A list of the inline policies embedded in the group.
groupDetail_groupPolicyList :: Lens.Lens' GroupDetail (Core.Maybe [PolicyDetail])
groupDetail_groupPolicyList = Lens.lens (\GroupDetail' {groupPolicyList} -> groupPolicyList) (\s@GroupDetail' {} a -> s {groupPolicyList = a} :: GroupDetail) Core.. Lens.mapping Lens._Coerce

-- | The path to the group. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
groupDetail_path :: Lens.Lens' GroupDetail (Core.Maybe Core.Text)
groupDetail_path = Lens.lens (\GroupDetail' {path} -> path) (\s@GroupDetail' {} a -> s {path = a} :: GroupDetail)

instance Core.FromXML GroupDetail where
  parseXML x =
    GroupDetail'
      Core.<$> ( x Core..@? "AttachedManagedPolicies"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "GroupName")
      Core.<*> (x Core..@? "CreateDate")
      Core.<*> (x Core..@? "Arn")
      Core.<*> (x Core..@? "GroupId")
      Core.<*> ( x Core..@? "GroupPolicyList" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Path")

instance Core.Hashable GroupDetail

instance Core.NFData GroupDetail

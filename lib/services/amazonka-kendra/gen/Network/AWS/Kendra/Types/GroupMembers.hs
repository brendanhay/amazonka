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
-- Module      : Network.AWS.Kendra.Types.GroupMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.GroupMembers where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.MemberGroup
import Network.AWS.Kendra.Types.MemberUser
import Network.AWS.Kendra.Types.S3Path
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A list of users or sub groups that belong to a group. Users and groups
-- are useful for filtering search results to different users based on
-- their group\'s access to documents.
--
-- /See:/ 'newGroupMembers' smart constructor.
data GroupMembers = GroupMembers'
  { -- | If you have more than 1000 users and\/or sub groups for a single group,
    -- you need to provide the path to the S3 file that lists your users and
    -- sub groups for a group. Your sub groups can contain more than 1000
    -- users, but the list of sub groups that belong to a group (and\/or users)
    -- must be no more than 1000.
    --
    -- You can download this
    -- <https://docs.aws.amazon.com/kendra/latest/dg/samples/group_members.zip example S3 file>
    -- that uses the correct format for listing group members. Note,
    -- @dataSourceId@ is optional. The value of @type@ for a group is always
    -- @GROUP@ and for a user it is always @USER@.
    s3PathforGroupMembers :: Prelude.Maybe S3Path,
    -- | A list of sub groups that belong to a group. For example, the sub groups
    -- \"Research\", \"Engineering\", and \"Sales and Marketing\" all belong to
    -- the group \"Company\".
    memberGroups :: Prelude.Maybe (Prelude.NonEmpty MemberGroup),
    -- | A list of users that belong to a group. For example, a list of interns
    -- all belong to the \"Interns\" group.
    memberUsers :: Prelude.Maybe (Prelude.NonEmpty MemberUser)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3PathforGroupMembers', 'groupMembers_s3PathforGroupMembers' - If you have more than 1000 users and\/or sub groups for a single group,
-- you need to provide the path to the S3 file that lists your users and
-- sub groups for a group. Your sub groups can contain more than 1000
-- users, but the list of sub groups that belong to a group (and\/or users)
-- must be no more than 1000.
--
-- You can download this
-- <https://docs.aws.amazon.com/kendra/latest/dg/samples/group_members.zip example S3 file>
-- that uses the correct format for listing group members. Note,
-- @dataSourceId@ is optional. The value of @type@ for a group is always
-- @GROUP@ and for a user it is always @USER@.
--
-- 'memberGroups', 'groupMembers_memberGroups' - A list of sub groups that belong to a group. For example, the sub groups
-- \"Research\", \"Engineering\", and \"Sales and Marketing\" all belong to
-- the group \"Company\".
--
-- 'memberUsers', 'groupMembers_memberUsers' - A list of users that belong to a group. For example, a list of interns
-- all belong to the \"Interns\" group.
newGroupMembers ::
  GroupMembers
newGroupMembers =
  GroupMembers'
    { s3PathforGroupMembers =
        Prelude.Nothing,
      memberGroups = Prelude.Nothing,
      memberUsers = Prelude.Nothing
    }

-- | If you have more than 1000 users and\/or sub groups for a single group,
-- you need to provide the path to the S3 file that lists your users and
-- sub groups for a group. Your sub groups can contain more than 1000
-- users, but the list of sub groups that belong to a group (and\/or users)
-- must be no more than 1000.
--
-- You can download this
-- <https://docs.aws.amazon.com/kendra/latest/dg/samples/group_members.zip example S3 file>
-- that uses the correct format for listing group members. Note,
-- @dataSourceId@ is optional. The value of @type@ for a group is always
-- @GROUP@ and for a user it is always @USER@.
groupMembers_s3PathforGroupMembers :: Lens.Lens' GroupMembers (Prelude.Maybe S3Path)
groupMembers_s3PathforGroupMembers = Lens.lens (\GroupMembers' {s3PathforGroupMembers} -> s3PathforGroupMembers) (\s@GroupMembers' {} a -> s {s3PathforGroupMembers = a} :: GroupMembers)

-- | A list of sub groups that belong to a group. For example, the sub groups
-- \"Research\", \"Engineering\", and \"Sales and Marketing\" all belong to
-- the group \"Company\".
groupMembers_memberGroups :: Lens.Lens' GroupMembers (Prelude.Maybe (Prelude.NonEmpty MemberGroup))
groupMembers_memberGroups = Lens.lens (\GroupMembers' {memberGroups} -> memberGroups) (\s@GroupMembers' {} a -> s {memberGroups = a} :: GroupMembers) Prelude.. Lens.mapping Lens.coerced

-- | A list of users that belong to a group. For example, a list of interns
-- all belong to the \"Interns\" group.
groupMembers_memberUsers :: Lens.Lens' GroupMembers (Prelude.Maybe (Prelude.NonEmpty MemberUser))
groupMembers_memberUsers = Lens.lens (\GroupMembers' {memberUsers} -> memberUsers) (\s@GroupMembers' {} a -> s {memberUsers = a} :: GroupMembers) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable GroupMembers

instance Prelude.NFData GroupMembers

instance Core.ToJSON GroupMembers where
  toJSON GroupMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("S3PathforGroupMembers" Core..=)
              Prelude.<$> s3PathforGroupMembers,
            ("MemberGroups" Core..=) Prelude.<$> memberGroups,
            ("MemberUsers" Core..=) Prelude.<$> memberUsers
          ]
      )

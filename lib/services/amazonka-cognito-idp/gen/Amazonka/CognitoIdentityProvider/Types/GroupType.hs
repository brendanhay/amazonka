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
-- Module      : Amazonka.CognitoIdentityProvider.Types.GroupType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.GroupType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The group type.
--
-- /See:/ 'newGroupType' smart constructor.
data GroupType = GroupType'
  { -- | The role Amazon Resource Name (ARN) for the group.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The date the group was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The date the group was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | A string containing the description of the group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID for the user pool.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | A non-negative integer value that specifies the precedence of this group
    -- relative to the other groups that a user can belong to in the user pool.
    -- Zero is the highest precedence value. Groups with lower @Precedence@
    -- values take precedence over groups with higher ornull @Precedence@
    -- values. If a user belongs to two or more groups, it is the group with
    -- the lowest precedence value whose role ARN is given in the user\'s
    -- tokens for the @cognito:roles@ and @cognito:preferred_role@ claims.
    --
    -- Two groups can have the same @Precedence@ value. If this happens,
    -- neither group takes precedence over the other. If two groups with the
    -- same @Precedence@ have the same role ARN, that role is used in the
    -- @cognito:preferred_role@ claim in tokens for users in each group. If the
    -- two groups have different role ARNs, the @cognito:preferred_role@ claim
    -- isn\'t set in users\' tokens.
    --
    -- The default @Precedence@ value is null.
    precedence :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'groupType_roleArn' - The role Amazon Resource Name (ARN) for the group.
--
-- 'lastModifiedDate', 'groupType_lastModifiedDate' - The date the group was last modified.
--
-- 'creationDate', 'groupType_creationDate' - The date the group was created.
--
-- 'groupName', 'groupType_groupName' - The name of the group.
--
-- 'description', 'groupType_description' - A string containing the description of the group.
--
-- 'userPoolId', 'groupType_userPoolId' - The user pool ID for the user pool.
--
-- 'precedence', 'groupType_precedence' - A non-negative integer value that specifies the precedence of this group
-- relative to the other groups that a user can belong to in the user pool.
-- Zero is the highest precedence value. Groups with lower @Precedence@
-- values take precedence over groups with higher ornull @Precedence@
-- values. If a user belongs to two or more groups, it is the group with
-- the lowest precedence value whose role ARN is given in the user\'s
-- tokens for the @cognito:roles@ and @cognito:preferred_role@ claims.
--
-- Two groups can have the same @Precedence@ value. If this happens,
-- neither group takes precedence over the other. If two groups with the
-- same @Precedence@ have the same role ARN, that role is used in the
-- @cognito:preferred_role@ claim in tokens for users in each group. If the
-- two groups have different role ARNs, the @cognito:preferred_role@ claim
-- isn\'t set in users\' tokens.
--
-- The default @Precedence@ value is null.
newGroupType ::
  GroupType
newGroupType =
  GroupType'
    { roleArn = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      groupName = Prelude.Nothing,
      description = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      precedence = Prelude.Nothing
    }

-- | The role Amazon Resource Name (ARN) for the group.
groupType_roleArn :: Lens.Lens' GroupType (Prelude.Maybe Prelude.Text)
groupType_roleArn = Lens.lens (\GroupType' {roleArn} -> roleArn) (\s@GroupType' {} a -> s {roleArn = a} :: GroupType)

-- | The date the group was last modified.
groupType_lastModifiedDate :: Lens.Lens' GroupType (Prelude.Maybe Prelude.UTCTime)
groupType_lastModifiedDate = Lens.lens (\GroupType' {lastModifiedDate} -> lastModifiedDate) (\s@GroupType' {} a -> s {lastModifiedDate = a} :: GroupType) Prelude.. Lens.mapping Core._Time

-- | The date the group was created.
groupType_creationDate :: Lens.Lens' GroupType (Prelude.Maybe Prelude.UTCTime)
groupType_creationDate = Lens.lens (\GroupType' {creationDate} -> creationDate) (\s@GroupType' {} a -> s {creationDate = a} :: GroupType) Prelude.. Lens.mapping Core._Time

-- | The name of the group.
groupType_groupName :: Lens.Lens' GroupType (Prelude.Maybe Prelude.Text)
groupType_groupName = Lens.lens (\GroupType' {groupName} -> groupName) (\s@GroupType' {} a -> s {groupName = a} :: GroupType)

-- | A string containing the description of the group.
groupType_description :: Lens.Lens' GroupType (Prelude.Maybe Prelude.Text)
groupType_description = Lens.lens (\GroupType' {description} -> description) (\s@GroupType' {} a -> s {description = a} :: GroupType)

-- | The user pool ID for the user pool.
groupType_userPoolId :: Lens.Lens' GroupType (Prelude.Maybe Prelude.Text)
groupType_userPoolId = Lens.lens (\GroupType' {userPoolId} -> userPoolId) (\s@GroupType' {} a -> s {userPoolId = a} :: GroupType)

-- | A non-negative integer value that specifies the precedence of this group
-- relative to the other groups that a user can belong to in the user pool.
-- Zero is the highest precedence value. Groups with lower @Precedence@
-- values take precedence over groups with higher ornull @Precedence@
-- values. If a user belongs to two or more groups, it is the group with
-- the lowest precedence value whose role ARN is given in the user\'s
-- tokens for the @cognito:roles@ and @cognito:preferred_role@ claims.
--
-- Two groups can have the same @Precedence@ value. If this happens,
-- neither group takes precedence over the other. If two groups with the
-- same @Precedence@ have the same role ARN, that role is used in the
-- @cognito:preferred_role@ claim in tokens for users in each group. If the
-- two groups have different role ARNs, the @cognito:preferred_role@ claim
-- isn\'t set in users\' tokens.
--
-- The default @Precedence@ value is null.
groupType_precedence :: Lens.Lens' GroupType (Prelude.Maybe Prelude.Natural)
groupType_precedence = Lens.lens (\GroupType' {precedence} -> precedence) (\s@GroupType' {} a -> s {precedence = a} :: GroupType)

instance Core.FromJSON GroupType where
  parseJSON =
    Core.withObject
      "GroupType"
      ( \x ->
          GroupType'
            Prelude.<$> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "GroupName")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "UserPoolId")
            Prelude.<*> (x Core..:? "Precedence")
      )

instance Prelude.Hashable GroupType where
  hashWithSalt _salt GroupType' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` precedence

instance Prelude.NFData GroupType where
  rnf GroupType' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf precedence

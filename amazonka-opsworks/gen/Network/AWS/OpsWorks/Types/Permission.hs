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
-- Module      : Network.AWS.OpsWorks.Types.Permission
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Permission where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes stack or user permissions.
--
-- /See:/ 'newPermission' smart constructor.
data Permission = Permission'
  { -- | Whether the user can use __sudo__.
    allowSudo :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
    -- (IAM) role. For more information about IAM ARNs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
    iamUserArn :: Prelude.Maybe Prelude.Text,
    -- | A stack ID.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | Whether the user can use SSH.
    allowSsh :: Prelude.Maybe Prelude.Bool,
    -- | The user\'s permission level, which must be the following:
    --
    -- -   @deny@
    --
    -- -   @show@
    --
    -- -   @deploy@
    --
    -- -   @manage@
    --
    -- -   @iam_only@
    --
    -- For more information on the permissions associated with these levels,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
    level :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Permission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowSudo', 'permission_allowSudo' - Whether the user can use __sudo__.
--
-- 'iamUserArn', 'permission_iamUserArn' - The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
--
-- 'stackId', 'permission_stackId' - A stack ID.
--
-- 'allowSsh', 'permission_allowSsh' - Whether the user can use SSH.
--
-- 'level', 'permission_level' - The user\'s permission level, which must be the following:
--
-- -   @deny@
--
-- -   @show@
--
-- -   @deploy@
--
-- -   @manage@
--
-- -   @iam_only@
--
-- For more information on the permissions associated with these levels,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
newPermission ::
  Permission
newPermission =
  Permission'
    { allowSudo = Prelude.Nothing,
      iamUserArn = Prelude.Nothing,
      stackId = Prelude.Nothing,
      allowSsh = Prelude.Nothing,
      level = Prelude.Nothing
    }

-- | Whether the user can use __sudo__.
permission_allowSudo :: Lens.Lens' Permission (Prelude.Maybe Prelude.Bool)
permission_allowSudo = Lens.lens (\Permission' {allowSudo} -> allowSudo) (\s@Permission' {} a -> s {allowSudo = a} :: Permission)

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
permission_iamUserArn :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_iamUserArn = Lens.lens (\Permission' {iamUserArn} -> iamUserArn) (\s@Permission' {} a -> s {iamUserArn = a} :: Permission)

-- | A stack ID.
permission_stackId :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_stackId = Lens.lens (\Permission' {stackId} -> stackId) (\s@Permission' {} a -> s {stackId = a} :: Permission)

-- | Whether the user can use SSH.
permission_allowSsh :: Lens.Lens' Permission (Prelude.Maybe Prelude.Bool)
permission_allowSsh = Lens.lens (\Permission' {allowSsh} -> allowSsh) (\s@Permission' {} a -> s {allowSsh = a} :: Permission)

-- | The user\'s permission level, which must be the following:
--
-- -   @deny@
--
-- -   @show@
--
-- -   @deploy@
--
-- -   @manage@
--
-- -   @iam_only@
--
-- For more information on the permissions associated with these levels,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
permission_level :: Lens.Lens' Permission (Prelude.Maybe Prelude.Text)
permission_level = Lens.lens (\Permission' {level} -> level) (\s@Permission' {} a -> s {level = a} :: Permission)

instance Prelude.FromJSON Permission where
  parseJSON =
    Prelude.withObject
      "Permission"
      ( \x ->
          Permission'
            Prelude.<$> (x Prelude..:? "AllowSudo")
            Prelude.<*> (x Prelude..:? "IamUserArn")
            Prelude.<*> (x Prelude..:? "StackId")
            Prelude.<*> (x Prelude..:? "AllowSsh")
            Prelude.<*> (x Prelude..:? "Level")
      )

instance Prelude.Hashable Permission

instance Prelude.NFData Permission

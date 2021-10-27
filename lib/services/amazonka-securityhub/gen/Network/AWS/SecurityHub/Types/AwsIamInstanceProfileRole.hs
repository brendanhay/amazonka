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
-- Module      : Network.AWS.SecurityHub.Types.AwsIamInstanceProfileRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsIamInstanceProfileRole where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a role associated with an instance profile.
--
-- /See:/ 'newAwsIamInstanceProfileRole' smart constructor.
data AwsIamInstanceProfileRole = AwsIamInstanceProfileRole'
  { -- | The policy that grants an entity permission to assume the role.
    assumeRolePolicyDocument :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The path to the role.
    path :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the role was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The name of the role.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the role.
    roleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamInstanceProfileRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assumeRolePolicyDocument', 'awsIamInstanceProfileRole_assumeRolePolicyDocument' - The policy that grants an entity permission to assume the role.
--
-- 'arn', 'awsIamInstanceProfileRole_arn' - The ARN of the role.
--
-- 'path', 'awsIamInstanceProfileRole_path' - The path to the role.
--
-- 'createDate', 'awsIamInstanceProfileRole_createDate' - Indicates when the role was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'roleName', 'awsIamInstanceProfileRole_roleName' - The name of the role.
--
-- 'roleId', 'awsIamInstanceProfileRole_roleId' - The identifier of the role.
newAwsIamInstanceProfileRole ::
  AwsIamInstanceProfileRole
newAwsIamInstanceProfileRole =
  AwsIamInstanceProfileRole'
    { assumeRolePolicyDocument =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      path = Prelude.Nothing,
      createDate = Prelude.Nothing,
      roleName = Prelude.Nothing,
      roleId = Prelude.Nothing
    }

-- | The policy that grants an entity permission to assume the role.
awsIamInstanceProfileRole_assumeRolePolicyDocument :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_assumeRolePolicyDocument = Lens.lens (\AwsIamInstanceProfileRole' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@AwsIamInstanceProfileRole' {} a -> s {assumeRolePolicyDocument = a} :: AwsIamInstanceProfileRole)

-- | The ARN of the role.
awsIamInstanceProfileRole_arn :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_arn = Lens.lens (\AwsIamInstanceProfileRole' {arn} -> arn) (\s@AwsIamInstanceProfileRole' {} a -> s {arn = a} :: AwsIamInstanceProfileRole)

-- | The path to the role.
awsIamInstanceProfileRole_path :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_path = Lens.lens (\AwsIamInstanceProfileRole' {path} -> path) (\s@AwsIamInstanceProfileRole' {} a -> s {path = a} :: AwsIamInstanceProfileRole)

-- | Indicates when the role was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamInstanceProfileRole_createDate :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_createDate = Lens.lens (\AwsIamInstanceProfileRole' {createDate} -> createDate) (\s@AwsIamInstanceProfileRole' {} a -> s {createDate = a} :: AwsIamInstanceProfileRole)

-- | The name of the role.
awsIamInstanceProfileRole_roleName :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_roleName = Lens.lens (\AwsIamInstanceProfileRole' {roleName} -> roleName) (\s@AwsIamInstanceProfileRole' {} a -> s {roleName = a} :: AwsIamInstanceProfileRole)

-- | The identifier of the role.
awsIamInstanceProfileRole_roleId :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_roleId = Lens.lens (\AwsIamInstanceProfileRole' {roleId} -> roleId) (\s@AwsIamInstanceProfileRole' {} a -> s {roleId = a} :: AwsIamInstanceProfileRole)

instance Core.FromJSON AwsIamInstanceProfileRole where
  parseJSON =
    Core.withObject
      "AwsIamInstanceProfileRole"
      ( \x ->
          AwsIamInstanceProfileRole'
            Prelude.<$> (x Core..:? "AssumeRolePolicyDocument")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Path")
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> (x Core..:? "RoleName")
            Prelude.<*> (x Core..:? "RoleId")
      )

instance Prelude.Hashable AwsIamInstanceProfileRole

instance Prelude.NFData AwsIamInstanceProfileRole

instance Core.ToJSON AwsIamInstanceProfileRole where
  toJSON AwsIamInstanceProfileRole' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AssumeRolePolicyDocument" Core..=)
              Prelude.<$> assumeRolePolicyDocument,
            ("Arn" Core..=) Prelude.<$> arn,
            ("Path" Core..=) Prelude.<$> path,
            ("CreateDate" Core..=) Prelude.<$> createDate,
            ("RoleName" Core..=) Prelude.<$> roleName,
            ("RoleId" Core..=) Prelude.<$> roleId
          ]
      )

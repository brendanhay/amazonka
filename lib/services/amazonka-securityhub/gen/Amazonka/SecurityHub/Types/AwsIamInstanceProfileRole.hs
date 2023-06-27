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
-- Module      : Amazonka.SecurityHub.Types.AwsIamInstanceProfileRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamInstanceProfileRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a role associated with an instance profile.
--
-- /See:/ 'newAwsIamInstanceProfileRole' smart constructor.
data AwsIamInstanceProfileRole = AwsIamInstanceProfileRole'
  { -- | The ARN of the role.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The policy that grants an entity permission to assume the role.
    assumeRolePolicyDocument :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the role was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The path to the role.
    path :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the role.
    roleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the role.
    roleName :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'awsIamInstanceProfileRole_arn' - The ARN of the role.
--
-- 'assumeRolePolicyDocument', 'awsIamInstanceProfileRole_assumeRolePolicyDocument' - The policy that grants an entity permission to assume the role.
--
-- 'createDate', 'awsIamInstanceProfileRole_createDate' - Indicates when the role was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
--
-- 'path', 'awsIamInstanceProfileRole_path' - The path to the role.
--
-- 'roleId', 'awsIamInstanceProfileRole_roleId' - The identifier of the role.
--
-- 'roleName', 'awsIamInstanceProfileRole_roleName' - The name of the role.
newAwsIamInstanceProfileRole ::
  AwsIamInstanceProfileRole
newAwsIamInstanceProfileRole =
  AwsIamInstanceProfileRole'
    { arn = Prelude.Nothing,
      assumeRolePolicyDocument = Prelude.Nothing,
      createDate = Prelude.Nothing,
      path = Prelude.Nothing,
      roleId = Prelude.Nothing,
      roleName = Prelude.Nothing
    }

-- | The ARN of the role.
awsIamInstanceProfileRole_arn :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_arn = Lens.lens (\AwsIamInstanceProfileRole' {arn} -> arn) (\s@AwsIamInstanceProfileRole' {} a -> s {arn = a} :: AwsIamInstanceProfileRole)

-- | The policy that grants an entity permission to assume the role.
awsIamInstanceProfileRole_assumeRolePolicyDocument :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_assumeRolePolicyDocument = Lens.lens (\AwsIamInstanceProfileRole' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@AwsIamInstanceProfileRole' {} a -> s {assumeRolePolicyDocument = a} :: AwsIamInstanceProfileRole)

-- | Indicates when the role was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
awsIamInstanceProfileRole_createDate :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_createDate = Lens.lens (\AwsIamInstanceProfileRole' {createDate} -> createDate) (\s@AwsIamInstanceProfileRole' {} a -> s {createDate = a} :: AwsIamInstanceProfileRole)

-- | The path to the role.
awsIamInstanceProfileRole_path :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_path = Lens.lens (\AwsIamInstanceProfileRole' {path} -> path) (\s@AwsIamInstanceProfileRole' {} a -> s {path = a} :: AwsIamInstanceProfileRole)

-- | The identifier of the role.
awsIamInstanceProfileRole_roleId :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_roleId = Lens.lens (\AwsIamInstanceProfileRole' {roleId} -> roleId) (\s@AwsIamInstanceProfileRole' {} a -> s {roleId = a} :: AwsIamInstanceProfileRole)

-- | The name of the role.
awsIamInstanceProfileRole_roleName :: Lens.Lens' AwsIamInstanceProfileRole (Prelude.Maybe Prelude.Text)
awsIamInstanceProfileRole_roleName = Lens.lens (\AwsIamInstanceProfileRole' {roleName} -> roleName) (\s@AwsIamInstanceProfileRole' {} a -> s {roleName = a} :: AwsIamInstanceProfileRole)

instance Data.FromJSON AwsIamInstanceProfileRole where
  parseJSON =
    Data.withObject
      "AwsIamInstanceProfileRole"
      ( \x ->
          AwsIamInstanceProfileRole'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "AssumeRolePolicyDocument")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "RoleId")
            Prelude.<*> (x Data..:? "RoleName")
      )

instance Prelude.Hashable AwsIamInstanceProfileRole where
  hashWithSalt _salt AwsIamInstanceProfileRole' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` assumeRolePolicyDocument
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` roleId
      `Prelude.hashWithSalt` roleName

instance Prelude.NFData AwsIamInstanceProfileRole where
  rnf AwsIamInstanceProfileRole' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf assumeRolePolicyDocument
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf roleId
      `Prelude.seq` Prelude.rnf roleName

instance Data.ToJSON AwsIamInstanceProfileRole where
  toJSON AwsIamInstanceProfileRole' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arn" Data..=) Prelude.<$> arn,
            ("AssumeRolePolicyDocument" Data..=)
              Prelude.<$> assumeRolePolicyDocument,
            ("CreateDate" Data..=) Prelude.<$> createDate,
            ("Path" Data..=) Prelude.<$> path,
            ("RoleId" Data..=) Prelude.<$> roleId,
            ("RoleName" Data..=) Prelude.<$> roleName
          ]
      )

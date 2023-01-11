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
-- Module      : Amazonka.SecurityHub.Types.AwsIamInstanceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamInstanceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamInstanceProfileRole

-- | Information about an instance profile.
--
-- /See:/ 'newAwsIamInstanceProfile' smart constructor.
data AwsIamInstanceProfile = AwsIamInstanceProfile'
  { -- | The ARN of the instance profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the instance profile was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the instance profile.
    instanceProfileId :: Prelude.Maybe Prelude.Text,
    -- | The name of the instance profile.
    instanceProfileName :: Prelude.Maybe Prelude.Text,
    -- | The path to the instance profile.
    path :: Prelude.Maybe Prelude.Text,
    -- | The roles associated with the instance profile.
    roles :: Prelude.Maybe [AwsIamInstanceProfileRole]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'awsIamInstanceProfile_arn' - The ARN of the instance profile.
--
-- 'createDate', 'awsIamInstanceProfile_createDate' - Indicates when the instance profile was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'instanceProfileId', 'awsIamInstanceProfile_instanceProfileId' - The identifier of the instance profile.
--
-- 'instanceProfileName', 'awsIamInstanceProfile_instanceProfileName' - The name of the instance profile.
--
-- 'path', 'awsIamInstanceProfile_path' - The path to the instance profile.
--
-- 'roles', 'awsIamInstanceProfile_roles' - The roles associated with the instance profile.
newAwsIamInstanceProfile ::
  AwsIamInstanceProfile
newAwsIamInstanceProfile =
  AwsIamInstanceProfile'
    { arn = Prelude.Nothing,
      createDate = Prelude.Nothing,
      instanceProfileId = Prelude.Nothing,
      instanceProfileName = Prelude.Nothing,
      path = Prelude.Nothing,
      roles = Prelude.Nothing
    }

-- | The ARN of the instance profile.
awsIamInstanceProfile_arn :: Lens.Lens' AwsIamInstanceProfile (Prelude.Maybe Prelude.Text)
awsIamInstanceProfile_arn = Lens.lens (\AwsIamInstanceProfile' {arn} -> arn) (\s@AwsIamInstanceProfile' {} a -> s {arn = a} :: AwsIamInstanceProfile)

-- | Indicates when the instance profile was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamInstanceProfile_createDate :: Lens.Lens' AwsIamInstanceProfile (Prelude.Maybe Prelude.Text)
awsIamInstanceProfile_createDate = Lens.lens (\AwsIamInstanceProfile' {createDate} -> createDate) (\s@AwsIamInstanceProfile' {} a -> s {createDate = a} :: AwsIamInstanceProfile)

-- | The identifier of the instance profile.
awsIamInstanceProfile_instanceProfileId :: Lens.Lens' AwsIamInstanceProfile (Prelude.Maybe Prelude.Text)
awsIamInstanceProfile_instanceProfileId = Lens.lens (\AwsIamInstanceProfile' {instanceProfileId} -> instanceProfileId) (\s@AwsIamInstanceProfile' {} a -> s {instanceProfileId = a} :: AwsIamInstanceProfile)

-- | The name of the instance profile.
awsIamInstanceProfile_instanceProfileName :: Lens.Lens' AwsIamInstanceProfile (Prelude.Maybe Prelude.Text)
awsIamInstanceProfile_instanceProfileName = Lens.lens (\AwsIamInstanceProfile' {instanceProfileName} -> instanceProfileName) (\s@AwsIamInstanceProfile' {} a -> s {instanceProfileName = a} :: AwsIamInstanceProfile)

-- | The path to the instance profile.
awsIamInstanceProfile_path :: Lens.Lens' AwsIamInstanceProfile (Prelude.Maybe Prelude.Text)
awsIamInstanceProfile_path = Lens.lens (\AwsIamInstanceProfile' {path} -> path) (\s@AwsIamInstanceProfile' {} a -> s {path = a} :: AwsIamInstanceProfile)

-- | The roles associated with the instance profile.
awsIamInstanceProfile_roles :: Lens.Lens' AwsIamInstanceProfile (Prelude.Maybe [AwsIamInstanceProfileRole])
awsIamInstanceProfile_roles = Lens.lens (\AwsIamInstanceProfile' {roles} -> roles) (\s@AwsIamInstanceProfile' {} a -> s {roles = a} :: AwsIamInstanceProfile) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsIamInstanceProfile where
  parseJSON =
    Data.withObject
      "AwsIamInstanceProfile"
      ( \x ->
          AwsIamInstanceProfile'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreateDate")
            Prelude.<*> (x Data..:? "InstanceProfileId")
            Prelude.<*> (x Data..:? "InstanceProfileName")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "Roles" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AwsIamInstanceProfile where
  hashWithSalt _salt AwsIamInstanceProfile' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` instanceProfileId
      `Prelude.hashWithSalt` instanceProfileName
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` roles

instance Prelude.NFData AwsIamInstanceProfile where
  rnf AwsIamInstanceProfile' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf instanceProfileId
      `Prelude.seq` Prelude.rnf instanceProfileName
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf roles

instance Data.ToJSON AwsIamInstanceProfile where
  toJSON AwsIamInstanceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Arn" Data..=) Prelude.<$> arn,
            ("CreateDate" Data..=) Prelude.<$> createDate,
            ("InstanceProfileId" Data..=)
              Prelude.<$> instanceProfileId,
            ("InstanceProfileName" Data..=)
              Prelude.<$> instanceProfileName,
            ("Path" Data..=) Prelude.<$> path,
            ("Roles" Data..=) Prelude.<$> roles
          ]
      )

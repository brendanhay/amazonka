{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RolesAnywhere.CreateProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile. A profile is configuration resource to list the roles
-- that RolesAnywhere service is trusted to assume. In addition, by
-- applying a profile you can intersect permissions with IAM managed
-- policies.
--
-- __Required permissions:__ @rolesanywhere:CreateProfile@.
module Amazonka.RolesAnywhere.CreateProfile
  ( -- * Creating a Request
    CreateProfile (..),
    newCreateProfile,

    -- * Request Lenses
    createProfile_durationSeconds,
    createProfile_enabled,
    createProfile_managedPolicyArns,
    createProfile_requireInstanceProperties,
    createProfile_sessionPolicy,
    createProfile_tags,
    createProfile_name,
    createProfile_roleArns,

    -- * Destructuring the Response
    ProfileDetailResponse (..),
    newProfileDetailResponse,

    -- * Response Lenses
    profileDetailResponse_profile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | The number of seconds the vended session credentials are valid for.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether the profile is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of managed policy ARNs that apply to the vended session
    -- credentials.
    managedPolicyArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether instance properties are required in
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- requests with this profile.
    requireInstanceProperties :: Prelude.Maybe Prelude.Bool,
    -- | A session policy that applies to the trust boundary of the vended
    -- session credentials.
    sessionPolicy :: Prelude.Maybe Prelude.Text,
    -- | The tags to attach to the profile.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the profile.
    name :: Prelude.Text,
    -- | A list of IAM roles that this profile can assume in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    roleArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationSeconds', 'createProfile_durationSeconds' - The number of seconds the vended session credentials are valid for.
--
-- 'enabled', 'createProfile_enabled' - Specifies whether the profile is enabled.
--
-- 'managedPolicyArns', 'createProfile_managedPolicyArns' - A list of managed policy ARNs that apply to the vended session
-- credentials.
--
-- 'requireInstanceProperties', 'createProfile_requireInstanceProperties' - Specifies whether instance properties are required in
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- requests with this profile.
--
-- 'sessionPolicy', 'createProfile_sessionPolicy' - A session policy that applies to the trust boundary of the vended
-- session credentials.
--
-- 'tags', 'createProfile_tags' - The tags to attach to the profile.
--
-- 'name', 'createProfile_name' - The name of the profile.
--
-- 'roleArns', 'createProfile_roleArns' - A list of IAM roles that this profile can assume in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
newCreateProfile ::
  -- | 'name'
  Prelude.Text ->
  CreateProfile
newCreateProfile pName_ =
  CreateProfile'
    { durationSeconds = Prelude.Nothing,
      enabled = Prelude.Nothing,
      managedPolicyArns = Prelude.Nothing,
      requireInstanceProperties = Prelude.Nothing,
      sessionPolicy = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      roleArns = Prelude.mempty
    }

-- | The number of seconds the vended session credentials are valid for.
createProfile_durationSeconds :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Natural)
createProfile_durationSeconds = Lens.lens (\CreateProfile' {durationSeconds} -> durationSeconds) (\s@CreateProfile' {} a -> s {durationSeconds = a} :: CreateProfile)

-- | Specifies whether the profile is enabled.
createProfile_enabled :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Bool)
createProfile_enabled = Lens.lens (\CreateProfile' {enabled} -> enabled) (\s@CreateProfile' {} a -> s {enabled = a} :: CreateProfile)

-- | A list of managed policy ARNs that apply to the vended session
-- credentials.
createProfile_managedPolicyArns :: Lens.Lens' CreateProfile (Prelude.Maybe [Prelude.Text])
createProfile_managedPolicyArns = Lens.lens (\CreateProfile' {managedPolicyArns} -> managedPolicyArns) (\s@CreateProfile' {} a -> s {managedPolicyArns = a} :: CreateProfile) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether instance properties are required in
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- requests with this profile.
createProfile_requireInstanceProperties :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Bool)
createProfile_requireInstanceProperties = Lens.lens (\CreateProfile' {requireInstanceProperties} -> requireInstanceProperties) (\s@CreateProfile' {} a -> s {requireInstanceProperties = a} :: CreateProfile)

-- | A session policy that applies to the trust boundary of the vended
-- session credentials.
createProfile_sessionPolicy :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_sessionPolicy = Lens.lens (\CreateProfile' {sessionPolicy} -> sessionPolicy) (\s@CreateProfile' {} a -> s {sessionPolicy = a} :: CreateProfile)

-- | The tags to attach to the profile.
createProfile_tags :: Lens.Lens' CreateProfile (Prelude.Maybe [Tag])
createProfile_tags = Lens.lens (\CreateProfile' {tags} -> tags) (\s@CreateProfile' {} a -> s {tags = a} :: CreateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of the profile.
createProfile_name :: Lens.Lens' CreateProfile Prelude.Text
createProfile_name = Lens.lens (\CreateProfile' {name} -> name) (\s@CreateProfile' {} a -> s {name = a} :: CreateProfile)

-- | A list of IAM roles that this profile can assume in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
createProfile_roleArns :: Lens.Lens' CreateProfile [Prelude.Text]
createProfile_roleArns = Lens.lens (\CreateProfile' {roleArns} -> roleArns) (\s@CreateProfile' {} a -> s {roleArns = a} :: CreateProfile) Prelude.. Lens.coerced

instance Core.AWSRequest CreateProfile where
  type
    AWSResponse CreateProfile =
      ProfileDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateProfile where
  hashWithSalt _salt CreateProfile' {..} =
    _salt
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` managedPolicyArns
      `Prelude.hashWithSalt` requireInstanceProperties
      `Prelude.hashWithSalt` sessionPolicy
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArns

instance Prelude.NFData CreateProfile where
  rnf CreateProfile' {..} =
    Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf managedPolicyArns
      `Prelude.seq` Prelude.rnf requireInstanceProperties
      `Prelude.seq` Prelude.rnf sessionPolicy
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArns

instance Data.ToHeaders CreateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProfile where
  toJSON CreateProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("durationSeconds" Data..=)
              Prelude.<$> durationSeconds,
            ("enabled" Data..=) Prelude.<$> enabled,
            ("managedPolicyArns" Data..=)
              Prelude.<$> managedPolicyArns,
            ("requireInstanceProperties" Data..=)
              Prelude.<$> requireInstanceProperties,
            ("sessionPolicy" Data..=) Prelude.<$> sessionPolicy,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("roleArns" Data..= roleArns)
          ]
      )

instance Data.ToPath CreateProfile where
  toPath = Prelude.const "/profiles"

instance Data.ToQuery CreateProfile where
  toQuery = Prelude.const Prelude.mempty

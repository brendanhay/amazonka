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
-- Module      : Amazonka.RolesAnywhere.UpdateProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the profile. A profile is configuration resource to list the
-- roles that RolesAnywhere service is trusted to assume. In addition, by
-- applying a profile you can scope-down permissions with IAM managed
-- policies.
--
-- __Required permissions:__ @rolesanywhere:UpdateProfile@.
module Amazonka.RolesAnywhere.UpdateProfile
  ( -- * Creating a Request
    UpdateProfile (..),
    newUpdateProfile,

    -- * Request Lenses
    updateProfile_name,
    updateProfile_managedPolicyArns,
    updateProfile_roleArns,
    updateProfile_durationSeconds,
    updateProfile_sessionPolicy,
    updateProfile_profileId,

    -- * Destructuring the Response
    ProfileDetailResponse (..),
    newProfileDetailResponse,

    -- * Response Lenses
    profileDetailResponse_profile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { -- | The name of the profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of managed policy ARNs that apply to the vended session
    -- credentials.
    managedPolicyArns :: Prelude.Maybe [Prelude.Text],
    -- | A list of IAM roles that this profile can assume in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    roleArns :: Prelude.Maybe [Prelude.Text],
    -- | The number of seconds the vended session credentials are valid for.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A session policy that applies to the trust boundary of the vended
    -- session credentials.
    sessionPolicy :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateProfile_name' - The name of the profile.
--
-- 'managedPolicyArns', 'updateProfile_managedPolicyArns' - A list of managed policy ARNs that apply to the vended session
-- credentials.
--
-- 'roleArns', 'updateProfile_roleArns' - A list of IAM roles that this profile can assume in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
--
-- 'durationSeconds', 'updateProfile_durationSeconds' - The number of seconds the vended session credentials are valid for.
--
-- 'sessionPolicy', 'updateProfile_sessionPolicy' - A session policy that applies to the trust boundary of the vended
-- session credentials.
--
-- 'profileId', 'updateProfile_profileId' - The unique identifier of the profile.
newUpdateProfile ::
  -- | 'profileId'
  Prelude.Text ->
  UpdateProfile
newUpdateProfile pProfileId_ =
  UpdateProfile'
    { name = Prelude.Nothing,
      managedPolicyArns = Prelude.Nothing,
      roleArns = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      sessionPolicy = Prelude.Nothing,
      profileId = pProfileId_
    }

-- | The name of the profile.
updateProfile_name :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_name = Lens.lens (\UpdateProfile' {name} -> name) (\s@UpdateProfile' {} a -> s {name = a} :: UpdateProfile)

-- | A list of managed policy ARNs that apply to the vended session
-- credentials.
updateProfile_managedPolicyArns :: Lens.Lens' UpdateProfile (Prelude.Maybe [Prelude.Text])
updateProfile_managedPolicyArns = Lens.lens (\UpdateProfile' {managedPolicyArns} -> managedPolicyArns) (\s@UpdateProfile' {} a -> s {managedPolicyArns = a} :: UpdateProfile) Prelude.. Lens.mapping Lens.coerced

-- | A list of IAM roles that this profile can assume in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
updateProfile_roleArns :: Lens.Lens' UpdateProfile (Prelude.Maybe [Prelude.Text])
updateProfile_roleArns = Lens.lens (\UpdateProfile' {roleArns} -> roleArns) (\s@UpdateProfile' {} a -> s {roleArns = a} :: UpdateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds the vended session credentials are valid for.
updateProfile_durationSeconds :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Natural)
updateProfile_durationSeconds = Lens.lens (\UpdateProfile' {durationSeconds} -> durationSeconds) (\s@UpdateProfile' {} a -> s {durationSeconds = a} :: UpdateProfile)

-- | A session policy that applies to the trust boundary of the vended
-- session credentials.
updateProfile_sessionPolicy :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_sessionPolicy = Lens.lens (\UpdateProfile' {sessionPolicy} -> sessionPolicy) (\s@UpdateProfile' {} a -> s {sessionPolicy = a} :: UpdateProfile)

-- | The unique identifier of the profile.
updateProfile_profileId :: Lens.Lens' UpdateProfile Prelude.Text
updateProfile_profileId = Lens.lens (\UpdateProfile' {profileId} -> profileId) (\s@UpdateProfile' {} a -> s {profileId = a} :: UpdateProfile)

instance Core.AWSRequest UpdateProfile where
  type
    AWSResponse UpdateProfile =
      ProfileDetailResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateProfile where
  hashWithSalt _salt UpdateProfile' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` managedPolicyArns
      `Prelude.hashWithSalt` roleArns
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` sessionPolicy
      `Prelude.hashWithSalt` profileId

instance Prelude.NFData UpdateProfile where
  rnf UpdateProfile' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf managedPolicyArns
      `Prelude.seq` Prelude.rnf roleArns
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf sessionPolicy
      `Prelude.seq` Prelude.rnf profileId

instance Core.ToHeaders UpdateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProfile where
  toJSON UpdateProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("managedPolicyArns" Core..=)
              Prelude.<$> managedPolicyArns,
            ("roleArns" Core..=) Prelude.<$> roleArns,
            ("durationSeconds" Core..=)
              Prelude.<$> durationSeconds,
            ("sessionPolicy" Core..=) Prelude.<$> sessionPolicy
          ]
      )

instance Core.ToPath UpdateProfile where
  toPath UpdateProfile' {..} =
    Prelude.mconcat ["/profile/", Core.toBS profileId]

instance Core.ToQuery UpdateProfile where
  toQuery = Prelude.const Prelude.mempty

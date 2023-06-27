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
-- Module      : Amazonka.WellArchitected.UpdateProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a profile.
module Amazonka.WellArchitected.UpdateProfile
  ( -- * Creating a Request
    UpdateProfile (..),
    newUpdateProfile,

    -- * Request Lenses
    updateProfile_profileDescription,
    updateProfile_profileQuestions,
    updateProfile_profileArn,

    -- * Destructuring the Response
    UpdateProfileResponse (..),
    newUpdateProfileResponse,

    -- * Response Lenses
    updateProfileResponse_profile,
    updateProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { -- | The profile description.
    profileDescription :: Prelude.Maybe Prelude.Text,
    -- | Profile questions.
    profileQuestions :: Prelude.Maybe [ProfileQuestionUpdate],
    -- | The profile ARN.
    profileArn :: Prelude.Text
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
-- 'profileDescription', 'updateProfile_profileDescription' - The profile description.
--
-- 'profileQuestions', 'updateProfile_profileQuestions' - Profile questions.
--
-- 'profileArn', 'updateProfile_profileArn' - The profile ARN.
newUpdateProfile ::
  -- | 'profileArn'
  Prelude.Text ->
  UpdateProfile
newUpdateProfile pProfileArn_ =
  UpdateProfile'
    { profileDescription =
        Prelude.Nothing,
      profileQuestions = Prelude.Nothing,
      profileArn = pProfileArn_
    }

-- | The profile description.
updateProfile_profileDescription :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_profileDescription = Lens.lens (\UpdateProfile' {profileDescription} -> profileDescription) (\s@UpdateProfile' {} a -> s {profileDescription = a} :: UpdateProfile)

-- | Profile questions.
updateProfile_profileQuestions :: Lens.Lens' UpdateProfile (Prelude.Maybe [ProfileQuestionUpdate])
updateProfile_profileQuestions = Lens.lens (\UpdateProfile' {profileQuestions} -> profileQuestions) (\s@UpdateProfile' {} a -> s {profileQuestions = a} :: UpdateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The profile ARN.
updateProfile_profileArn :: Lens.Lens' UpdateProfile Prelude.Text
updateProfile_profileArn = Lens.lens (\UpdateProfile' {profileArn} -> profileArn) (\s@UpdateProfile' {} a -> s {profileArn = a} :: UpdateProfile)

instance Core.AWSRequest UpdateProfile where
  type
    AWSResponse UpdateProfile =
      UpdateProfileResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProfileResponse'
            Prelude.<$> (x Data..?> "Profile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProfile where
  hashWithSalt _salt UpdateProfile' {..} =
    _salt
      `Prelude.hashWithSalt` profileDescription
      `Prelude.hashWithSalt` profileQuestions
      `Prelude.hashWithSalt` profileArn

instance Prelude.NFData UpdateProfile where
  rnf UpdateProfile' {..} =
    Prelude.rnf profileDescription
      `Prelude.seq` Prelude.rnf profileQuestions
      `Prelude.seq` Prelude.rnf profileArn

instance Data.ToHeaders UpdateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProfile where
  toJSON UpdateProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProfileDescription" Data..=)
              Prelude.<$> profileDescription,
            ("ProfileQuestions" Data..=)
              Prelude.<$> profileQuestions
          ]
      )

instance Data.ToPath UpdateProfile where
  toPath UpdateProfile' {..} =
    Prelude.mconcat
      ["/profiles/", Data.toBS profileArn]

instance Data.ToQuery UpdateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProfileResponse' smart constructor.
data UpdateProfileResponse = UpdateProfileResponse'
  { -- | The profile.
    profile :: Prelude.Maybe Profile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profile', 'updateProfileResponse_profile' - The profile.
--
-- 'httpStatus', 'updateProfileResponse_httpStatus' - The response's http status code.
newUpdateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProfileResponse
newUpdateProfileResponse pHttpStatus_ =
  UpdateProfileResponse'
    { profile = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The profile.
updateProfileResponse_profile :: Lens.Lens' UpdateProfileResponse (Prelude.Maybe Profile)
updateProfileResponse_profile = Lens.lens (\UpdateProfileResponse' {profile} -> profile) (\s@UpdateProfileResponse' {} a -> s {profile = a} :: UpdateProfileResponse)

-- | The response's http status code.
updateProfileResponse_httpStatus :: Lens.Lens' UpdateProfileResponse Prelude.Int
updateProfileResponse_httpStatus = Lens.lens (\UpdateProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateProfileResponse' {} a -> s {httpStatus = a} :: UpdateProfileResponse)

instance Prelude.NFData UpdateProfileResponse where
  rnf UpdateProfileResponse' {..} =
    Prelude.rnf profile
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.Transfer.UpdateProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates some of the parameters for an existing profile. Provide the
-- @ProfileId@ for the profile that you want to update, along with the new
-- values for the parameters to update.
module Amazonka.Transfer.UpdateProfile
  ( -- * Creating a Request
    UpdateProfile (..),
    newUpdateProfile,

    -- * Request Lenses
    updateProfile_certificateIds,
    updateProfile_profileId,

    -- * Destructuring the Response
    UpdateProfileResponse (..),
    newUpdateProfileResponse,

    -- * Response Lenses
    updateProfileResponse_httpStatus,
    updateProfileResponse_profileId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { -- | An array of identifiers for the imported certificates. You use this
    -- identifier for working with profiles and partner profiles.
    certificateIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the profile object that you are updating.
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
-- 'certificateIds', 'updateProfile_certificateIds' - An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
--
-- 'profileId', 'updateProfile_profileId' - The identifier of the profile object that you are updating.
newUpdateProfile ::
  -- | 'profileId'
  Prelude.Text ->
  UpdateProfile
newUpdateProfile pProfileId_ =
  UpdateProfile'
    { certificateIds = Prelude.Nothing,
      profileId = pProfileId_
    }

-- | An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
updateProfile_certificateIds :: Lens.Lens' UpdateProfile (Prelude.Maybe [Prelude.Text])
updateProfile_certificateIds = Lens.lens (\UpdateProfile' {certificateIds} -> certificateIds) (\s@UpdateProfile' {} a -> s {certificateIds = a} :: UpdateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the profile object that you are updating.
updateProfile_profileId :: Lens.Lens' UpdateProfile Prelude.Text
updateProfile_profileId = Lens.lens (\UpdateProfile' {profileId} -> profileId) (\s@UpdateProfile' {} a -> s {profileId = a} :: UpdateProfile)

instance Core.AWSRequest UpdateProfile where
  type
    AWSResponse UpdateProfile =
      UpdateProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProfileId")
      )

instance Prelude.Hashable UpdateProfile where
  hashWithSalt _salt UpdateProfile' {..} =
    _salt
      `Prelude.hashWithSalt` certificateIds
      `Prelude.hashWithSalt` profileId

instance Prelude.NFData UpdateProfile where
  rnf UpdateProfile' {..} =
    Prelude.rnf certificateIds
      `Prelude.seq` Prelude.rnf profileId

instance Data.ToHeaders UpdateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.UpdateProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProfile where
  toJSON UpdateProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateIds" Data..=)
              Prelude.<$> certificateIds,
            Prelude.Just ("ProfileId" Data..= profileId)
          ]
      )

instance Data.ToPath UpdateProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProfileResponse' smart constructor.
data UpdateProfileResponse = UpdateProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the identifier for the profile that\'s being updated.
    profileId :: Prelude.Text
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
-- 'httpStatus', 'updateProfileResponse_httpStatus' - The response's http status code.
--
-- 'profileId', 'updateProfileResponse_profileId' - Returns the identifier for the profile that\'s being updated.
newUpdateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'profileId'
  Prelude.Text ->
  UpdateProfileResponse
newUpdateProfileResponse pHttpStatus_ pProfileId_ =
  UpdateProfileResponse'
    { httpStatus = pHttpStatus_,
      profileId = pProfileId_
    }

-- | The response's http status code.
updateProfileResponse_httpStatus :: Lens.Lens' UpdateProfileResponse Prelude.Int
updateProfileResponse_httpStatus = Lens.lens (\UpdateProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateProfileResponse' {} a -> s {httpStatus = a} :: UpdateProfileResponse)

-- | Returns the identifier for the profile that\'s being updated.
updateProfileResponse_profileId :: Lens.Lens' UpdateProfileResponse Prelude.Text
updateProfileResponse_profileId = Lens.lens (\UpdateProfileResponse' {profileId} -> profileId) (\s@UpdateProfileResponse' {} a -> s {profileId = a} :: UpdateProfileResponse)

instance Prelude.NFData UpdateProfileResponse where
  rnf UpdateProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profileId

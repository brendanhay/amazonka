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
-- Module      : Amazonka.AlexaBusiness.GetProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a room profile by profile ARN.
module Amazonka.AlexaBusiness.GetProfile
  ( -- * Creating a Request
    GetProfile (..),
    newGetProfile,

    -- * Request Lenses
    getProfile_profileArn,

    -- * Destructuring the Response
    GetProfileResponse (..),
    newGetProfileResponse,

    -- * Response Lenses
    getProfileResponse_profile,
    getProfileResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProfile' smart constructor.
data GetProfile = GetProfile'
  { -- | The ARN of the room profile for which to request details. Required.
    profileArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileArn', 'getProfile_profileArn' - The ARN of the room profile for which to request details. Required.
newGetProfile ::
  GetProfile
newGetProfile =
  GetProfile' {profileArn = Prelude.Nothing}

-- | The ARN of the room profile for which to request details. Required.
getProfile_profileArn :: Lens.Lens' GetProfile (Prelude.Maybe Prelude.Text)
getProfile_profileArn = Lens.lens (\GetProfile' {profileArn} -> profileArn) (\s@GetProfile' {} a -> s {profileArn = a} :: GetProfile)

instance Core.AWSRequest GetProfile where
  type AWSResponse GetProfile = GetProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProfileResponse'
            Prelude.<$> (x Core..?> "Profile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProfile where
  hashWithSalt _salt GetProfile' {..} =
    _salt `Prelude.hashWithSalt` profileArn

instance Prelude.NFData GetProfile where
  rnf GetProfile' {..} = Prelude.rnf profileArn

instance Core.ToHeaders GetProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetProfile where
  toJSON GetProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [("ProfileArn" Core..=) Prelude.<$> profileArn]
      )

instance Core.ToPath GetProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery GetProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProfileResponse' smart constructor.
data GetProfileResponse = GetProfileResponse'
  { -- | The details of the room profile requested. Required.
    profile :: Prelude.Maybe Profile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profile', 'getProfileResponse_profile' - The details of the room profile requested. Required.
--
-- 'httpStatus', 'getProfileResponse_httpStatus' - The response's http status code.
newGetProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProfileResponse
newGetProfileResponse pHttpStatus_ =
  GetProfileResponse'
    { profile = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the room profile requested. Required.
getProfileResponse_profile :: Lens.Lens' GetProfileResponse (Prelude.Maybe Profile)
getProfileResponse_profile = Lens.lens (\GetProfileResponse' {profile} -> profile) (\s@GetProfileResponse' {} a -> s {profile = a} :: GetProfileResponse)

-- | The response's http status code.
getProfileResponse_httpStatus :: Lens.Lens' GetProfileResponse Prelude.Int
getProfileResponse_httpStatus = Lens.lens (\GetProfileResponse' {httpStatus} -> httpStatus) (\s@GetProfileResponse' {} a -> s {httpStatus = a} :: GetProfileResponse)

instance Prelude.NFData GetProfileResponse where
  rnf GetProfileResponse' {..} =
    Prelude.rnf profile
      `Prelude.seq` Prelude.rnf httpStatus

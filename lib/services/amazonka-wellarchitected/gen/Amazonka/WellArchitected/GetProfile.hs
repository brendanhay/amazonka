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
-- Module      : Amazonka.WellArchitected.GetProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get profile information.
module Amazonka.WellArchitected.GetProfile
  ( -- * Creating a Request
    GetProfile (..),
    newGetProfile,

    -- * Request Lenses
    getProfile_profileVersion,
    getProfile_profileArn,

    -- * Destructuring the Response
    GetProfileResponse (..),
    newGetProfileResponse,

    -- * Response Lenses
    getProfileResponse_profile,
    getProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newGetProfile' smart constructor.
data GetProfile = GetProfile'
  { -- | The profile version.
    profileVersion :: Prelude.Maybe Prelude.Text,
    -- | The profile ARN.
    profileArn :: Prelude.Text
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
-- 'profileVersion', 'getProfile_profileVersion' - The profile version.
--
-- 'profileArn', 'getProfile_profileArn' - The profile ARN.
newGetProfile ::
  -- | 'profileArn'
  Prelude.Text ->
  GetProfile
newGetProfile pProfileArn_ =
  GetProfile'
    { profileVersion = Prelude.Nothing,
      profileArn = pProfileArn_
    }

-- | The profile version.
getProfile_profileVersion :: Lens.Lens' GetProfile (Prelude.Maybe Prelude.Text)
getProfile_profileVersion = Lens.lens (\GetProfile' {profileVersion} -> profileVersion) (\s@GetProfile' {} a -> s {profileVersion = a} :: GetProfile)

-- | The profile ARN.
getProfile_profileArn :: Lens.Lens' GetProfile Prelude.Text
getProfile_profileArn = Lens.lens (\GetProfile' {profileArn} -> profileArn) (\s@GetProfile' {} a -> s {profileArn = a} :: GetProfile)

instance Core.AWSRequest GetProfile where
  type AWSResponse GetProfile = GetProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProfileResponse'
            Prelude.<$> (x Data..?> "Profile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProfile where
  hashWithSalt _salt GetProfile' {..} =
    _salt
      `Prelude.hashWithSalt` profileVersion
      `Prelude.hashWithSalt` profileArn

instance Prelude.NFData GetProfile where
  rnf GetProfile' {..} =
    Prelude.rnf profileVersion
      `Prelude.seq` Prelude.rnf profileArn

instance Data.ToHeaders GetProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetProfile where
  toPath GetProfile' {..} =
    Prelude.mconcat
      ["/profiles/", Data.toBS profileArn]

instance Data.ToQuery GetProfile where
  toQuery GetProfile' {..} =
    Prelude.mconcat
      ["ProfileVersion" Data.=: profileVersion]

-- | /See:/ 'newGetProfileResponse' smart constructor.
data GetProfileResponse = GetProfileResponse'
  { -- | The profile.
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
-- 'profile', 'getProfileResponse_profile' - The profile.
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

-- | The profile.
getProfileResponse_profile :: Lens.Lens' GetProfileResponse (Prelude.Maybe Profile)
getProfileResponse_profile = Lens.lens (\GetProfileResponse' {profile} -> profile) (\s@GetProfileResponse' {} a -> s {profile = a} :: GetProfileResponse)

-- | The response's http status code.
getProfileResponse_httpStatus :: Lens.Lens' GetProfileResponse Prelude.Int
getProfileResponse_httpStatus = Lens.lens (\GetProfileResponse' {httpStatus} -> httpStatus) (\s@GetProfileResponse' {} a -> s {httpStatus = a} :: GetProfileResponse)

instance Prelude.NFData GetProfileResponse where
  rnf GetProfileResponse' {..} =
    Prelude.rnf profile
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.AlexaBusiness.GetProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a room profile by profile ARN.
module Network.AWS.AlexaBusiness.GetProfile
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetProfile' smart constructor.
data GetProfile = GetProfile'
  { -- | The ARN of the room profile for which to request details. Required.
    profileArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  GetProfile' {profileArn = Core.Nothing}

-- | The ARN of the room profile for which to request details. Required.
getProfile_profileArn :: Lens.Lens' GetProfile (Core.Maybe Core.Text)
getProfile_profileArn = Lens.lens (\GetProfile' {profileArn} -> profileArn) (\s@GetProfile' {} a -> s {profileArn = a} :: GetProfile)

instance Core.AWSRequest GetProfile where
  type AWSResponse GetProfile = GetProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProfileResponse'
            Core.<$> (x Core..?> "Profile")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetProfile

instance Core.NFData GetProfile

instance Core.ToHeaders GetProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.GetProfile" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetProfile where
  toJSON GetProfile' {..} =
    Core.object
      ( Core.catMaybes
          [("ProfileArn" Core..=) Core.<$> profileArn]
      )

instance Core.ToPath GetProfile where
  toPath = Core.const "/"

instance Core.ToQuery GetProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetProfileResponse' smart constructor.
data GetProfileResponse = GetProfileResponse'
  { -- | The details of the room profile requested. Required.
    profile :: Core.Maybe Profile,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetProfileResponse
newGetProfileResponse pHttpStatus_ =
  GetProfileResponse'
    { profile = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the room profile requested. Required.
getProfileResponse_profile :: Lens.Lens' GetProfileResponse (Core.Maybe Profile)
getProfileResponse_profile = Lens.lens (\GetProfileResponse' {profile} -> profile) (\s@GetProfileResponse' {} a -> s {profile = a} :: GetProfileResponse)

-- | The response's http status code.
getProfileResponse_httpStatus :: Lens.Lens' GetProfileResponse Core.Int
getProfileResponse_httpStatus = Lens.lens (\GetProfileResponse' {httpStatus} -> httpStatus) (\s@GetProfileResponse' {} a -> s {httpStatus = a} :: GetProfileResponse)

instance Core.NFData GetProfileResponse

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
-- Module      : Network.AWS.AlexaBusiness.DeleteProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room profile by the profile ARN.
module Network.AWS.AlexaBusiness.DeleteProfile
  ( -- * Creating a Request
    DeleteProfile (..),
    newDeleteProfile,

    -- * Request Lenses
    deleteProfile_profileArn,

    -- * Destructuring the Response
    DeleteProfileResponse (..),
    newDeleteProfileResponse,

    -- * Response Lenses
    deleteProfileResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProfile' smart constructor.
data DeleteProfile = DeleteProfile'
  { -- | The ARN of the room profile to delete. Required.
    profileArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileArn', 'deleteProfile_profileArn' - The ARN of the room profile to delete. Required.
newDeleteProfile ::
  DeleteProfile
newDeleteProfile =
  DeleteProfile' {profileArn = Core.Nothing}

-- | The ARN of the room profile to delete. Required.
deleteProfile_profileArn :: Lens.Lens' DeleteProfile (Core.Maybe Core.Text)
deleteProfile_profileArn = Lens.lens (\DeleteProfile' {profileArn} -> profileArn) (\s@DeleteProfile' {} a -> s {profileArn = a} :: DeleteProfile)

instance Core.AWSRequest DeleteProfile where
  type
    AWSResponse DeleteProfile =
      DeleteProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProfile

instance Core.NFData DeleteProfile

instance Core.ToHeaders DeleteProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DeleteProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProfile where
  toJSON DeleteProfile' {..} =
    Core.object
      ( Core.catMaybes
          [("ProfileArn" Core..=) Core.<$> profileArn]
      )

instance Core.ToPath DeleteProfile where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProfileResponse' smart constructor.
data DeleteProfileResponse = DeleteProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProfileResponse_httpStatus' - The response's http status code.
newDeleteProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProfileResponse
newDeleteProfileResponse pHttpStatus_ =
  DeleteProfileResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteProfileResponse_httpStatus :: Lens.Lens' DeleteProfileResponse Core.Int
deleteProfileResponse_httpStatus = Lens.lens (\DeleteProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteProfileResponse' {} a -> s {httpStatus = a} :: DeleteProfileResponse)

instance Core.NFData DeleteProfileResponse

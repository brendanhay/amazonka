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
-- Module      : Network.AWS.AlexaBusiness.RevokeInvitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes an invitation and invalidates the enrollment URL.
module Network.AWS.AlexaBusiness.RevokeInvitation
  ( -- * Creating a Request
    RevokeInvitation (..),
    newRevokeInvitation,

    -- * Request Lenses
    revokeInvitation_userArn,
    revokeInvitation_enrollmentId,

    -- * Destructuring the Response
    RevokeInvitationResponse (..),
    newRevokeInvitationResponse,

    -- * Response Lenses
    revokeInvitationResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRevokeInvitation' smart constructor.
data RevokeInvitation = RevokeInvitation'
  { -- | The ARN of the user for whom to revoke an enrollment invitation.
    -- Required.
    userArn :: Core.Maybe Core.Text,
    -- | The ARN of the enrollment invitation to revoke. Required.
    enrollmentId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'revokeInvitation_userArn' - The ARN of the user for whom to revoke an enrollment invitation.
-- Required.
--
-- 'enrollmentId', 'revokeInvitation_enrollmentId' - The ARN of the enrollment invitation to revoke. Required.
newRevokeInvitation ::
  RevokeInvitation
newRevokeInvitation =
  RevokeInvitation'
    { userArn = Core.Nothing,
      enrollmentId = Core.Nothing
    }

-- | The ARN of the user for whom to revoke an enrollment invitation.
-- Required.
revokeInvitation_userArn :: Lens.Lens' RevokeInvitation (Core.Maybe Core.Text)
revokeInvitation_userArn = Lens.lens (\RevokeInvitation' {userArn} -> userArn) (\s@RevokeInvitation' {} a -> s {userArn = a} :: RevokeInvitation)

-- | The ARN of the enrollment invitation to revoke. Required.
revokeInvitation_enrollmentId :: Lens.Lens' RevokeInvitation (Core.Maybe Core.Text)
revokeInvitation_enrollmentId = Lens.lens (\RevokeInvitation' {enrollmentId} -> enrollmentId) (\s@RevokeInvitation' {} a -> s {enrollmentId = a} :: RevokeInvitation)

instance Core.AWSRequest RevokeInvitation where
  type
    AWSResponse RevokeInvitation =
      RevokeInvitationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RevokeInvitationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RevokeInvitation

instance Core.NFData RevokeInvitation

instance Core.ToHeaders RevokeInvitation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.RevokeInvitation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RevokeInvitation where
  toJSON RevokeInvitation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UserArn" Core..=) Core.<$> userArn,
            ("EnrollmentId" Core..=) Core.<$> enrollmentId
          ]
      )

instance Core.ToPath RevokeInvitation where
  toPath = Core.const "/"

instance Core.ToQuery RevokeInvitation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRevokeInvitationResponse' smart constructor.
data RevokeInvitationResponse = RevokeInvitationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevokeInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'revokeInvitationResponse_httpStatus' - The response's http status code.
newRevokeInvitationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RevokeInvitationResponse
newRevokeInvitationResponse pHttpStatus_ =
  RevokeInvitationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
revokeInvitationResponse_httpStatus :: Lens.Lens' RevokeInvitationResponse Core.Int
revokeInvitationResponse_httpStatus = Lens.lens (\RevokeInvitationResponse' {httpStatus} -> httpStatus) (\s@RevokeInvitationResponse' {} a -> s {httpStatus = a} :: RevokeInvitationResponse)

instance Core.NFData RevokeInvitationResponse

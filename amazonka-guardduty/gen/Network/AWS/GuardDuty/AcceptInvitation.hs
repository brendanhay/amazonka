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
-- Module      : Network.AWS.GuardDuty.AcceptInvitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the invitation to be monitored by a GuardDuty administrator
-- account.
module Network.AWS.GuardDuty.AcceptInvitation
  ( -- * Creating a Request
    AcceptInvitation (..),
    newAcceptInvitation,

    -- * Request Lenses
    acceptInvitation_detectorId,
    acceptInvitation_masterId,
    acceptInvitation_invitationId,

    -- * Destructuring the Response
    AcceptInvitationResponse (..),
    newAcceptInvitationResponse,

    -- * Response Lenses
    acceptInvitationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptInvitation' smart constructor.
data AcceptInvitation = AcceptInvitation'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Core.Text,
    -- | The account ID of the GuardDuty administrator account whose invitation
    -- you\'re accepting.
    masterId :: Core.Text,
    -- | The value that is used to validate the administrator account to the
    -- member account.
    invitationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'acceptInvitation_detectorId' - The unique ID of the detector of the GuardDuty member account.
--
-- 'masterId', 'acceptInvitation_masterId' - The account ID of the GuardDuty administrator account whose invitation
-- you\'re accepting.
--
-- 'invitationId', 'acceptInvitation_invitationId' - The value that is used to validate the administrator account to the
-- member account.
newAcceptInvitation ::
  -- | 'detectorId'
  Core.Text ->
  -- | 'masterId'
  Core.Text ->
  -- | 'invitationId'
  Core.Text ->
  AcceptInvitation
newAcceptInvitation
  pDetectorId_
  pMasterId_
  pInvitationId_ =
    AcceptInvitation'
      { detectorId = pDetectorId_,
        masterId = pMasterId_,
        invitationId = pInvitationId_
      }

-- | The unique ID of the detector of the GuardDuty member account.
acceptInvitation_detectorId :: Lens.Lens' AcceptInvitation Core.Text
acceptInvitation_detectorId = Lens.lens (\AcceptInvitation' {detectorId} -> detectorId) (\s@AcceptInvitation' {} a -> s {detectorId = a} :: AcceptInvitation)

-- | The account ID of the GuardDuty administrator account whose invitation
-- you\'re accepting.
acceptInvitation_masterId :: Lens.Lens' AcceptInvitation Core.Text
acceptInvitation_masterId = Lens.lens (\AcceptInvitation' {masterId} -> masterId) (\s@AcceptInvitation' {} a -> s {masterId = a} :: AcceptInvitation)

-- | The value that is used to validate the administrator account to the
-- member account.
acceptInvitation_invitationId :: Lens.Lens' AcceptInvitation Core.Text
acceptInvitation_invitationId = Lens.lens (\AcceptInvitation' {invitationId} -> invitationId) (\s@AcceptInvitation' {} a -> s {invitationId = a} :: AcceptInvitation)

instance Core.AWSRequest AcceptInvitation where
  type
    AWSResponse AcceptInvitation =
      AcceptInvitationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptInvitationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcceptInvitation

instance Core.NFData AcceptInvitation

instance Core.ToHeaders AcceptInvitation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AcceptInvitation where
  toJSON AcceptInvitation' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("masterId" Core..= masterId),
            Core.Just ("invitationId" Core..= invitationId)
          ]
      )

instance Core.ToPath AcceptInvitation where
  toPath AcceptInvitation' {..} =
    Core.mconcat
      ["/detector/", Core.toBS detectorId, "/master"]

instance Core.ToQuery AcceptInvitation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAcceptInvitationResponse' smart constructor.
data AcceptInvitationResponse = AcceptInvitationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptInvitationResponse_httpStatus' - The response's http status code.
newAcceptInvitationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptInvitationResponse
newAcceptInvitationResponse pHttpStatus_ =
  AcceptInvitationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptInvitationResponse_httpStatus :: Lens.Lens' AcceptInvitationResponse Core.Int
acceptInvitationResponse_httpStatus = Lens.lens (\AcceptInvitationResponse' {httpStatus} -> httpStatus) (\s@AcceptInvitationResponse' {} a -> s {httpStatus = a} :: AcceptInvitationResponse)

instance Core.NFData AcceptInvitationResponse

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
-- Module      : Amazonka.GuardDuty.AcceptInvitation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the invitation to be monitored by a GuardDuty administrator
-- account.
module Amazonka.GuardDuty.AcceptInvitation
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

import qualified Amazonka.Core as Core
import Amazonka.GuardDuty.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptInvitation' smart constructor.
data AcceptInvitation = AcceptInvitation'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Prelude.Text,
    -- | The account ID of the GuardDuty administrator account whose invitation
    -- you\'re accepting.
    masterId :: Prelude.Text,
    -- | The value that is used to validate the administrator account to the
    -- member account.
    invitationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'masterId'
  Prelude.Text ->
  -- | 'invitationId'
  Prelude.Text ->
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
acceptInvitation_detectorId :: Lens.Lens' AcceptInvitation Prelude.Text
acceptInvitation_detectorId = Lens.lens (\AcceptInvitation' {detectorId} -> detectorId) (\s@AcceptInvitation' {} a -> s {detectorId = a} :: AcceptInvitation)

-- | The account ID of the GuardDuty administrator account whose invitation
-- you\'re accepting.
acceptInvitation_masterId :: Lens.Lens' AcceptInvitation Prelude.Text
acceptInvitation_masterId = Lens.lens (\AcceptInvitation' {masterId} -> masterId) (\s@AcceptInvitation' {} a -> s {masterId = a} :: AcceptInvitation)

-- | The value that is used to validate the administrator account to the
-- member account.
acceptInvitation_invitationId :: Lens.Lens' AcceptInvitation Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptInvitation where
  hashWithSalt salt' AcceptInvitation' {..} =
    salt' `Prelude.hashWithSalt` invitationId
      `Prelude.hashWithSalt` masterId
      `Prelude.hashWithSalt` detectorId

instance Prelude.NFData AcceptInvitation where
  rnf AcceptInvitation' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf invitationId
      `Prelude.seq` Prelude.rnf masterId

instance Core.ToHeaders AcceptInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AcceptInvitation where
  toJSON AcceptInvitation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("masterId" Core..= masterId),
            Prelude.Just ("invitationId" Core..= invitationId)
          ]
      )

instance Core.ToPath AcceptInvitation where
  toPath AcceptInvitation' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId, "/master"]

instance Core.ToQuery AcceptInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptInvitationResponse' smart constructor.
data AcceptInvitationResponse = AcceptInvitationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AcceptInvitationResponse
newAcceptInvitationResponse pHttpStatus_ =
  AcceptInvitationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptInvitationResponse_httpStatus :: Lens.Lens' AcceptInvitationResponse Prelude.Int
acceptInvitationResponse_httpStatus = Lens.lens (\AcceptInvitationResponse' {httpStatus} -> httpStatus) (\s@AcceptInvitationResponse' {} a -> s {httpStatus = a} :: AcceptInvitationResponse)

instance Prelude.NFData AcceptInvitationResponse where
  rnf AcceptInvitationResponse' {..} =
    Prelude.rnf httpStatus

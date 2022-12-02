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
-- Module      : Amazonka.GuardDuty.AcceptAdministratorInvitation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the invitation to be a member account and get monitored by a
-- GuardDuty administrator account that sent the invitation.
module Amazonka.GuardDuty.AcceptAdministratorInvitation
  ( -- * Creating a Request
    AcceptAdministratorInvitation (..),
    newAcceptAdministratorInvitation,

    -- * Request Lenses
    acceptAdministratorInvitation_detectorId,
    acceptAdministratorInvitation_administratorId,
    acceptAdministratorInvitation_invitationId,

    -- * Destructuring the Response
    AcceptAdministratorInvitationResponse (..),
    newAcceptAdministratorInvitationResponse,

    -- * Response Lenses
    acceptAdministratorInvitationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptAdministratorInvitation' smart constructor.
data AcceptAdministratorInvitation = AcceptAdministratorInvitation'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Prelude.Text,
    -- | The account ID of the GuardDuty administrator account whose invitation
    -- you\'re accepting.
    administratorId :: Prelude.Text,
    -- | The value that is used to validate the administrator account to the
    -- member account.
    invitationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptAdministratorInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'acceptAdministratorInvitation_detectorId' - The unique ID of the detector of the GuardDuty member account.
--
-- 'administratorId', 'acceptAdministratorInvitation_administratorId' - The account ID of the GuardDuty administrator account whose invitation
-- you\'re accepting.
--
-- 'invitationId', 'acceptAdministratorInvitation_invitationId' - The value that is used to validate the administrator account to the
-- member account.
newAcceptAdministratorInvitation ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'administratorId'
  Prelude.Text ->
  -- | 'invitationId'
  Prelude.Text ->
  AcceptAdministratorInvitation
newAcceptAdministratorInvitation
  pDetectorId_
  pAdministratorId_
  pInvitationId_ =
    AcceptAdministratorInvitation'
      { detectorId =
          pDetectorId_,
        administratorId = pAdministratorId_,
        invitationId = pInvitationId_
      }

-- | The unique ID of the detector of the GuardDuty member account.
acceptAdministratorInvitation_detectorId :: Lens.Lens' AcceptAdministratorInvitation Prelude.Text
acceptAdministratorInvitation_detectorId = Lens.lens (\AcceptAdministratorInvitation' {detectorId} -> detectorId) (\s@AcceptAdministratorInvitation' {} a -> s {detectorId = a} :: AcceptAdministratorInvitation)

-- | The account ID of the GuardDuty administrator account whose invitation
-- you\'re accepting.
acceptAdministratorInvitation_administratorId :: Lens.Lens' AcceptAdministratorInvitation Prelude.Text
acceptAdministratorInvitation_administratorId = Lens.lens (\AcceptAdministratorInvitation' {administratorId} -> administratorId) (\s@AcceptAdministratorInvitation' {} a -> s {administratorId = a} :: AcceptAdministratorInvitation)

-- | The value that is used to validate the administrator account to the
-- member account.
acceptAdministratorInvitation_invitationId :: Lens.Lens' AcceptAdministratorInvitation Prelude.Text
acceptAdministratorInvitation_invitationId = Lens.lens (\AcceptAdministratorInvitation' {invitationId} -> invitationId) (\s@AcceptAdministratorInvitation' {} a -> s {invitationId = a} :: AcceptAdministratorInvitation)

instance
  Core.AWSRequest
    AcceptAdministratorInvitation
  where
  type
    AWSResponse AcceptAdministratorInvitation =
      AcceptAdministratorInvitationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AcceptAdministratorInvitationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptAdministratorInvitation
  where
  hashWithSalt _salt AcceptAdministratorInvitation' {..} =
    _salt `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` administratorId
      `Prelude.hashWithSalt` invitationId

instance Prelude.NFData AcceptAdministratorInvitation where
  rnf AcceptAdministratorInvitation' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf administratorId
      `Prelude.seq` Prelude.rnf invitationId

instance Data.ToHeaders AcceptAdministratorInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AcceptAdministratorInvitation where
  toJSON AcceptAdministratorInvitation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("administratorId" Data..= administratorId),
            Prelude.Just ("invitationId" Data..= invitationId)
          ]
      )

instance Data.ToPath AcceptAdministratorInvitation where
  toPath AcceptAdministratorInvitation' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/administrator"
      ]

instance Data.ToQuery AcceptAdministratorInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptAdministratorInvitationResponse' smart constructor.
data AcceptAdministratorInvitationResponse = AcceptAdministratorInvitationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptAdministratorInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptAdministratorInvitationResponse_httpStatus' - The response's http status code.
newAcceptAdministratorInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptAdministratorInvitationResponse
newAcceptAdministratorInvitationResponse pHttpStatus_ =
  AcceptAdministratorInvitationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
acceptAdministratorInvitationResponse_httpStatus :: Lens.Lens' AcceptAdministratorInvitationResponse Prelude.Int
acceptAdministratorInvitationResponse_httpStatus = Lens.lens (\AcceptAdministratorInvitationResponse' {httpStatus} -> httpStatus) (\s@AcceptAdministratorInvitationResponse' {} a -> s {httpStatus = a} :: AcceptAdministratorInvitationResponse)

instance
  Prelude.NFData
    AcceptAdministratorInvitationResponse
  where
  rnf AcceptAdministratorInvitationResponse' {..} =
    Prelude.rnf httpStatus

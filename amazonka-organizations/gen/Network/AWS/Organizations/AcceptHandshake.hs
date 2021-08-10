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
-- Module      : Network.AWS.Organizations.AcceptHandshake
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a response to the originator of a handshake agreeing to the action
-- proposed by the handshake request.
--
-- This operation can be called only by the following principals when they
-- also have the relevant IAM permissions:
--
-- -   __Invitation to join__ or __Approve all features request__
--     handshakes: only a principal from the member account.
--
--     The user who calls the API for an invitation to join must have the
--     @organizations:AcceptHandshake@ permission. If you enabled all
--     features in the organization, the user must also have the
--     @iam:CreateServiceLinkedRole@ permission so that AWS Organizations
--     can create the required service-linked role named
--     @AWSServiceRoleForOrganizations@. For more information, see
--     <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integration_services.html#orgs_integration_service-linked-roles AWS Organizations and Service-Linked Roles>
--     in the /AWS Organizations User Guide/.
--
-- -   __Enable all features final confirmation__ handshake: only a
--     principal from the management account.
--
--     For more information about invitations, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_accounts_invites.html Inviting an AWS Account to Join Your Organization>
--     in the /AWS Organizations User Guide./ For more information about
--     requests to enable all features in the organization, see
--     <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html Enabling All Features in Your Organization>
--     in the /AWS Organizations User Guide./
--
-- After you accept a handshake, it continues to appear in the results of
-- relevant APIs for only 30 days. After that, it\'s deleted.
module Network.AWS.Organizations.AcceptHandshake
  ( -- * Creating a Request
    AcceptHandshake (..),
    newAcceptHandshake,

    -- * Request Lenses
    acceptHandshake_handshakeId,

    -- * Destructuring the Response
    AcceptHandshakeResponse (..),
    newAcceptHandshakeResponse,

    -- * Response Lenses
    acceptHandshakeResponse_handshake,
    acceptHandshakeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptHandshake' smart constructor.
data AcceptHandshake = AcceptHandshake'
  { -- | The unique identifier (ID) of the handshake that you want to accept.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
    -- string requires \"h-\" followed by from 8 to 32 lowercase letters or
    -- digits.
    handshakeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptHandshake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshakeId', 'acceptHandshake_handshakeId' - The unique identifier (ID) of the handshake that you want to accept.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
newAcceptHandshake ::
  -- | 'handshakeId'
  Prelude.Text ->
  AcceptHandshake
newAcceptHandshake pHandshakeId_ =
  AcceptHandshake' {handshakeId = pHandshakeId_}

-- | The unique identifier (ID) of the handshake that you want to accept.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
acceptHandshake_handshakeId :: Lens.Lens' AcceptHandshake Prelude.Text
acceptHandshake_handshakeId = Lens.lens (\AcceptHandshake' {handshakeId} -> handshakeId) (\s@AcceptHandshake' {} a -> s {handshakeId = a} :: AcceptHandshake)

instance Core.AWSRequest AcceptHandshake where
  type
    AWSResponse AcceptHandshake =
      AcceptHandshakeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptHandshakeResponse'
            Prelude.<$> (x Core..?> "Handshake")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AcceptHandshake

instance Prelude.NFData AcceptHandshake

instance Core.ToHeaders AcceptHandshake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.AcceptHandshake" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AcceptHandshake where
  toJSON AcceptHandshake' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("HandshakeId" Core..= handshakeId)]
      )

instance Core.ToPath AcceptHandshake where
  toPath = Prelude.const "/"

instance Core.ToQuery AcceptHandshake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptHandshakeResponse' smart constructor.
data AcceptHandshakeResponse = AcceptHandshakeResponse'
  { -- | A structure that contains details about the accepted handshake.
    handshake :: Prelude.Maybe Handshake,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptHandshakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshake', 'acceptHandshakeResponse_handshake' - A structure that contains details about the accepted handshake.
--
-- 'httpStatus', 'acceptHandshakeResponse_httpStatus' - The response's http status code.
newAcceptHandshakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptHandshakeResponse
newAcceptHandshakeResponse pHttpStatus_ =
  AcceptHandshakeResponse'
    { handshake =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the accepted handshake.
acceptHandshakeResponse_handshake :: Lens.Lens' AcceptHandshakeResponse (Prelude.Maybe Handshake)
acceptHandshakeResponse_handshake = Lens.lens (\AcceptHandshakeResponse' {handshake} -> handshake) (\s@AcceptHandshakeResponse' {} a -> s {handshake = a} :: AcceptHandshakeResponse)

-- | The response's http status code.
acceptHandshakeResponse_httpStatus :: Lens.Lens' AcceptHandshakeResponse Prelude.Int
acceptHandshakeResponse_httpStatus = Lens.lens (\AcceptHandshakeResponse' {httpStatus} -> httpStatus) (\s@AcceptHandshakeResponse' {} a -> s {httpStatus = a} :: AcceptHandshakeResponse)

instance Prelude.NFData AcceptHandshakeResponse

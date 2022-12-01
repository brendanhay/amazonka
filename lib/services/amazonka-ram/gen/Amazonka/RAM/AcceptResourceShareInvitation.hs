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
-- Module      : Amazonka.RAM.AcceptResourceShareInvitation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts an invitation to a resource share from another Amazon Web
-- Services account. After you accept the invitation, the resources
-- included in the resource share are available to interact with in the
-- relevant Amazon Web Services Management Consoles and tools.
module Amazonka.RAM.AcceptResourceShareInvitation
  ( -- * Creating a Request
    AcceptResourceShareInvitation (..),
    newAcceptResourceShareInvitation,

    -- * Request Lenses
    acceptResourceShareInvitation_clientToken,
    acceptResourceShareInvitation_resourceShareInvitationArn,

    -- * Destructuring the Response
    AcceptResourceShareInvitationResponse (..),
    newAcceptResourceShareInvitationResponse,

    -- * Response Lenses
    acceptResourceShareInvitationResponse_clientToken,
    acceptResourceShareInvitationResponse_resourceShareInvitation,
    acceptResourceShareInvitationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptResourceShareInvitation' smart constructor.
data AcceptResourceShareInvitation = AcceptResourceShareInvitation'
  { -- | Specifies a unique, case-sensitive identifier that you provide to ensure
    -- the idempotency of the request. This lets you safely retry the request
    -- without accidentally performing the same operation a second time.
    -- Passing the same value to a later call to an operation requires that you
    -- also pass the same value for all other parameters. We recommend that you
    -- use a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
    --
    -- If you don\'t provide this value, then Amazon Web Services generates a
    -- random one for you.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the invitation that you want to accept.
    resourceShareInvitationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptResourceShareInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'acceptResourceShareInvitation_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
--
-- 'resourceShareInvitationArn', 'acceptResourceShareInvitation_resourceShareInvitationArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the invitation that you want to accept.
newAcceptResourceShareInvitation ::
  -- | 'resourceShareInvitationArn'
  Prelude.Text ->
  AcceptResourceShareInvitation
newAcceptResourceShareInvitation
  pResourceShareInvitationArn_ =
    AcceptResourceShareInvitation'
      { clientToken =
          Prelude.Nothing,
        resourceShareInvitationArn =
          pResourceShareInvitationArn_
      }

-- | Specifies a unique, case-sensitive identifier that you provide to ensure
-- the idempotency of the request. This lets you safely retry the request
-- without accidentally performing the same operation a second time.
-- Passing the same value to a later call to an operation requires that you
-- also pass the same value for all other parameters. We recommend that you
-- use a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID type of value.>.
--
-- If you don\'t provide this value, then Amazon Web Services generates a
-- random one for you.
acceptResourceShareInvitation_clientToken :: Lens.Lens' AcceptResourceShareInvitation (Prelude.Maybe Prelude.Text)
acceptResourceShareInvitation_clientToken = Lens.lens (\AcceptResourceShareInvitation' {clientToken} -> clientToken) (\s@AcceptResourceShareInvitation' {} a -> s {clientToken = a} :: AcceptResourceShareInvitation)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the invitation that you want to accept.
acceptResourceShareInvitation_resourceShareInvitationArn :: Lens.Lens' AcceptResourceShareInvitation Prelude.Text
acceptResourceShareInvitation_resourceShareInvitationArn = Lens.lens (\AcceptResourceShareInvitation' {resourceShareInvitationArn} -> resourceShareInvitationArn) (\s@AcceptResourceShareInvitation' {} a -> s {resourceShareInvitationArn = a} :: AcceptResourceShareInvitation)

instance
  Core.AWSRequest
    AcceptResourceShareInvitation
  where
  type
    AWSResponse AcceptResourceShareInvitation =
      AcceptResourceShareInvitationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptResourceShareInvitationResponse'
            Prelude.<$> (x Core..?> "clientToken")
            Prelude.<*> (x Core..?> "resourceShareInvitation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AcceptResourceShareInvitation
  where
  hashWithSalt _salt AcceptResourceShareInvitation' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` resourceShareInvitationArn

instance Prelude.NFData AcceptResourceShareInvitation where
  rnf AcceptResourceShareInvitation' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareInvitationArn

instance Core.ToHeaders AcceptResourceShareInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AcceptResourceShareInvitation where
  toJSON AcceptResourceShareInvitation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "resourceShareInvitationArn"
                  Core..= resourceShareInvitationArn
              )
          ]
      )

instance Core.ToPath AcceptResourceShareInvitation where
  toPath =
    Prelude.const "/acceptresourceshareinvitation"

instance Core.ToQuery AcceptResourceShareInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptResourceShareInvitationResponse' smart constructor.
data AcceptResourceShareInvitationResponse = AcceptResourceShareInvitationResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An object that contains information about the specified invitation.
    resourceShareInvitation :: Prelude.Maybe ResourceShareInvitation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptResourceShareInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'acceptResourceShareInvitationResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'resourceShareInvitation', 'acceptResourceShareInvitationResponse_resourceShareInvitation' - An object that contains information about the specified invitation.
--
-- 'httpStatus', 'acceptResourceShareInvitationResponse_httpStatus' - The response's http status code.
newAcceptResourceShareInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AcceptResourceShareInvitationResponse
newAcceptResourceShareInvitationResponse pHttpStatus_ =
  AcceptResourceShareInvitationResponse'
    { clientToken =
        Prelude.Nothing,
      resourceShareInvitation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
acceptResourceShareInvitationResponse_clientToken :: Lens.Lens' AcceptResourceShareInvitationResponse (Prelude.Maybe Prelude.Text)
acceptResourceShareInvitationResponse_clientToken = Lens.lens (\AcceptResourceShareInvitationResponse' {clientToken} -> clientToken) (\s@AcceptResourceShareInvitationResponse' {} a -> s {clientToken = a} :: AcceptResourceShareInvitationResponse)

-- | An object that contains information about the specified invitation.
acceptResourceShareInvitationResponse_resourceShareInvitation :: Lens.Lens' AcceptResourceShareInvitationResponse (Prelude.Maybe ResourceShareInvitation)
acceptResourceShareInvitationResponse_resourceShareInvitation = Lens.lens (\AcceptResourceShareInvitationResponse' {resourceShareInvitation} -> resourceShareInvitation) (\s@AcceptResourceShareInvitationResponse' {} a -> s {resourceShareInvitation = a} :: AcceptResourceShareInvitationResponse)

-- | The response's http status code.
acceptResourceShareInvitationResponse_httpStatus :: Lens.Lens' AcceptResourceShareInvitationResponse Prelude.Int
acceptResourceShareInvitationResponse_httpStatus = Lens.lens (\AcceptResourceShareInvitationResponse' {httpStatus} -> httpStatus) (\s@AcceptResourceShareInvitationResponse' {} a -> s {httpStatus = a} :: AcceptResourceShareInvitationResponse)

instance
  Prelude.NFData
    AcceptResourceShareInvitationResponse
  where
  rnf AcceptResourceShareInvitationResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf resourceShareInvitation
      `Prelude.seq` Prelude.rnf httpStatus

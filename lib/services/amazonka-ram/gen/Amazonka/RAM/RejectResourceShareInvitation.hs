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
-- Module      : Amazonka.RAM.RejectResourceShareInvitation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects an invitation to a resource share from another Amazon Web
-- Services account.
module Amazonka.RAM.RejectResourceShareInvitation
  ( -- * Creating a Request
    RejectResourceShareInvitation (..),
    newRejectResourceShareInvitation,

    -- * Request Lenses
    rejectResourceShareInvitation_clientToken,
    rejectResourceShareInvitation_resourceShareInvitationArn,

    -- * Destructuring the Response
    RejectResourceShareInvitationResponse (..),
    newRejectResourceShareInvitationResponse,

    -- * Response Lenses
    rejectResourceShareInvitationResponse_clientToken,
    rejectResourceShareInvitationResponse_resourceShareInvitation,
    rejectResourceShareInvitationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectResourceShareInvitation' smart constructor.
data RejectResourceShareInvitation = RejectResourceShareInvitation'
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
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the invitation that you want to reject.
    resourceShareInvitationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectResourceShareInvitation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'rejectResourceShareInvitation_clientToken' - Specifies a unique, case-sensitive identifier that you provide to ensure
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
-- 'resourceShareInvitationArn', 'rejectResourceShareInvitation_resourceShareInvitationArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the invitation that you want to reject.
newRejectResourceShareInvitation ::
  -- | 'resourceShareInvitationArn'
  Prelude.Text ->
  RejectResourceShareInvitation
newRejectResourceShareInvitation
  pResourceShareInvitationArn_ =
    RejectResourceShareInvitation'
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
rejectResourceShareInvitation_clientToken :: Lens.Lens' RejectResourceShareInvitation (Prelude.Maybe Prelude.Text)
rejectResourceShareInvitation_clientToken = Lens.lens (\RejectResourceShareInvitation' {clientToken} -> clientToken) (\s@RejectResourceShareInvitation' {} a -> s {clientToken = a} :: RejectResourceShareInvitation)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the invitation that you want to reject.
rejectResourceShareInvitation_resourceShareInvitationArn :: Lens.Lens' RejectResourceShareInvitation Prelude.Text
rejectResourceShareInvitation_resourceShareInvitationArn = Lens.lens (\RejectResourceShareInvitation' {resourceShareInvitationArn} -> resourceShareInvitationArn) (\s@RejectResourceShareInvitation' {} a -> s {resourceShareInvitationArn = a} :: RejectResourceShareInvitation)

instance
  Core.AWSRequest
    RejectResourceShareInvitation
  where
  type
    AWSResponse RejectResourceShareInvitation =
      RejectResourceShareInvitationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectResourceShareInvitationResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "resourceShareInvitation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectResourceShareInvitation
  where
  hashWithSalt _salt RejectResourceShareInvitation' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` resourceShareInvitationArn

instance Prelude.NFData RejectResourceShareInvitation where
  rnf RejectResourceShareInvitation' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf resourceShareInvitationArn

instance Data.ToHeaders RejectResourceShareInvitation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RejectResourceShareInvitation where
  toJSON RejectResourceShareInvitation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "resourceShareInvitationArn"
                  Data..= resourceShareInvitationArn
              )
          ]
      )

instance Data.ToPath RejectResourceShareInvitation where
  toPath =
    Prelude.const "/rejectresourceshareinvitation"

instance Data.ToQuery RejectResourceShareInvitation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectResourceShareInvitationResponse' smart constructor.
data RejectResourceShareInvitationResponse = RejectResourceShareInvitationResponse'
  { -- | The idempotency identifier associated with this request. If you want to
    -- repeat the same operation in an idempotent manner then you must include
    -- this value in the @clientToken@ request parameter of that later call.
    -- All other parameters must also have the same values that you used in the
    -- first call.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An object that contains the details about the rejected invitation.
    resourceShareInvitation :: Prelude.Maybe ResourceShareInvitation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectResourceShareInvitationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'rejectResourceShareInvitationResponse_clientToken' - The idempotency identifier associated with this request. If you want to
-- repeat the same operation in an idempotent manner then you must include
-- this value in the @clientToken@ request parameter of that later call.
-- All other parameters must also have the same values that you used in the
-- first call.
--
-- 'resourceShareInvitation', 'rejectResourceShareInvitationResponse_resourceShareInvitation' - An object that contains the details about the rejected invitation.
--
-- 'httpStatus', 'rejectResourceShareInvitationResponse_httpStatus' - The response's http status code.
newRejectResourceShareInvitationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectResourceShareInvitationResponse
newRejectResourceShareInvitationResponse pHttpStatus_ =
  RejectResourceShareInvitationResponse'
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
rejectResourceShareInvitationResponse_clientToken :: Lens.Lens' RejectResourceShareInvitationResponse (Prelude.Maybe Prelude.Text)
rejectResourceShareInvitationResponse_clientToken = Lens.lens (\RejectResourceShareInvitationResponse' {clientToken} -> clientToken) (\s@RejectResourceShareInvitationResponse' {} a -> s {clientToken = a} :: RejectResourceShareInvitationResponse)

-- | An object that contains the details about the rejected invitation.
rejectResourceShareInvitationResponse_resourceShareInvitation :: Lens.Lens' RejectResourceShareInvitationResponse (Prelude.Maybe ResourceShareInvitation)
rejectResourceShareInvitationResponse_resourceShareInvitation = Lens.lens (\RejectResourceShareInvitationResponse' {resourceShareInvitation} -> resourceShareInvitation) (\s@RejectResourceShareInvitationResponse' {} a -> s {resourceShareInvitation = a} :: RejectResourceShareInvitationResponse)

-- | The response's http status code.
rejectResourceShareInvitationResponse_httpStatus :: Lens.Lens' RejectResourceShareInvitationResponse Prelude.Int
rejectResourceShareInvitationResponse_httpStatus = Lens.lens (\RejectResourceShareInvitationResponse' {httpStatus} -> httpStatus) (\s@RejectResourceShareInvitationResponse' {} a -> s {httpStatus = a} :: RejectResourceShareInvitationResponse)

instance
  Prelude.NFData
    RejectResourceShareInvitationResponse
  where
  rnf RejectResourceShareInvitationResponse' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf resourceShareInvitation `Prelude.seq`
        Prelude.rnf httpStatus

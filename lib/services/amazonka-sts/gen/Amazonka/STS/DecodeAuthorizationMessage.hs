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
-- Module      : Amazonka.STS.DecodeAuthorizationMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decodes additional information about the authorization status of a
-- request from an encoded message returned in response to an Amazon Web
-- Services request.
--
-- For example, if a user is not authorized to perform an operation that he
-- or she has requested, the request returns a
-- @Client.UnauthorizedOperation@ response (an HTTP 403 response). Some
-- Amazon Web Services operations additionally return an encoded message
-- that can provide details about this authorization failure.
--
-- Only certain Amazon Web Services operations return an encoded
-- authorization message. The documentation for an individual operation
-- indicates whether that operation returns an encoded message in addition
-- to returning an HTTP code.
--
-- The message is encoded because the details of the authorization status
-- can contain privileged information that the user who requested the
-- operation should not see. To decode an authorization status message, a
-- user must be granted permissions through an IAM
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html policy>
-- to request the @DecodeAuthorizationMessage@
-- (@sts:DecodeAuthorizationMessage@) action.
--
-- The decoded message includes the following type of information:
--
-- -   Whether the request was denied due to an explicit deny or due to the
--     absence of an explicit allow. For more information, see
--     <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html#policy-eval-denyallow Determining Whether a Request is Allowed or Denied>
--     in the /IAM User Guide/.
--
-- -   The principal who made the request.
--
-- -   The requested action.
--
-- -   The requested resource.
--
-- -   The values of condition keys in the context of the user\'s request.
module Amazonka.STS.DecodeAuthorizationMessage
  ( -- * Creating a Request
    DecodeAuthorizationMessage (..),
    newDecodeAuthorizationMessage,

    -- * Request Lenses
    decodeAuthorizationMessage_encodedMessage,

    -- * Destructuring the Response
    DecodeAuthorizationMessageResponse (..),
    newDecodeAuthorizationMessageResponse,

    -- * Response Lenses
    decodeAuthorizationMessageResponse_decodedMessage,
    decodeAuthorizationMessageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.STS.Types

-- | /See:/ 'newDecodeAuthorizationMessage' smart constructor.
data DecodeAuthorizationMessage = DecodeAuthorizationMessage'
  { -- | The encoded message that was returned with the response.
    encodedMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecodeAuthorizationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encodedMessage', 'decodeAuthorizationMessage_encodedMessage' - The encoded message that was returned with the response.
newDecodeAuthorizationMessage ::
  -- | 'encodedMessage'
  Prelude.Text ->
  DecodeAuthorizationMessage
newDecodeAuthorizationMessage pEncodedMessage_ =
  DecodeAuthorizationMessage'
    { encodedMessage =
        pEncodedMessage_
    }

-- | The encoded message that was returned with the response.
decodeAuthorizationMessage_encodedMessage :: Lens.Lens' DecodeAuthorizationMessage Prelude.Text
decodeAuthorizationMessage_encodedMessage = Lens.lens (\DecodeAuthorizationMessage' {encodedMessage} -> encodedMessage) (\s@DecodeAuthorizationMessage' {} a -> s {encodedMessage = a} :: DecodeAuthorizationMessage)

instance Core.AWSRequest DecodeAuthorizationMessage where
  type
    AWSResponse DecodeAuthorizationMessage =
      DecodeAuthorizationMessageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DecodeAuthorizationMessageResult"
      ( \s h x ->
          DecodeAuthorizationMessageResponse'
            Prelude.<$> (x Data..@? "DecodedMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DecodeAuthorizationMessage where
  hashWithSalt _salt DecodeAuthorizationMessage' {..} =
    _salt `Prelude.hashWithSalt` encodedMessage

instance Prelude.NFData DecodeAuthorizationMessage where
  rnf DecodeAuthorizationMessage' {..} =
    Prelude.rnf encodedMessage

instance Data.ToHeaders DecodeAuthorizationMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DecodeAuthorizationMessage where
  toPath = Prelude.const "/"

instance Data.ToQuery DecodeAuthorizationMessage where
  toQuery DecodeAuthorizationMessage' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DecodeAuthorizationMessage" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-06-15" :: Prelude.ByteString),
        "EncodedMessage" Data.=: encodedMessage
      ]

-- | A document that contains additional information about the authorization
-- status of a request from an encoded message that is returned in response
-- to an Amazon Web Services request.
--
-- /See:/ 'newDecodeAuthorizationMessageResponse' smart constructor.
data DecodeAuthorizationMessageResponse = DecodeAuthorizationMessageResponse'
  { -- | The API returns a response with the decoded message.
    decodedMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DecodeAuthorizationMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'decodedMessage', 'decodeAuthorizationMessageResponse_decodedMessage' - The API returns a response with the decoded message.
--
-- 'httpStatus', 'decodeAuthorizationMessageResponse_httpStatus' - The response's http status code.
newDecodeAuthorizationMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DecodeAuthorizationMessageResponse
newDecodeAuthorizationMessageResponse pHttpStatus_ =
  DecodeAuthorizationMessageResponse'
    { decodedMessage =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The API returns a response with the decoded message.
decodeAuthorizationMessageResponse_decodedMessage :: Lens.Lens' DecodeAuthorizationMessageResponse (Prelude.Maybe Prelude.Text)
decodeAuthorizationMessageResponse_decodedMessage = Lens.lens (\DecodeAuthorizationMessageResponse' {decodedMessage} -> decodedMessage) (\s@DecodeAuthorizationMessageResponse' {} a -> s {decodedMessage = a} :: DecodeAuthorizationMessageResponse)

-- | The response's http status code.
decodeAuthorizationMessageResponse_httpStatus :: Lens.Lens' DecodeAuthorizationMessageResponse Prelude.Int
decodeAuthorizationMessageResponse_httpStatus = Lens.lens (\DecodeAuthorizationMessageResponse' {httpStatus} -> httpStatus) (\s@DecodeAuthorizationMessageResponse' {} a -> s {httpStatus = a} :: DecodeAuthorizationMessageResponse)

instance
  Prelude.NFData
    DecodeAuthorizationMessageResponse
  where
  rnf DecodeAuthorizationMessageResponse' {..} =
    Prelude.rnf decodedMessage
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.SES.VerifyEmailIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an email address to the list of identities for your Amazon SES
-- account in the current AWS region and attempts to verify it. As a result
-- of executing this operation, a verification email is sent to the
-- specified address.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.VerifyEmailIdentity
  ( -- * Creating a Request
    VerifyEmailIdentity (..),
    newVerifyEmailIdentity,

    -- * Request Lenses
    verifyEmailIdentity_emailAddress,

    -- * Destructuring the Response
    VerifyEmailIdentityResponse (..),
    newVerifyEmailIdentityResponse,

    -- * Response Lenses
    verifyEmailIdentityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to begin email address verification with Amazon
-- SES. For information about email address verification, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html Amazon SES Developer Guide>.
--
-- /See:/ 'newVerifyEmailIdentity' smart constructor.
data VerifyEmailIdentity = VerifyEmailIdentity'
  { -- | The email address to be verified.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyEmailIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'verifyEmailIdentity_emailAddress' - The email address to be verified.
newVerifyEmailIdentity ::
  -- | 'emailAddress'
  Prelude.Text ->
  VerifyEmailIdentity
newVerifyEmailIdentity pEmailAddress_ =
  VerifyEmailIdentity' {emailAddress = pEmailAddress_}

-- | The email address to be verified.
verifyEmailIdentity_emailAddress :: Lens.Lens' VerifyEmailIdentity Prelude.Text
verifyEmailIdentity_emailAddress = Lens.lens (\VerifyEmailIdentity' {emailAddress} -> emailAddress) (\s@VerifyEmailIdentity' {} a -> s {emailAddress = a} :: VerifyEmailIdentity)

instance Core.AWSRequest VerifyEmailIdentity where
  type
    AWSResponse VerifyEmailIdentity =
      VerifyEmailIdentityResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "VerifyEmailIdentityResult"
      ( \s h x ->
          VerifyEmailIdentityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable VerifyEmailIdentity where
  hashWithSalt _salt VerifyEmailIdentity' {..} =
    _salt `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData VerifyEmailIdentity where
  rnf VerifyEmailIdentity' {..} =
    Prelude.rnf emailAddress

instance Data.ToHeaders VerifyEmailIdentity where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath VerifyEmailIdentity where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifyEmailIdentity where
  toQuery VerifyEmailIdentity' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("VerifyEmailIdentity" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "EmailAddress" Data.=: emailAddress
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newVerifyEmailIdentityResponse' smart constructor.
data VerifyEmailIdentityResponse = VerifyEmailIdentityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyEmailIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifyEmailIdentityResponse_httpStatus' - The response's http status code.
newVerifyEmailIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  VerifyEmailIdentityResponse
newVerifyEmailIdentityResponse pHttpStatus_ =
  VerifyEmailIdentityResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
verifyEmailIdentityResponse_httpStatus :: Lens.Lens' VerifyEmailIdentityResponse Prelude.Int
verifyEmailIdentityResponse_httpStatus = Lens.lens (\VerifyEmailIdentityResponse' {httpStatus} -> httpStatus) (\s@VerifyEmailIdentityResponse' {} a -> s {httpStatus = a} :: VerifyEmailIdentityResponse)

instance Prelude.NFData VerifyEmailIdentityResponse where
  rnf VerifyEmailIdentityResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.SES.VerifyEmailAddress
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @VerifyEmailIdentity@ operation to verify a new
-- email address.
module Amazonka.SES.VerifyEmailAddress
  ( -- * Creating a Request
    VerifyEmailAddress (..),
    newVerifyEmailAddress,

    -- * Request Lenses
    verifyEmailAddress_emailAddress,

    -- * Destructuring the Response
    VerifyEmailAddressResponse (..),
    newVerifyEmailAddressResponse,
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
-- /See:/ 'newVerifyEmailAddress' smart constructor.
data VerifyEmailAddress = VerifyEmailAddress'
  { -- | The email address to be verified.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyEmailAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'verifyEmailAddress_emailAddress' - The email address to be verified.
newVerifyEmailAddress ::
  -- | 'emailAddress'
  Prelude.Text ->
  VerifyEmailAddress
newVerifyEmailAddress pEmailAddress_ =
  VerifyEmailAddress' {emailAddress = pEmailAddress_}

-- | The email address to be verified.
verifyEmailAddress_emailAddress :: Lens.Lens' VerifyEmailAddress Prelude.Text
verifyEmailAddress_emailAddress = Lens.lens (\VerifyEmailAddress' {emailAddress} -> emailAddress) (\s@VerifyEmailAddress' {} a -> s {emailAddress = a} :: VerifyEmailAddress)

instance Core.AWSRequest VerifyEmailAddress where
  type
    AWSResponse VerifyEmailAddress =
      VerifyEmailAddressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull VerifyEmailAddressResponse'

instance Prelude.Hashable VerifyEmailAddress where
  hashWithSalt _salt VerifyEmailAddress' {..} =
    _salt `Prelude.hashWithSalt` emailAddress

instance Prelude.NFData VerifyEmailAddress where
  rnf VerifyEmailAddress' {..} =
    Prelude.rnf emailAddress

instance Data.ToHeaders VerifyEmailAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath VerifyEmailAddress where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifyEmailAddress where
  toQuery VerifyEmailAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("VerifyEmailAddress" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "EmailAddress" Data.=: emailAddress
      ]

-- | /See:/ 'newVerifyEmailAddressResponse' smart constructor.
data VerifyEmailAddressResponse = VerifyEmailAddressResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyEmailAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newVerifyEmailAddressResponse ::
  VerifyEmailAddressResponse
newVerifyEmailAddressResponse =
  VerifyEmailAddressResponse'

instance Prelude.NFData VerifyEmailAddressResponse where
  rnf _ = ()

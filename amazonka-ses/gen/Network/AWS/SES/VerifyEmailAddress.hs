{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.VerifyEmailAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecated. Use the @VerifyEmailIdentity@ operation to verify a new
-- email address.
module Network.AWS.SES.VerifyEmailAddress
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to begin email address verification with Amazon
-- SES. For information about email address verification, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-email-addresses.html Amazon SES Developer Guide>.
--
-- /See:/ 'newVerifyEmailAddress' smart constructor.
data VerifyEmailAddress = VerifyEmailAddress'
  { -- | The email address to be verified.
    emailAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest VerifyEmailAddress where
  type
    Rs VerifyEmailAddress =
      VerifyEmailAddressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull VerifyEmailAddressResponse'

instance Prelude.Hashable VerifyEmailAddress

instance Prelude.NFData VerifyEmailAddress

instance Prelude.ToHeaders VerifyEmailAddress where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath VerifyEmailAddress where
  toPath = Prelude.const "/"

instance Prelude.ToQuery VerifyEmailAddress where
  toQuery VerifyEmailAddress' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("VerifyEmailAddress" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "EmailAddress" Prelude.=: emailAddress
      ]

-- | /See:/ 'newVerifyEmailAddressResponse' smart constructor.
data VerifyEmailAddressResponse = VerifyEmailAddressResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VerifyEmailAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newVerifyEmailAddressResponse ::
  VerifyEmailAddressResponse
newVerifyEmailAddressResponse =
  VerifyEmailAddressResponse'

instance Prelude.NFData VerifyEmailAddressResponse

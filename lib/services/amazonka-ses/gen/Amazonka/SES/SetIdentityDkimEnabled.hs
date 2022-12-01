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
-- Module      : Amazonka.SES.SetIdentityDkimEnabled
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables Easy DKIM signing of email sent from an identity. If
-- Easy DKIM signing is enabled for a domain, then Amazon SES uses DKIM to
-- sign all email that it sends from addresses on that domain. If Easy DKIM
-- signing is enabled for an email address, then Amazon SES uses DKIM to
-- sign all email it sends from that address.
--
-- For email addresses (for example, @user\@example.com@), you can only
-- enable DKIM signing if the corresponding domain (in this case,
-- @example.com@) has been set up to use Easy DKIM.
--
-- You can enable DKIM signing for an identity at any time after you start
-- the verification process for the identity, even if the verification
-- process isn\'t complete.
--
-- You can execute this operation no more than once per second.
--
-- For more information about Easy DKIM signing, go to the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
module Amazonka.SES.SetIdentityDkimEnabled
  ( -- * Creating a Request
    SetIdentityDkimEnabled (..),
    newSetIdentityDkimEnabled,

    -- * Request Lenses
    setIdentityDkimEnabled_identity,
    setIdentityDkimEnabled_dkimEnabled,

    -- * Destructuring the Response
    SetIdentityDkimEnabledResponse (..),
    newSetIdentityDkimEnabledResponse,

    -- * Response Lenses
    setIdentityDkimEnabledResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to enable or disable Amazon SES Easy DKIM signing
-- for an identity. For more information about setting up Easy DKIM, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSetIdentityDkimEnabled' smart constructor.
data SetIdentityDkimEnabled = SetIdentityDkimEnabled'
  { -- | The identity for which DKIM signing should be enabled or disabled.
    identity :: Prelude.Text,
    -- | Sets whether DKIM signing is enabled for an identity. Set to @true@ to
    -- enable DKIM signing for this identity; @false@ to disable it.
    dkimEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityDkimEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'setIdentityDkimEnabled_identity' - The identity for which DKIM signing should be enabled or disabled.
--
-- 'dkimEnabled', 'setIdentityDkimEnabled_dkimEnabled' - Sets whether DKIM signing is enabled for an identity. Set to @true@ to
-- enable DKIM signing for this identity; @false@ to disable it.
newSetIdentityDkimEnabled ::
  -- | 'identity'
  Prelude.Text ->
  -- | 'dkimEnabled'
  Prelude.Bool ->
  SetIdentityDkimEnabled
newSetIdentityDkimEnabled pIdentity_ pDkimEnabled_ =
  SetIdentityDkimEnabled'
    { identity = pIdentity_,
      dkimEnabled = pDkimEnabled_
    }

-- | The identity for which DKIM signing should be enabled or disabled.
setIdentityDkimEnabled_identity :: Lens.Lens' SetIdentityDkimEnabled Prelude.Text
setIdentityDkimEnabled_identity = Lens.lens (\SetIdentityDkimEnabled' {identity} -> identity) (\s@SetIdentityDkimEnabled' {} a -> s {identity = a} :: SetIdentityDkimEnabled)

-- | Sets whether DKIM signing is enabled for an identity. Set to @true@ to
-- enable DKIM signing for this identity; @false@ to disable it.
setIdentityDkimEnabled_dkimEnabled :: Lens.Lens' SetIdentityDkimEnabled Prelude.Bool
setIdentityDkimEnabled_dkimEnabled = Lens.lens (\SetIdentityDkimEnabled' {dkimEnabled} -> dkimEnabled) (\s@SetIdentityDkimEnabled' {} a -> s {dkimEnabled = a} :: SetIdentityDkimEnabled)

instance Core.AWSRequest SetIdentityDkimEnabled where
  type
    AWSResponse SetIdentityDkimEnabled =
      SetIdentityDkimEnabledResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetIdentityDkimEnabledResult"
      ( \s h x ->
          SetIdentityDkimEnabledResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetIdentityDkimEnabled where
  hashWithSalt _salt SetIdentityDkimEnabled' {..} =
    _salt `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` dkimEnabled

instance Prelude.NFData SetIdentityDkimEnabled where
  rnf SetIdentityDkimEnabled' {..} =
    Prelude.rnf identity
      `Prelude.seq` Prelude.rnf dkimEnabled

instance Core.ToHeaders SetIdentityDkimEnabled where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath SetIdentityDkimEnabled where
  toPath = Prelude.const "/"

instance Core.ToQuery SetIdentityDkimEnabled where
  toQuery SetIdentityDkimEnabled' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("SetIdentityDkimEnabled" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "Identity" Core.=: identity,
        "DkimEnabled" Core.=: dkimEnabled
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newSetIdentityDkimEnabledResponse' smart constructor.
data SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityDkimEnabledResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setIdentityDkimEnabledResponse_httpStatus' - The response's http status code.
newSetIdentityDkimEnabledResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetIdentityDkimEnabledResponse
newSetIdentityDkimEnabledResponse pHttpStatus_ =
  SetIdentityDkimEnabledResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setIdentityDkimEnabledResponse_httpStatus :: Lens.Lens' SetIdentityDkimEnabledResponse Prelude.Int
setIdentityDkimEnabledResponse_httpStatus = Lens.lens (\SetIdentityDkimEnabledResponse' {httpStatus} -> httpStatus) (\s@SetIdentityDkimEnabledResponse' {} a -> s {httpStatus = a} :: SetIdentityDkimEnabledResponse)

instance
  Prelude.NFData
    SetIdentityDkimEnabledResponse
  where
  rnf SetIdentityDkimEnabledResponse' {..} =
    Prelude.rnf httpStatus

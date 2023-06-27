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
-- Module      : Amazonka.SES.SetIdentityMailFromDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables the custom MAIL FROM domain setup for a verified
-- identity (an email address or a domain).
--
-- To send emails using the specified MAIL FROM domain, you must add an MX
-- record to your MAIL FROM domain\'s DNS settings. If you want your emails
-- to pass Sender Policy Framework (SPF) checks, you must also add or
-- update an SPF record. For more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from-set.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.SetIdentityMailFromDomain
  ( -- * Creating a Request
    SetIdentityMailFromDomain (..),
    newSetIdentityMailFromDomain,

    -- * Request Lenses
    setIdentityMailFromDomain_behaviorOnMXFailure,
    setIdentityMailFromDomain_mailFromDomain,
    setIdentityMailFromDomain_identity,

    -- * Destructuring the Response
    SetIdentityMailFromDomainResponse (..),
    newSetIdentityMailFromDomainResponse,

    -- * Response Lenses
    setIdentityMailFromDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to enable or disable the Amazon SES custom MAIL
-- FROM domain setup for a verified identity. For information about using a
-- custom MAIL FROM domain, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSetIdentityMailFromDomain' smart constructor.
data SetIdentityMailFromDomain = SetIdentityMailFromDomain'
  { -- | The action that you want Amazon SES to take if it cannot successfully
    -- read the required MX record when you send an email. If you choose
    -- @UseDefaultValue@, Amazon SES will use amazonses.com (or a subdomain of
    -- that) as the MAIL FROM domain. If you choose @RejectMessage@, Amazon SES
    -- will return a @MailFromDomainNotVerified@ error and not send the email.
    --
    -- The action specified in @BehaviorOnMXFailure@ is taken when the custom
    -- MAIL FROM domain setup is in the @Pending@, @Failed@, and
    -- @TemporaryFailure@ states.
    behaviorOnMXFailure :: Prelude.Maybe BehaviorOnMXFailure,
    -- | The custom MAIL FROM domain that you want the verified identity to use.
    -- The MAIL FROM domain must 1) be a subdomain of the verified identity, 2)
    -- not be used in a \"From\" address if the MAIL FROM domain is the
    -- destination of email feedback forwarding (for more information, see the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide>),
    -- and 3) not be used to receive emails. A value of @null@ disables the
    -- custom MAIL FROM setting for the identity.
    mailFromDomain :: Prelude.Maybe Prelude.Text,
    -- | The verified identity for which you want to enable or disable the
    -- specified custom MAIL FROM domain.
    identity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityMailFromDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'behaviorOnMXFailure', 'setIdentityMailFromDomain_behaviorOnMXFailure' - The action that you want Amazon SES to take if it cannot successfully
-- read the required MX record when you send an email. If you choose
-- @UseDefaultValue@, Amazon SES will use amazonses.com (or a subdomain of
-- that) as the MAIL FROM domain. If you choose @RejectMessage@, Amazon SES
-- will return a @MailFromDomainNotVerified@ error and not send the email.
--
-- The action specified in @BehaviorOnMXFailure@ is taken when the custom
-- MAIL FROM domain setup is in the @Pending@, @Failed@, and
-- @TemporaryFailure@ states.
--
-- 'mailFromDomain', 'setIdentityMailFromDomain_mailFromDomain' - The custom MAIL FROM domain that you want the verified identity to use.
-- The MAIL FROM domain must 1) be a subdomain of the verified identity, 2)
-- not be used in a \"From\" address if the MAIL FROM domain is the
-- destination of email feedback forwarding (for more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide>),
-- and 3) not be used to receive emails. A value of @null@ disables the
-- custom MAIL FROM setting for the identity.
--
-- 'identity', 'setIdentityMailFromDomain_identity' - The verified identity for which you want to enable or disable the
-- specified custom MAIL FROM domain.
newSetIdentityMailFromDomain ::
  -- | 'identity'
  Prelude.Text ->
  SetIdentityMailFromDomain
newSetIdentityMailFromDomain pIdentity_ =
  SetIdentityMailFromDomain'
    { behaviorOnMXFailure =
        Prelude.Nothing,
      mailFromDomain = Prelude.Nothing,
      identity = pIdentity_
    }

-- | The action that you want Amazon SES to take if it cannot successfully
-- read the required MX record when you send an email. If you choose
-- @UseDefaultValue@, Amazon SES will use amazonses.com (or a subdomain of
-- that) as the MAIL FROM domain. If you choose @RejectMessage@, Amazon SES
-- will return a @MailFromDomainNotVerified@ error and not send the email.
--
-- The action specified in @BehaviorOnMXFailure@ is taken when the custom
-- MAIL FROM domain setup is in the @Pending@, @Failed@, and
-- @TemporaryFailure@ states.
setIdentityMailFromDomain_behaviorOnMXFailure :: Lens.Lens' SetIdentityMailFromDomain (Prelude.Maybe BehaviorOnMXFailure)
setIdentityMailFromDomain_behaviorOnMXFailure = Lens.lens (\SetIdentityMailFromDomain' {behaviorOnMXFailure} -> behaviorOnMXFailure) (\s@SetIdentityMailFromDomain' {} a -> s {behaviorOnMXFailure = a} :: SetIdentityMailFromDomain)

-- | The custom MAIL FROM domain that you want the verified identity to use.
-- The MAIL FROM domain must 1) be a subdomain of the verified identity, 2)
-- not be used in a \"From\" address if the MAIL FROM domain is the
-- destination of email feedback forwarding (for more information, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide>),
-- and 3) not be used to receive emails. A value of @null@ disables the
-- custom MAIL FROM setting for the identity.
setIdentityMailFromDomain_mailFromDomain :: Lens.Lens' SetIdentityMailFromDomain (Prelude.Maybe Prelude.Text)
setIdentityMailFromDomain_mailFromDomain = Lens.lens (\SetIdentityMailFromDomain' {mailFromDomain} -> mailFromDomain) (\s@SetIdentityMailFromDomain' {} a -> s {mailFromDomain = a} :: SetIdentityMailFromDomain)

-- | The verified identity for which you want to enable or disable the
-- specified custom MAIL FROM domain.
setIdentityMailFromDomain_identity :: Lens.Lens' SetIdentityMailFromDomain Prelude.Text
setIdentityMailFromDomain_identity = Lens.lens (\SetIdentityMailFromDomain' {identity} -> identity) (\s@SetIdentityMailFromDomain' {} a -> s {identity = a} :: SetIdentityMailFromDomain)

instance Core.AWSRequest SetIdentityMailFromDomain where
  type
    AWSResponse SetIdentityMailFromDomain =
      SetIdentityMailFromDomainResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetIdentityMailFromDomainResult"
      ( \s h x ->
          SetIdentityMailFromDomainResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetIdentityMailFromDomain where
  hashWithSalt _salt SetIdentityMailFromDomain' {..} =
    _salt
      `Prelude.hashWithSalt` behaviorOnMXFailure
      `Prelude.hashWithSalt` mailFromDomain
      `Prelude.hashWithSalt` identity

instance Prelude.NFData SetIdentityMailFromDomain where
  rnf SetIdentityMailFromDomain' {..} =
    Prelude.rnf behaviorOnMXFailure
      `Prelude.seq` Prelude.rnf mailFromDomain
      `Prelude.seq` Prelude.rnf identity

instance Data.ToHeaders SetIdentityMailFromDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetIdentityMailFromDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery SetIdentityMailFromDomain where
  toQuery SetIdentityMailFromDomain' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetIdentityMailFromDomain" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "BehaviorOnMXFailure" Data.=: behaviorOnMXFailure,
        "MailFromDomain" Data.=: mailFromDomain,
        "Identity" Data.=: identity
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newSetIdentityMailFromDomainResponse' smart constructor.
data SetIdentityMailFromDomainResponse = SetIdentityMailFromDomainResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityMailFromDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setIdentityMailFromDomainResponse_httpStatus' - The response's http status code.
newSetIdentityMailFromDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetIdentityMailFromDomainResponse
newSetIdentityMailFromDomainResponse pHttpStatus_ =
  SetIdentityMailFromDomainResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setIdentityMailFromDomainResponse_httpStatus :: Lens.Lens' SetIdentityMailFromDomainResponse Prelude.Int
setIdentityMailFromDomainResponse_httpStatus = Lens.lens (\SetIdentityMailFromDomainResponse' {httpStatus} -> httpStatus) (\s@SetIdentityMailFromDomainResponse' {} a -> s {httpStatus = a} :: SetIdentityMailFromDomainResponse)

instance
  Prelude.NFData
    SetIdentityMailFromDomainResponse
  where
  rnf SetIdentityMailFromDomainResponse' {..} =
    Prelude.rnf httpStatus

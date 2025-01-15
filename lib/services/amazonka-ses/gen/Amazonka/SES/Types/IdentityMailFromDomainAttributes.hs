{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SES.Types.IdentityMailFromDomainAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.IdentityMailFromDomainAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.BehaviorOnMXFailure
import Amazonka.SES.Types.CustomMailFromStatus

-- | Represents the custom MAIL FROM domain attributes of a verified identity
-- (email address or domain).
--
-- /See:/ 'newIdentityMailFromDomainAttributes' smart constructor.
data IdentityMailFromDomainAttributes = IdentityMailFromDomainAttributes'
  { -- | The custom MAIL FROM domain that the identity is configured to use.
    mailFromDomain :: Prelude.Text,
    -- | The state that indicates whether Amazon SES has successfully read the MX
    -- record required for custom MAIL FROM domain setup. If the state is
    -- @Success@, Amazon SES uses the specified custom MAIL FROM domain when
    -- the verified identity sends an email. All other states indicate that
    -- Amazon SES takes the action described by @BehaviorOnMXFailure@.
    mailFromDomainStatus :: CustomMailFromStatus,
    -- | The action that Amazon SES takes if it cannot successfully read the
    -- required MX record when you send an email. A value of @UseDefaultValue@
    -- indicates that if Amazon SES cannot read the required MX record, it uses
    -- amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value
    -- of @RejectMessage@ indicates that if Amazon SES cannot read the required
    -- MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and
    -- does not send the email.
    --
    -- The custom MAIL FROM setup states that result in this behavior are
    -- @Pending@, @Failed@, and @TemporaryFailure@.
    behaviorOnMXFailure :: BehaviorOnMXFailure
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityMailFromDomainAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mailFromDomain', 'identityMailFromDomainAttributes_mailFromDomain' - The custom MAIL FROM domain that the identity is configured to use.
--
-- 'mailFromDomainStatus', 'identityMailFromDomainAttributes_mailFromDomainStatus' - The state that indicates whether Amazon SES has successfully read the MX
-- record required for custom MAIL FROM domain setup. If the state is
-- @Success@, Amazon SES uses the specified custom MAIL FROM domain when
-- the verified identity sends an email. All other states indicate that
-- Amazon SES takes the action described by @BehaviorOnMXFailure@.
--
-- 'behaviorOnMXFailure', 'identityMailFromDomainAttributes_behaviorOnMXFailure' - The action that Amazon SES takes if it cannot successfully read the
-- required MX record when you send an email. A value of @UseDefaultValue@
-- indicates that if Amazon SES cannot read the required MX record, it uses
-- amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value
-- of @RejectMessage@ indicates that if Amazon SES cannot read the required
-- MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and
-- does not send the email.
--
-- The custom MAIL FROM setup states that result in this behavior are
-- @Pending@, @Failed@, and @TemporaryFailure@.
newIdentityMailFromDomainAttributes ::
  -- | 'mailFromDomain'
  Prelude.Text ->
  -- | 'mailFromDomainStatus'
  CustomMailFromStatus ->
  -- | 'behaviorOnMXFailure'
  BehaviorOnMXFailure ->
  IdentityMailFromDomainAttributes
newIdentityMailFromDomainAttributes
  pMailFromDomain_
  pMailFromDomainStatus_
  pBehaviorOnMXFailure_ =
    IdentityMailFromDomainAttributes'
      { mailFromDomain =
          pMailFromDomain_,
        mailFromDomainStatus =
          pMailFromDomainStatus_,
        behaviorOnMXFailure =
          pBehaviorOnMXFailure_
      }

-- | The custom MAIL FROM domain that the identity is configured to use.
identityMailFromDomainAttributes_mailFromDomain :: Lens.Lens' IdentityMailFromDomainAttributes Prelude.Text
identityMailFromDomainAttributes_mailFromDomain = Lens.lens (\IdentityMailFromDomainAttributes' {mailFromDomain} -> mailFromDomain) (\s@IdentityMailFromDomainAttributes' {} a -> s {mailFromDomain = a} :: IdentityMailFromDomainAttributes)

-- | The state that indicates whether Amazon SES has successfully read the MX
-- record required for custom MAIL FROM domain setup. If the state is
-- @Success@, Amazon SES uses the specified custom MAIL FROM domain when
-- the verified identity sends an email. All other states indicate that
-- Amazon SES takes the action described by @BehaviorOnMXFailure@.
identityMailFromDomainAttributes_mailFromDomainStatus :: Lens.Lens' IdentityMailFromDomainAttributes CustomMailFromStatus
identityMailFromDomainAttributes_mailFromDomainStatus = Lens.lens (\IdentityMailFromDomainAttributes' {mailFromDomainStatus} -> mailFromDomainStatus) (\s@IdentityMailFromDomainAttributes' {} a -> s {mailFromDomainStatus = a} :: IdentityMailFromDomainAttributes)

-- | The action that Amazon SES takes if it cannot successfully read the
-- required MX record when you send an email. A value of @UseDefaultValue@
-- indicates that if Amazon SES cannot read the required MX record, it uses
-- amazonses.com (or a subdomain of that) as the MAIL FROM domain. A value
-- of @RejectMessage@ indicates that if Amazon SES cannot read the required
-- MX record, Amazon SES returns a @MailFromDomainNotVerified@ error and
-- does not send the email.
--
-- The custom MAIL FROM setup states that result in this behavior are
-- @Pending@, @Failed@, and @TemporaryFailure@.
identityMailFromDomainAttributes_behaviorOnMXFailure :: Lens.Lens' IdentityMailFromDomainAttributes BehaviorOnMXFailure
identityMailFromDomainAttributes_behaviorOnMXFailure = Lens.lens (\IdentityMailFromDomainAttributes' {behaviorOnMXFailure} -> behaviorOnMXFailure) (\s@IdentityMailFromDomainAttributes' {} a -> s {behaviorOnMXFailure = a} :: IdentityMailFromDomainAttributes)

instance
  Data.FromXML
    IdentityMailFromDomainAttributes
  where
  parseXML x =
    IdentityMailFromDomainAttributes'
      Prelude.<$> (x Data..@ "MailFromDomain")
      Prelude.<*> (x Data..@ "MailFromDomainStatus")
      Prelude.<*> (x Data..@ "BehaviorOnMXFailure")

instance
  Prelude.Hashable
    IdentityMailFromDomainAttributes
  where
  hashWithSalt
    _salt
    IdentityMailFromDomainAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` mailFromDomain
        `Prelude.hashWithSalt` mailFromDomainStatus
        `Prelude.hashWithSalt` behaviorOnMXFailure

instance
  Prelude.NFData
    IdentityMailFromDomainAttributes
  where
  rnf IdentityMailFromDomainAttributes' {..} =
    Prelude.rnf mailFromDomain `Prelude.seq`
      Prelude.rnf mailFromDomainStatus `Prelude.seq`
        Prelude.rnf behaviorOnMXFailure

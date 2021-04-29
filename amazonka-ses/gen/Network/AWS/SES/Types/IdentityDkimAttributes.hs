{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SES.Types.IdentityDkimAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityDkimAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.VerificationStatus

-- | Represents the DKIM attributes of a verified email address or a domain.
--
-- /See:/ 'newIdentityDkimAttributes' smart constructor.
data IdentityDkimAttributes = IdentityDkimAttributes'
  { -- | A set of character strings that represent the domain\'s identity. Using
    -- these tokens, you need to create DNS CNAME records that point to DKIM
    -- public keys that are hosted by Amazon SES. Amazon Web Services
    -- eventually detects that you\'ve updated your DNS records. This detection
    -- process might take up to 72 hours. After successful detection, Amazon
    -- SES is able to DKIM-sign email originating from that domain. (This only
    -- applies to domain identities, not email address identities.)
    --
    -- For more information about creating DNS records using DKIM tokens, see
    -- the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
    dkimTokens :: Prelude.Maybe [Prelude.Text],
    -- | Is true if DKIM signing is enabled for email sent from the identity.
    -- It\'s false otherwise. The default value is true.
    dkimEnabled :: Prelude.Bool,
    -- | Describes whether Amazon SES has successfully verified the DKIM DNS
    -- records (tokens) published in the domain name\'s DNS. (This only applies
    -- to domain identities, not email address identities.)
    dkimVerificationStatus :: VerificationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IdentityDkimAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dkimTokens', 'identityDkimAttributes_dkimTokens' - A set of character strings that represent the domain\'s identity. Using
-- these tokens, you need to create DNS CNAME records that point to DKIM
-- public keys that are hosted by Amazon SES. Amazon Web Services
-- eventually detects that you\'ve updated your DNS records. This detection
-- process might take up to 72 hours. After successful detection, Amazon
-- SES is able to DKIM-sign email originating from that domain. (This only
-- applies to domain identities, not email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
--
-- 'dkimEnabled', 'identityDkimAttributes_dkimEnabled' - Is true if DKIM signing is enabled for email sent from the identity.
-- It\'s false otherwise. The default value is true.
--
-- 'dkimVerificationStatus', 'identityDkimAttributes_dkimVerificationStatus' - Describes whether Amazon SES has successfully verified the DKIM DNS
-- records (tokens) published in the domain name\'s DNS. (This only applies
-- to domain identities, not email address identities.)
newIdentityDkimAttributes ::
  -- | 'dkimEnabled'
  Prelude.Bool ->
  -- | 'dkimVerificationStatus'
  VerificationStatus ->
  IdentityDkimAttributes
newIdentityDkimAttributes
  pDkimEnabled_
  pDkimVerificationStatus_ =
    IdentityDkimAttributes'
      { dkimTokens =
          Prelude.Nothing,
        dkimEnabled = pDkimEnabled_,
        dkimVerificationStatus = pDkimVerificationStatus_
      }

-- | A set of character strings that represent the domain\'s identity. Using
-- these tokens, you need to create DNS CNAME records that point to DKIM
-- public keys that are hosted by Amazon SES. Amazon Web Services
-- eventually detects that you\'ve updated your DNS records. This detection
-- process might take up to 72 hours. After successful detection, Amazon
-- SES is able to DKIM-sign email originating from that domain. (This only
-- applies to domain identities, not email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
identityDkimAttributes_dkimTokens :: Lens.Lens' IdentityDkimAttributes (Prelude.Maybe [Prelude.Text])
identityDkimAttributes_dkimTokens = Lens.lens (\IdentityDkimAttributes' {dkimTokens} -> dkimTokens) (\s@IdentityDkimAttributes' {} a -> s {dkimTokens = a} :: IdentityDkimAttributes) Prelude.. Lens.mapping Prelude._Coerce

-- | Is true if DKIM signing is enabled for email sent from the identity.
-- It\'s false otherwise. The default value is true.
identityDkimAttributes_dkimEnabled :: Lens.Lens' IdentityDkimAttributes Prelude.Bool
identityDkimAttributes_dkimEnabled = Lens.lens (\IdentityDkimAttributes' {dkimEnabled} -> dkimEnabled) (\s@IdentityDkimAttributes' {} a -> s {dkimEnabled = a} :: IdentityDkimAttributes)

-- | Describes whether Amazon SES has successfully verified the DKIM DNS
-- records (tokens) published in the domain name\'s DNS. (This only applies
-- to domain identities, not email address identities.)
identityDkimAttributes_dkimVerificationStatus :: Lens.Lens' IdentityDkimAttributes VerificationStatus
identityDkimAttributes_dkimVerificationStatus = Lens.lens (\IdentityDkimAttributes' {dkimVerificationStatus} -> dkimVerificationStatus) (\s@IdentityDkimAttributes' {} a -> s {dkimVerificationStatus = a} :: IdentityDkimAttributes)

instance Prelude.FromXML IdentityDkimAttributes where
  parseXML x =
    IdentityDkimAttributes'
      Prelude.<$> ( x Prelude..@? "DkimTokens"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@ "DkimEnabled")
      Prelude.<*> (x Prelude..@ "DkimVerificationStatus")

instance Prelude.Hashable IdentityDkimAttributes

instance Prelude.NFData IdentityDkimAttributes

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
-- Module      : Network.AWS.SESv2.Types.DkimAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.DkimAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.DkimSigningAttributesOrigin
import Network.AWS.SESv2.Types.DkimStatus

-- | An object that contains information about the DKIM authentication status
-- for an email identity.
--
-- Amazon SES determines the authentication status by searching for
-- specific records in the DNS configuration for the domain. If you used
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
-- to set up DKIM authentication, Amazon SES tries to find three unique
-- CNAME records in the DNS configuration for your domain. If you provided
-- a public key to perform DKIM authentication, Amazon SES tries to find a
-- TXT record that uses the selector that you specified. The value of the
-- TXT record must be a public key that\'s paired with the private key that
-- you specified in the process of creating the identity
--
-- /See:/ 'newDkimAttributes' smart constructor.
data DkimAttributes = DkimAttributes'
  { -- | Describes whether or not Amazon SES has successfully located the DKIM
    -- records in the DNS records for the domain. The status can be one of the
    -- following:
    --
    -- -   @PENDING@ – The verification process was initiated, but Amazon SES
    --     hasn\'t yet detected the DKIM records in the DNS configuration for
    --     the domain.
    --
    -- -   @SUCCESS@ – The verification process completed successfully.
    --
    -- -   @FAILED@ – The verification process failed. This typically occurs
    --     when Amazon SES fails to find the DKIM records in the DNS
    --     configuration of the domain.
    --
    -- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
    --     from determining the DKIM authentication status of the domain.
    --
    -- -   @NOT_STARTED@ – The DKIM verification process hasn\'t been initiated
    --     for the domain.
    status :: Prelude.Maybe DkimStatus,
    -- | If you used
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
    -- to configure DKIM authentication for the domain, then this object
    -- contains a set of unique strings that you use to create a set of CNAME
    -- records that you add to the DNS configuration for your domain. When
    -- Amazon SES detects these records in the DNS configuration for your
    -- domain, the DKIM authentication process is complete.
    --
    -- If you configured DKIM authentication for the domain by providing your
    -- own public-private key pair, then this object contains the selector for
    -- the public key.
    --
    -- Regardless of the DKIM authentication method you use, Amazon SES
    -- searches for the appropriate records in the DNS configuration of the
    -- domain for up to 72 hours.
    tokens :: Prelude.Maybe [Prelude.Text],
    -- | A string that indicates how DKIM was configured for the identity. There
    -- are two possible values:
    --
    -- -   @AWS_SES@ – Indicates that DKIM was configured for the identity by
    --     using
    --     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
    --
    -- -   @EXTERNAL@ – Indicates that DKIM was configured for the identity by
    --     using Bring Your Own DKIM (BYODKIM).
    signingAttributesOrigin :: Prelude.Maybe DkimSigningAttributesOrigin,
    -- | If the value is @true@, then the messages that you send from the
    -- identity are signed using DKIM. If the value is @false@, then the
    -- messages that you send from the identity aren\'t DKIM-signed.
    signingEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DkimAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dkimAttributes_status' - Describes whether or not Amazon SES has successfully located the DKIM
-- records in the DNS records for the domain. The status can be one of the
-- following:
--
-- -   @PENDING@ – The verification process was initiated, but Amazon SES
--     hasn\'t yet detected the DKIM records in the DNS configuration for
--     the domain.
--
-- -   @SUCCESS@ – The verification process completed successfully.
--
-- -   @FAILED@ – The verification process failed. This typically occurs
--     when Amazon SES fails to find the DKIM records in the DNS
--     configuration of the domain.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
--     from determining the DKIM authentication status of the domain.
--
-- -   @NOT_STARTED@ – The DKIM verification process hasn\'t been initiated
--     for the domain.
--
-- 'tokens', 'dkimAttributes_tokens' - If you used
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
-- to configure DKIM authentication for the domain, then this object
-- contains a set of unique strings that you use to create a set of CNAME
-- records that you add to the DNS configuration for your domain. When
-- Amazon SES detects these records in the DNS configuration for your
-- domain, the DKIM authentication process is complete.
--
-- If you configured DKIM authentication for the domain by providing your
-- own public-private key pair, then this object contains the selector for
-- the public key.
--
-- Regardless of the DKIM authentication method you use, Amazon SES
-- searches for the appropriate records in the DNS configuration of the
-- domain for up to 72 hours.
--
-- 'signingAttributesOrigin', 'dkimAttributes_signingAttributesOrigin' - A string that indicates how DKIM was configured for the identity. There
-- are two possible values:
--
-- -   @AWS_SES@ – Indicates that DKIM was configured for the identity by
--     using
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- -   @EXTERNAL@ – Indicates that DKIM was configured for the identity by
--     using Bring Your Own DKIM (BYODKIM).
--
-- 'signingEnabled', 'dkimAttributes_signingEnabled' - If the value is @true@, then the messages that you send from the
-- identity are signed using DKIM. If the value is @false@, then the
-- messages that you send from the identity aren\'t DKIM-signed.
newDkimAttributes ::
  DkimAttributes
newDkimAttributes =
  DkimAttributes'
    { status = Prelude.Nothing,
      tokens = Prelude.Nothing,
      signingAttributesOrigin = Prelude.Nothing,
      signingEnabled = Prelude.Nothing
    }

-- | Describes whether or not Amazon SES has successfully located the DKIM
-- records in the DNS records for the domain. The status can be one of the
-- following:
--
-- -   @PENDING@ – The verification process was initiated, but Amazon SES
--     hasn\'t yet detected the DKIM records in the DNS configuration for
--     the domain.
--
-- -   @SUCCESS@ – The verification process completed successfully.
--
-- -   @FAILED@ – The verification process failed. This typically occurs
--     when Amazon SES fails to find the DKIM records in the DNS
--     configuration of the domain.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
--     from determining the DKIM authentication status of the domain.
--
-- -   @NOT_STARTED@ – The DKIM verification process hasn\'t been initiated
--     for the domain.
dkimAttributes_status :: Lens.Lens' DkimAttributes (Prelude.Maybe DkimStatus)
dkimAttributes_status = Lens.lens (\DkimAttributes' {status} -> status) (\s@DkimAttributes' {} a -> s {status = a} :: DkimAttributes)

-- | If you used
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
-- to configure DKIM authentication for the domain, then this object
-- contains a set of unique strings that you use to create a set of CNAME
-- records that you add to the DNS configuration for your domain. When
-- Amazon SES detects these records in the DNS configuration for your
-- domain, the DKIM authentication process is complete.
--
-- If you configured DKIM authentication for the domain by providing your
-- own public-private key pair, then this object contains the selector for
-- the public key.
--
-- Regardless of the DKIM authentication method you use, Amazon SES
-- searches for the appropriate records in the DNS configuration of the
-- domain for up to 72 hours.
dkimAttributes_tokens :: Lens.Lens' DkimAttributes (Prelude.Maybe [Prelude.Text])
dkimAttributes_tokens = Lens.lens (\DkimAttributes' {tokens} -> tokens) (\s@DkimAttributes' {} a -> s {tokens = a} :: DkimAttributes) Prelude.. Lens.mapping Lens._Coerce

-- | A string that indicates how DKIM was configured for the identity. There
-- are two possible values:
--
-- -   @AWS_SES@ – Indicates that DKIM was configured for the identity by
--     using
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- -   @EXTERNAL@ – Indicates that DKIM was configured for the identity by
--     using Bring Your Own DKIM (BYODKIM).
dkimAttributes_signingAttributesOrigin :: Lens.Lens' DkimAttributes (Prelude.Maybe DkimSigningAttributesOrigin)
dkimAttributes_signingAttributesOrigin = Lens.lens (\DkimAttributes' {signingAttributesOrigin} -> signingAttributesOrigin) (\s@DkimAttributes' {} a -> s {signingAttributesOrigin = a} :: DkimAttributes)

-- | If the value is @true@, then the messages that you send from the
-- identity are signed using DKIM. If the value is @false@, then the
-- messages that you send from the identity aren\'t DKIM-signed.
dkimAttributes_signingEnabled :: Lens.Lens' DkimAttributes (Prelude.Maybe Prelude.Bool)
dkimAttributes_signingEnabled = Lens.lens (\DkimAttributes' {signingEnabled} -> signingEnabled) (\s@DkimAttributes' {} a -> s {signingEnabled = a} :: DkimAttributes)

instance Core.FromJSON DkimAttributes where
  parseJSON =
    Core.withObject
      "DkimAttributes"
      ( \x ->
          DkimAttributes'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Tokens" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "SigningAttributesOrigin")
            Prelude.<*> (x Core..:? "SigningEnabled")
      )

instance Prelude.Hashable DkimAttributes

instance Prelude.NFData DkimAttributes

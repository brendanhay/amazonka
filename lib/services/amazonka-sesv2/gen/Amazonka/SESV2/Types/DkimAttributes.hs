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
-- Module      : Amazonka.SESV2.Types.DkimAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DkimAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.DkimSigningAttributesOrigin
import Amazonka.SESV2.Types.DkimSigningKeyLength
import Amazonka.SESV2.Types.DkimStatus

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
  { -- | A string that indicates how DKIM was configured for the identity. These
    -- are the possible values:
    --
    -- -   @AWS_SES@ – Indicates that DKIM was configured for the identity by
    --     using
    --     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
    --
    -- -   @EXTERNAL@ – Indicates that DKIM was configured for the identity by
    --     using Bring Your Own DKIM (BYODKIM).
    signingAttributesOrigin :: Prelude.Maybe DkimSigningAttributesOrigin,
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
    -- | [Easy DKIM] The key length of the DKIM key pair in use.
    currentSigningKeyLength :: Prelude.Maybe DkimSigningKeyLength,
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
    status :: Prelude.Maybe DkimStatus,
    -- | If the value is @true@, then the messages that you send from the
    -- identity are signed using DKIM. If the value is @false@, then the
    -- messages that you send from the identity aren\'t DKIM-signed.
    signingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | [Easy DKIM] The last time a key pair was generated for this identity.
    lastKeyGenerationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | [Easy DKIM] The key length of the future DKIM key pair to be generated.
    -- This can be changed at most once per day.
    nextSigningKeyLength :: Prelude.Maybe DkimSigningKeyLength
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
-- 'signingAttributesOrigin', 'dkimAttributes_signingAttributesOrigin' - A string that indicates how DKIM was configured for the identity. These
-- are the possible values:
--
-- -   @AWS_SES@ – Indicates that DKIM was configured for the identity by
--     using
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- -   @EXTERNAL@ – Indicates that DKIM was configured for the identity by
--     using Bring Your Own DKIM (BYODKIM).
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
-- 'currentSigningKeyLength', 'dkimAttributes_currentSigningKeyLength' - [Easy DKIM] The key length of the DKIM key pair in use.
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
-- 'signingEnabled', 'dkimAttributes_signingEnabled' - If the value is @true@, then the messages that you send from the
-- identity are signed using DKIM. If the value is @false@, then the
-- messages that you send from the identity aren\'t DKIM-signed.
--
-- 'lastKeyGenerationTimestamp', 'dkimAttributes_lastKeyGenerationTimestamp' - [Easy DKIM] The last time a key pair was generated for this identity.
--
-- 'nextSigningKeyLength', 'dkimAttributes_nextSigningKeyLength' - [Easy DKIM] The key length of the future DKIM key pair to be generated.
-- This can be changed at most once per day.
newDkimAttributes ::
  DkimAttributes
newDkimAttributes =
  DkimAttributes'
    { signingAttributesOrigin =
        Prelude.Nothing,
      tokens = Prelude.Nothing,
      currentSigningKeyLength = Prelude.Nothing,
      status = Prelude.Nothing,
      signingEnabled = Prelude.Nothing,
      lastKeyGenerationTimestamp = Prelude.Nothing,
      nextSigningKeyLength = Prelude.Nothing
    }

-- | A string that indicates how DKIM was configured for the identity. These
-- are the possible values:
--
-- -   @AWS_SES@ – Indicates that DKIM was configured for the identity by
--     using
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- -   @EXTERNAL@ – Indicates that DKIM was configured for the identity by
--     using Bring Your Own DKIM (BYODKIM).
dkimAttributes_signingAttributesOrigin :: Lens.Lens' DkimAttributes (Prelude.Maybe DkimSigningAttributesOrigin)
dkimAttributes_signingAttributesOrigin = Lens.lens (\DkimAttributes' {signingAttributesOrigin} -> signingAttributesOrigin) (\s@DkimAttributes' {} a -> s {signingAttributesOrigin = a} :: DkimAttributes)

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
dkimAttributes_tokens = Lens.lens (\DkimAttributes' {tokens} -> tokens) (\s@DkimAttributes' {} a -> s {tokens = a} :: DkimAttributes) Prelude.. Lens.mapping Lens.coerced

-- | [Easy DKIM] The key length of the DKIM key pair in use.
dkimAttributes_currentSigningKeyLength :: Lens.Lens' DkimAttributes (Prelude.Maybe DkimSigningKeyLength)
dkimAttributes_currentSigningKeyLength = Lens.lens (\DkimAttributes' {currentSigningKeyLength} -> currentSigningKeyLength) (\s@DkimAttributes' {} a -> s {currentSigningKeyLength = a} :: DkimAttributes)

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

-- | If the value is @true@, then the messages that you send from the
-- identity are signed using DKIM. If the value is @false@, then the
-- messages that you send from the identity aren\'t DKIM-signed.
dkimAttributes_signingEnabled :: Lens.Lens' DkimAttributes (Prelude.Maybe Prelude.Bool)
dkimAttributes_signingEnabled = Lens.lens (\DkimAttributes' {signingEnabled} -> signingEnabled) (\s@DkimAttributes' {} a -> s {signingEnabled = a} :: DkimAttributes)

-- | [Easy DKIM] The last time a key pair was generated for this identity.
dkimAttributes_lastKeyGenerationTimestamp :: Lens.Lens' DkimAttributes (Prelude.Maybe Prelude.UTCTime)
dkimAttributes_lastKeyGenerationTimestamp = Lens.lens (\DkimAttributes' {lastKeyGenerationTimestamp} -> lastKeyGenerationTimestamp) (\s@DkimAttributes' {} a -> s {lastKeyGenerationTimestamp = a} :: DkimAttributes) Prelude.. Lens.mapping Data._Time

-- | [Easy DKIM] The key length of the future DKIM key pair to be generated.
-- This can be changed at most once per day.
dkimAttributes_nextSigningKeyLength :: Lens.Lens' DkimAttributes (Prelude.Maybe DkimSigningKeyLength)
dkimAttributes_nextSigningKeyLength = Lens.lens (\DkimAttributes' {nextSigningKeyLength} -> nextSigningKeyLength) (\s@DkimAttributes' {} a -> s {nextSigningKeyLength = a} :: DkimAttributes)

instance Data.FromJSON DkimAttributes where
  parseJSON =
    Data.withObject
      "DkimAttributes"
      ( \x ->
          DkimAttributes'
            Prelude.<$> (x Data..:? "SigningAttributesOrigin")
            Prelude.<*> (x Data..:? "Tokens" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CurrentSigningKeyLength")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "SigningEnabled")
            Prelude.<*> (x Data..:? "LastKeyGenerationTimestamp")
            Prelude.<*> (x Data..:? "NextSigningKeyLength")
      )

instance Prelude.Hashable DkimAttributes where
  hashWithSalt _salt DkimAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` signingAttributesOrigin
      `Prelude.hashWithSalt` tokens
      `Prelude.hashWithSalt` currentSigningKeyLength
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` signingEnabled
      `Prelude.hashWithSalt` lastKeyGenerationTimestamp
      `Prelude.hashWithSalt` nextSigningKeyLength

instance Prelude.NFData DkimAttributes where
  rnf DkimAttributes' {..} =
    Prelude.rnf signingAttributesOrigin
      `Prelude.seq` Prelude.rnf tokens
      `Prelude.seq` Prelude.rnf currentSigningKeyLength
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf signingEnabled
      `Prelude.seq` Prelude.rnf lastKeyGenerationTimestamp
      `Prelude.seq` Prelude.rnf nextSigningKeyLength

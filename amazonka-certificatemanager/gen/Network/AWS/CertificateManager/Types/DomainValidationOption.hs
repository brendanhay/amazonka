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
-- Module      : Network.AWS.CertificateManager.Types.DomainValidationOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.DomainValidationOption where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the domain names that you want ACM to use to
-- send you emails that enable you to validate domain ownership.
--
-- /See:/ 'newDomainValidationOption' smart constructor.
data DomainValidationOption = DomainValidationOption'
  { -- | A fully qualified domain name (FQDN) in the certificate request.
    domainName :: Prelude.Text,
    -- | The domain name that you want ACM to use to send you validation emails.
    -- This domain name is the suffix of the email addresses that you want ACM
    -- to use. This must be the same as the @DomainName@ value or a superdomain
    -- of the @DomainName@ value. For example, if you request a certificate for
    -- @testing.example.com@, you can specify @example.com@ for this value. In
    -- that case, ACM sends domain validation emails to the following five
    -- addresses:
    --
    -- -   admin\@example.com
    --
    -- -   administrator\@example.com
    --
    -- -   hostmaster\@example.com
    --
    -- -   postmaster\@example.com
    --
    -- -   webmaster\@example.com
    validationDomain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DomainValidationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'domainValidationOption_domainName' - A fully qualified domain name (FQDN) in the certificate request.
--
-- 'validationDomain', 'domainValidationOption_validationDomain' - The domain name that you want ACM to use to send you validation emails.
-- This domain name is the suffix of the email addresses that you want ACM
-- to use. This must be the same as the @DomainName@ value or a superdomain
-- of the @DomainName@ value. For example, if you request a certificate for
-- @testing.example.com@, you can specify @example.com@ for this value. In
-- that case, ACM sends domain validation emails to the following five
-- addresses:
--
-- -   admin\@example.com
--
-- -   administrator\@example.com
--
-- -   hostmaster\@example.com
--
-- -   postmaster\@example.com
--
-- -   webmaster\@example.com
newDomainValidationOption ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'validationDomain'
  Prelude.Text ->
  DomainValidationOption
newDomainValidationOption
  pDomainName_
  pValidationDomain_ =
    DomainValidationOption'
      { domainName = pDomainName_,
        validationDomain = pValidationDomain_
      }

-- | A fully qualified domain name (FQDN) in the certificate request.
domainValidationOption_domainName :: Lens.Lens' DomainValidationOption Prelude.Text
domainValidationOption_domainName = Lens.lens (\DomainValidationOption' {domainName} -> domainName) (\s@DomainValidationOption' {} a -> s {domainName = a} :: DomainValidationOption)

-- | The domain name that you want ACM to use to send you validation emails.
-- This domain name is the suffix of the email addresses that you want ACM
-- to use. This must be the same as the @DomainName@ value or a superdomain
-- of the @DomainName@ value. For example, if you request a certificate for
-- @testing.example.com@, you can specify @example.com@ for this value. In
-- that case, ACM sends domain validation emails to the following five
-- addresses:
--
-- -   admin\@example.com
--
-- -   administrator\@example.com
--
-- -   hostmaster\@example.com
--
-- -   postmaster\@example.com
--
-- -   webmaster\@example.com
domainValidationOption_validationDomain :: Lens.Lens' DomainValidationOption Prelude.Text
domainValidationOption_validationDomain = Lens.lens (\DomainValidationOption' {validationDomain} -> validationDomain) (\s@DomainValidationOption' {} a -> s {validationDomain = a} :: DomainValidationOption)

instance Prelude.Hashable DomainValidationOption

instance Prelude.NFData DomainValidationOption

instance Prelude.ToJSON DomainValidationOption where
  toJSON DomainValidationOption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Prelude..= domainName),
            Prelude.Just
              ("ValidationDomain" Prelude..= validationDomain)
          ]
      )

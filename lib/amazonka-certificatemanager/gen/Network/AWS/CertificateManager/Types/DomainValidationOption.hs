{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.DomainValidationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.DomainValidationOption where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the domain names that you want ACM to use to send you emails that enable you to validate domain ownership.
--
--
--
-- /See:/ 'domainValidationOption' smart constructor.
data DomainValidationOption = DomainValidationOption'
  { _dvoDomainName ::
      !Text,
    _dvoValidationDomain :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainValidationOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvoDomainName' - A fully qualified domain name (FQDN) in the certificate request.
--
-- * 'dvoValidationDomain' - The domain name that you want ACM to use to send you validation emails. This domain name is the suffix of the email addresses that you want ACM to use. This must be the same as the @DomainName@ value or a superdomain of the @DomainName@ value. For example, if you request a certificate for @testing.example.com@ , you can specify @example.com@ for this value. In that case, ACM sends domain validation emails to the following five addresses:     * admin@example.com     * administrator@example.com     * hostmaster@example.com     * postmaster@example.com     * webmaster@example.com
domainValidationOption ::
  -- | 'dvoDomainName'
  Text ->
  -- | 'dvoValidationDomain'
  Text ->
  DomainValidationOption
domainValidationOption pDomainName_ pValidationDomain_ =
  DomainValidationOption'
    { _dvoDomainName = pDomainName_,
      _dvoValidationDomain = pValidationDomain_
    }

-- | A fully qualified domain name (FQDN) in the certificate request.
dvoDomainName :: Lens' DomainValidationOption Text
dvoDomainName = lens _dvoDomainName (\s a -> s {_dvoDomainName = a})

-- | The domain name that you want ACM to use to send you validation emails. This domain name is the suffix of the email addresses that you want ACM to use. This must be the same as the @DomainName@ value or a superdomain of the @DomainName@ value. For example, if you request a certificate for @testing.example.com@ , you can specify @example.com@ for this value. In that case, ACM sends domain validation emails to the following five addresses:     * admin@example.com     * administrator@example.com     * hostmaster@example.com     * postmaster@example.com     * webmaster@example.com
dvoValidationDomain :: Lens' DomainValidationOption Text
dvoValidationDomain = lens _dvoValidationDomain (\s a -> s {_dvoValidationDomain = a})

instance Hashable DomainValidationOption

instance NFData DomainValidationOption

instance ToJSON DomainValidationOption where
  toJSON DomainValidationOption' {..} =
    object
      ( catMaybes
          [ Just ("DomainName" .= _dvoDomainName),
            Just ("ValidationDomain" .= _dvoValidationDomain)
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.DomainValidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.DomainValidation where

import Network.AWS.CertificateManager.Types.DomainStatus
import Network.AWS.CertificateManager.Types.ResourceRecord
import Network.AWS.CertificateManager.Types.ValidationMethod
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the validation of each domain name in the certificate.
--
--
--
-- /See:/ 'domainValidation' smart constructor.
data DomainValidation = DomainValidation'
  { _dvValidationEmails ::
      !(Maybe [Text]),
    _dvValidationMethod :: !(Maybe ValidationMethod),
    _dvResourceRecord :: !(Maybe ResourceRecord),
    _dvValidationStatus :: !(Maybe DomainStatus),
    _dvValidationDomain :: !(Maybe Text),
    _dvDomainName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainValidation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvValidationEmails' - A list of email addresses that ACM used to send domain validation emails.
--
-- * 'dvValidationMethod' - Specifies the domain validation method.
--
-- * 'dvResourceRecord' - Contains the CNAME record that you add to your DNS database for domain validation. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership> . Note: The CNAME information that you need does not include the name of your domain. If you include  your domain name in the DNS database CNAME record, validation fails.  For example, if the name is "_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com", only "_a79865eb4cd1a6ab990a45779b4e0b96" must be used.
--
-- * 'dvValidationStatus' - The validation status of the domain name. This can be one of the following values:     * @PENDING_VALIDATION@      * SUCCESS     * FAILED
--
-- * 'dvValidationDomain' - The domain name that ACM used to send domain validation emails.
--
-- * 'dvDomainName' - A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ .
domainValidation ::
  -- | 'dvDomainName'
  Text ->
  DomainValidation
domainValidation pDomainName_ =
  DomainValidation'
    { _dvValidationEmails = Nothing,
      _dvValidationMethod = Nothing,
      _dvResourceRecord = Nothing,
      _dvValidationStatus = Nothing,
      _dvValidationDomain = Nothing,
      _dvDomainName = pDomainName_
    }

-- | A list of email addresses that ACM used to send domain validation emails.
dvValidationEmails :: Lens' DomainValidation [Text]
dvValidationEmails = lens _dvValidationEmails (\s a -> s {_dvValidationEmails = a}) . _Default . _Coerce

-- | Specifies the domain validation method.
dvValidationMethod :: Lens' DomainValidation (Maybe ValidationMethod)
dvValidationMethod = lens _dvValidationMethod (\s a -> s {_dvValidationMethod = a})

-- | Contains the CNAME record that you add to your DNS database for domain validation. For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership> . Note: The CNAME information that you need does not include the name of your domain. If you include  your domain name in the DNS database CNAME record, validation fails.  For example, if the name is "_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com", only "_a79865eb4cd1a6ab990a45779b4e0b96" must be used.
dvResourceRecord :: Lens' DomainValidation (Maybe ResourceRecord)
dvResourceRecord = lens _dvResourceRecord (\s a -> s {_dvResourceRecord = a})

-- | The validation status of the domain name. This can be one of the following values:     * @PENDING_VALIDATION@      * SUCCESS     * FAILED
dvValidationStatus :: Lens' DomainValidation (Maybe DomainStatus)
dvValidationStatus = lens _dvValidationStatus (\s a -> s {_dvValidationStatus = a})

-- | The domain name that ACM used to send domain validation emails.
dvValidationDomain :: Lens' DomainValidation (Maybe Text)
dvValidationDomain = lens _dvValidationDomain (\s a -> s {_dvValidationDomain = a})

-- | A fully qualified domain name (FQDN) in the certificate. For example, @www.example.com@ or @example.com@ .
dvDomainName :: Lens' DomainValidation Text
dvDomainName = lens _dvDomainName (\s a -> s {_dvDomainName = a})

instance FromJSON DomainValidation where
  parseJSON =
    withObject
      "DomainValidation"
      ( \x ->
          DomainValidation'
            <$> (x .:? "ValidationEmails" .!= mempty)
            <*> (x .:? "ValidationMethod")
            <*> (x .:? "ResourceRecord")
            <*> (x .:? "ValidationStatus")
            <*> (x .:? "ValidationDomain")
            <*> (x .: "DomainName")
      )

instance Hashable DomainValidation

instance NFData DomainValidation

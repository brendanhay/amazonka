{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.Sum where

import Network.AWS.Prelude

data CertificateStatus
  = CSExpired
  | CSFailed
  | CSInactive
  | CSIssued
  | CSPendingValidation
  | CSRevoked
  | CSValidationTimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CertificateStatus where
    parser = takeLowerText >>= \case
        "expired" -> pure CSExpired
        "failed" -> pure CSFailed
        "inactive" -> pure CSInactive
        "issued" -> pure CSIssued
        "pending_validation" -> pure CSPendingValidation
        "revoked" -> pure CSRevoked
        "validation_timed_out" -> pure CSValidationTimedOut
        e -> fromTextError $ "Failure parsing CertificateStatus from value: '" <> e
           <> "'. Accepted values: expired, failed, inactive, issued, pending_validation, revoked, validation_timed_out"

instance ToText CertificateStatus where
    toText = \case
        CSExpired -> "EXPIRED"
        CSFailed -> "FAILED"
        CSInactive -> "INACTIVE"
        CSIssued -> "ISSUED"
        CSPendingValidation -> "PENDING_VALIDATION"
        CSRevoked -> "REVOKED"
        CSValidationTimedOut -> "VALIDATION_TIMED_OUT"

instance Hashable     CertificateStatus
instance NFData       CertificateStatus
instance ToByteString CertificateStatus
instance ToQuery      CertificateStatus
instance ToHeader     CertificateStatus

instance ToJSON CertificateStatus where
    toJSON = toJSONText

instance FromJSON CertificateStatus where
    parseJSON = parseJSONText "CertificateStatus"

data CertificateTransparencyLoggingPreference
  = Disabled
  | Enabled
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CertificateTransparencyLoggingPreference where
    parser = takeLowerText >>= \case
        "disabled" -> pure Disabled
        "enabled" -> pure Enabled
        e -> fromTextError $ "Failure parsing CertificateTransparencyLoggingPreference from value: '" <> e
           <> "'. Accepted values: disabled, enabled"

instance ToText CertificateTransparencyLoggingPreference where
    toText = \case
        Disabled -> "DISABLED"
        Enabled -> "ENABLED"

instance Hashable     CertificateTransparencyLoggingPreference
instance NFData       CertificateTransparencyLoggingPreference
instance ToByteString CertificateTransparencyLoggingPreference
instance ToQuery      CertificateTransparencyLoggingPreference
instance ToHeader     CertificateTransparencyLoggingPreference

instance ToJSON CertificateTransparencyLoggingPreference where
    toJSON = toJSONText

instance FromJSON CertificateTransparencyLoggingPreference where
    parseJSON = parseJSONText "CertificateTransparencyLoggingPreference"

data CertificateType
  = AmazonIssued
  | Imported
  | Private
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CertificateType where
    parser = takeLowerText >>= \case
        "amazon_issued" -> pure AmazonIssued
        "imported" -> pure Imported
        "private" -> pure Private
        e -> fromTextError $ "Failure parsing CertificateType from value: '" <> e
           <> "'. Accepted values: amazon_issued, imported, private"

instance ToText CertificateType where
    toText = \case
        AmazonIssued -> "AMAZON_ISSUED"
        Imported -> "IMPORTED"
        Private -> "PRIVATE"

instance Hashable     CertificateType
instance NFData       CertificateType
instance ToByteString CertificateType
instance ToQuery      CertificateType
instance ToHeader     CertificateType

instance FromJSON CertificateType where
    parseJSON = parseJSONText "CertificateType"

data DomainStatus
  = Failed
  | PendingValidation
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DomainStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure Failed
        "pending_validation" -> pure PendingValidation
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing DomainStatus from value: '" <> e
           <> "'. Accepted values: failed, pending_validation, success"

instance ToText DomainStatus where
    toText = \case
        Failed -> "FAILED"
        PendingValidation -> "PENDING_VALIDATION"
        Success -> "SUCCESS"

instance Hashable     DomainStatus
instance NFData       DomainStatus
instance ToByteString DomainStatus
instance ToQuery      DomainStatus
instance ToHeader     DomainStatus

instance FromJSON DomainStatus where
    parseJSON = parseJSONText "DomainStatus"

data ExtendedKeyUsageName
  = Any
  | CodeSigning
  | Custom
  | EmailProtection
  | IPsecEndSystem
  | IPsecTunnel
  | IPsecUser
  | None
  | OcspSigning
  | TLSWebClientAuthentication
  | TLSWebServerAuthentication
  | TimeStamping
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExtendedKeyUsageName where
    parser = takeLowerText >>= \case
        "any" -> pure Any
        "code_signing" -> pure CodeSigning
        "custom" -> pure Custom
        "email_protection" -> pure EmailProtection
        "ipsec_end_system" -> pure IPsecEndSystem
        "ipsec_tunnel" -> pure IPsecTunnel
        "ipsec_user" -> pure IPsecUser
        "none" -> pure None
        "ocsp_signing" -> pure OcspSigning
        "tls_web_client_authentication" -> pure TLSWebClientAuthentication
        "tls_web_server_authentication" -> pure TLSWebServerAuthentication
        "time_stamping" -> pure TimeStamping
        e -> fromTextError $ "Failure parsing ExtendedKeyUsageName from value: '" <> e
           <> "'. Accepted values: any, code_signing, custom, email_protection, ipsec_end_system, ipsec_tunnel, ipsec_user, none, ocsp_signing, tls_web_client_authentication, tls_web_server_authentication, time_stamping"

instance ToText ExtendedKeyUsageName where
    toText = \case
        Any -> "ANY"
        CodeSigning -> "CODE_SIGNING"
        Custom -> "CUSTOM"
        EmailProtection -> "EMAIL_PROTECTION"
        IPsecEndSystem -> "IPSEC_END_SYSTEM"
        IPsecTunnel -> "IPSEC_TUNNEL"
        IPsecUser -> "IPSEC_USER"
        None -> "NONE"
        OcspSigning -> "OCSP_SIGNING"
        TLSWebClientAuthentication -> "TLS_WEB_CLIENT_AUTHENTICATION"
        TLSWebServerAuthentication -> "TLS_WEB_SERVER_AUTHENTICATION"
        TimeStamping -> "TIME_STAMPING"

instance Hashable     ExtendedKeyUsageName
instance NFData       ExtendedKeyUsageName
instance ToByteString ExtendedKeyUsageName
instance ToQuery      ExtendedKeyUsageName
instance ToHeader     ExtendedKeyUsageName

instance ToJSON ExtendedKeyUsageName where
    toJSON = toJSONText

instance FromJSON ExtendedKeyUsageName where
    parseJSON = parseJSONText "ExtendedKeyUsageName"

data FailureReason
  = AdditionalVerificationRequired
  | CaaError
  | DomainNotAllowed
  | InvalidPublicDomain
  | NoAvailableContacts
  | Other
  | PcaInvalidARN
  | PcaInvalidArgs
  | PcaInvalidState
  | PcaLimitExceeded
  | PcaRequestFailed
  | PcaResourceNotFound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FailureReason where
    parser = takeLowerText >>= \case
        "additional_verification_required" -> pure AdditionalVerificationRequired
        "caa_error" -> pure CaaError
        "domain_not_allowed" -> pure DomainNotAllowed
        "invalid_public_domain" -> pure InvalidPublicDomain
        "no_available_contacts" -> pure NoAvailableContacts
        "other" -> pure Other
        "pca_invalid_arn" -> pure PcaInvalidARN
        "pca_invalid_args" -> pure PcaInvalidArgs
        "pca_invalid_state" -> pure PcaInvalidState
        "pca_limit_exceeded" -> pure PcaLimitExceeded
        "pca_request_failed" -> pure PcaRequestFailed
        "pca_resource_not_found" -> pure PcaResourceNotFound
        e -> fromTextError $ "Failure parsing FailureReason from value: '" <> e
           <> "'. Accepted values: additional_verification_required, caa_error, domain_not_allowed, invalid_public_domain, no_available_contacts, other, pca_invalid_arn, pca_invalid_args, pca_invalid_state, pca_limit_exceeded, pca_request_failed, pca_resource_not_found"

instance ToText FailureReason where
    toText = \case
        AdditionalVerificationRequired -> "ADDITIONAL_VERIFICATION_REQUIRED"
        CaaError -> "CAA_ERROR"
        DomainNotAllowed -> "DOMAIN_NOT_ALLOWED"
        InvalidPublicDomain -> "INVALID_PUBLIC_DOMAIN"
        NoAvailableContacts -> "NO_AVAILABLE_CONTACTS"
        Other -> "OTHER"
        PcaInvalidARN -> "PCA_INVALID_ARN"
        PcaInvalidArgs -> "PCA_INVALID_ARGS"
        PcaInvalidState -> "PCA_INVALID_STATE"
        PcaLimitExceeded -> "PCA_LIMIT_EXCEEDED"
        PcaRequestFailed -> "PCA_REQUEST_FAILED"
        PcaResourceNotFound -> "PCA_RESOURCE_NOT_FOUND"

instance Hashable     FailureReason
instance NFData       FailureReason
instance ToByteString FailureReason
instance ToQuery      FailureReason
instance ToHeader     FailureReason

instance FromJSON FailureReason where
    parseJSON = parseJSONText "FailureReason"

data KeyAlgorithm
  = EcPRIME256V1
  | EcSECP384R1
  | EcSECP521R1
  | Rsa1024
  | Rsa2048
  | Rsa4096
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText KeyAlgorithm where
    parser = takeLowerText >>= \case
        "ec_prime256v1" -> pure EcPRIME256V1
        "ec_secp384r1" -> pure EcSECP384R1
        "ec_secp521r1" -> pure EcSECP521R1
        "rsa_1024" -> pure Rsa1024
        "rsa_2048" -> pure Rsa2048
        "rsa_4096" -> pure Rsa4096
        e -> fromTextError $ "Failure parsing KeyAlgorithm from value: '" <> e
           <> "'. Accepted values: ec_prime256v1, ec_secp384r1, ec_secp521r1, rsa_1024, rsa_2048, rsa_4096"

instance ToText KeyAlgorithm where
    toText = \case
        EcPRIME256V1 -> "EC_prime256v1"
        EcSECP384R1 -> "EC_secp384r1"
        EcSECP521R1 -> "EC_secp521r1"
        Rsa1024 -> "RSA_1024"
        Rsa2048 -> "RSA_2048"
        Rsa4096 -> "RSA_4096"

instance Hashable     KeyAlgorithm
instance NFData       KeyAlgorithm
instance ToByteString KeyAlgorithm
instance ToQuery      KeyAlgorithm
instance ToHeader     KeyAlgorithm

instance ToJSON KeyAlgorithm where
    toJSON = toJSONText

instance FromJSON KeyAlgorithm where
    parseJSON = parseJSONText "KeyAlgorithm"

data KeyUsageName
  = KUNAny
  | KUNCertificateSigning
  | KUNCrlSigning
  | KUNCustom
  | KUNDataEncipherment
  | KUNDecipherOnly
  | KUNDigitalSignature
  | KUNEncipherOnly
  | KUNKeyAgreement
  | KUNKeyEncipherment
  | KUNNonRepudiation
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText KeyUsageName where
    parser = takeLowerText >>= \case
        "any" -> pure KUNAny
        "certificate_signing" -> pure KUNCertificateSigning
        "crl_signing" -> pure KUNCrlSigning
        "custom" -> pure KUNCustom
        "data_encipherment" -> pure KUNDataEncipherment
        "decipher_only" -> pure KUNDecipherOnly
        "digital_signature" -> pure KUNDigitalSignature
        "encipher_only" -> pure KUNEncipherOnly
        "key_agreement" -> pure KUNKeyAgreement
        "key_encipherment" -> pure KUNKeyEncipherment
        "non_repudiation" -> pure KUNNonRepudiation
        e -> fromTextError $ "Failure parsing KeyUsageName from value: '" <> e
           <> "'. Accepted values: any, certificate_signing, crl_signing, custom, data_encipherment, decipher_only, digital_signature, encipher_only, key_agreement, key_encipherment, non_repudiation"

instance ToText KeyUsageName where
    toText = \case
        KUNAny -> "ANY"
        KUNCertificateSigning -> "CERTIFICATE_SIGNING"
        KUNCrlSigning -> "CRL_SIGNING"
        KUNCustom -> "CUSTOM"
        KUNDataEncipherment -> "DATA_ENCIPHERMENT"
        KUNDecipherOnly -> "DECIPHER_ONLY"
        KUNDigitalSignature -> "DIGITAL_SIGNATURE"
        KUNEncipherOnly -> "ENCIPHER_ONLY"
        KUNKeyAgreement -> "KEY_AGREEMENT"
        KUNKeyEncipherment -> "KEY_ENCIPHERMENT"
        KUNNonRepudiation -> "NON_REPUDIATION"

instance Hashable     KeyUsageName
instance NFData       KeyUsageName
instance ToByteString KeyUsageName
instance ToQuery      KeyUsageName
instance ToHeader     KeyUsageName

instance ToJSON KeyUsageName where
    toJSON = toJSONText

instance FromJSON KeyUsageName where
    parseJSON = parseJSONText "KeyUsageName"

data RecordType =
  Cname
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RecordType where
    parser = takeLowerText >>= \case
        "cname" -> pure Cname
        e -> fromTextError $ "Failure parsing RecordType from value: '" <> e
           <> "'. Accepted values: cname"

instance ToText RecordType where
    toText = \case
        Cname -> "CNAME"

instance Hashable     RecordType
instance NFData       RecordType
instance ToByteString RecordType
instance ToQuery      RecordType
instance ToHeader     RecordType

instance FromJSON RecordType where
    parseJSON = parseJSONText "RecordType"

data RenewalEligibility
  = Eligible
  | Ineligible
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RenewalEligibility where
    parser = takeLowerText >>= \case
        "eligible" -> pure Eligible
        "ineligible" -> pure Ineligible
        e -> fromTextError $ "Failure parsing RenewalEligibility from value: '" <> e
           <> "'. Accepted values: eligible, ineligible"

instance ToText RenewalEligibility where
    toText = \case
        Eligible -> "ELIGIBLE"
        Ineligible -> "INELIGIBLE"

instance Hashable     RenewalEligibility
instance NFData       RenewalEligibility
instance ToByteString RenewalEligibility
instance ToQuery      RenewalEligibility
instance ToHeader     RenewalEligibility

instance FromJSON RenewalEligibility where
    parseJSON = parseJSONText "RenewalEligibility"

data RenewalStatus
  = RSFailed
  | RSPendingAutoRenewal
  | RSPendingValidation
  | RSSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RenewalStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure RSFailed
        "pending_auto_renewal" -> pure RSPendingAutoRenewal
        "pending_validation" -> pure RSPendingValidation
        "success" -> pure RSSuccess
        e -> fromTextError $ "Failure parsing RenewalStatus from value: '" <> e
           <> "'. Accepted values: failed, pending_auto_renewal, pending_validation, success"

instance ToText RenewalStatus where
    toText = \case
        RSFailed -> "FAILED"
        RSPendingAutoRenewal -> "PENDING_AUTO_RENEWAL"
        RSPendingValidation -> "PENDING_VALIDATION"
        RSSuccess -> "SUCCESS"

instance Hashable     RenewalStatus
instance NFData       RenewalStatus
instance ToByteString RenewalStatus
instance ToQuery      RenewalStatus
instance ToHeader     RenewalStatus

instance FromJSON RenewalStatus where
    parseJSON = parseJSONText "RenewalStatus"

data RevocationReason
  = AACompromise
  | AffiliationChanged
  | CaCompromise
  | CertificateHold
  | CessationOfOperation
  | KeyCompromise
  | PrivilegeWithdrawn
  | RemoveFromCrl
  | Superceded
  | Unspecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RevocationReason where
    parser = takeLowerText >>= \case
        "a_a_compromise" -> pure AACompromise
        "affiliation_changed" -> pure AffiliationChanged
        "ca_compromise" -> pure CaCompromise
        "certificate_hold" -> pure CertificateHold
        "cessation_of_operation" -> pure CessationOfOperation
        "key_compromise" -> pure KeyCompromise
        "privilege_withdrawn" -> pure PrivilegeWithdrawn
        "remove_from_crl" -> pure RemoveFromCrl
        "superceded" -> pure Superceded
        "unspecified" -> pure Unspecified
        e -> fromTextError $ "Failure parsing RevocationReason from value: '" <> e
           <> "'. Accepted values: a_a_compromise, affiliation_changed, ca_compromise, certificate_hold, cessation_of_operation, key_compromise, privilege_withdrawn, remove_from_crl, superceded, unspecified"

instance ToText RevocationReason where
    toText = \case
        AACompromise -> "A_A_COMPROMISE"
        AffiliationChanged -> "AFFILIATION_CHANGED"
        CaCompromise -> "CA_COMPROMISE"
        CertificateHold -> "CERTIFICATE_HOLD"
        CessationOfOperation -> "CESSATION_OF_OPERATION"
        KeyCompromise -> "KEY_COMPROMISE"
        PrivilegeWithdrawn -> "PRIVILEGE_WITHDRAWN"
        RemoveFromCrl -> "REMOVE_FROM_CRL"
        Superceded -> "SUPERCEDED"
        Unspecified -> "UNSPECIFIED"

instance Hashable     RevocationReason
instance NFData       RevocationReason
instance ToByteString RevocationReason
instance ToQuery      RevocationReason
instance ToHeader     RevocationReason

instance FromJSON RevocationReason where
    parseJSON = parseJSONText "RevocationReason"

data ValidationMethod
  = DNS
  | Email
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ValidationMethod where
    parser = takeLowerText >>= \case
        "dns" -> pure DNS
        "email" -> pure Email
        e -> fromTextError $ "Failure parsing ValidationMethod from value: '" <> e
           <> "'. Accepted values: dns, email"

instance ToText ValidationMethod where
    toText = \case
        DNS -> "DNS"
        Email -> "EMAIL"

instance Hashable     ValidationMethod
instance NFData       ValidationMethod
instance ToByteString ValidationMethod
instance ToQuery      ValidationMethod
instance ToHeader     ValidationMethod

instance ToJSON ValidationMethod where
    toJSON = toJSONText

instance FromJSON ValidationMethod where
    parseJSON = parseJSONText "ValidationMethod"

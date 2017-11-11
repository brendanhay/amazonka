{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Sum
-- Copyright   : (c) 2013-2017 Brendan Hay
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

data CertificateType
  = AmazonIssued
  | Imported
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CertificateType where
    parser = takeLowerText >>= \case
        "amazon_issued" -> pure AmazonIssued
        "imported" -> pure Imported
        e -> fromTextError $ "Failure parsing CertificateType from value: '" <> e
           <> "'. Accepted values: amazon_issued, imported"

instance ToText CertificateType where
    toText = \case
        AmazonIssued -> "AMAZON_ISSUED"
        Imported -> "IMPORTED"

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

data FailureReason
  = AdditionalVerificationRequired
  | DomainNotAllowed
  | InvalidPublicDomain
  | NoAvailableContacts
  | Other
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FailureReason where
    parser = takeLowerText >>= \case
        "additional_verification_required" -> pure AdditionalVerificationRequired
        "domain_not_allowed" -> pure DomainNotAllowed
        "invalid_public_domain" -> pure InvalidPublicDomain
        "no_available_contacts" -> pure NoAvailableContacts
        "other" -> pure Other
        e -> fromTextError $ "Failure parsing FailureReason from value: '" <> e
           <> "'. Accepted values: additional_verification_required, domain_not_allowed, invalid_public_domain, no_available_contacts, other"

instance ToText FailureReason where
    toText = \case
        AdditionalVerificationRequired -> "ADDITIONAL_VERIFICATION_REQUIRED"
        DomainNotAllowed -> "DOMAIN_NOT_ALLOWED"
        InvalidPublicDomain -> "INVALID_PUBLIC_DOMAIN"
        NoAvailableContacts -> "NO_AVAILABLE_CONTACTS"
        Other -> "OTHER"

instance Hashable     FailureReason
instance NFData       FailureReason
instance ToByteString FailureReason
instance ToQuery      FailureReason
instance ToHeader     FailureReason

instance FromJSON FailureReason where
    parseJSON = parseJSONText "FailureReason"

data KeyAlgorithm
  = EcPRIME256V1
  | Rsa1024
  | Rsa2048
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText KeyAlgorithm where
    parser = takeLowerText >>= \case
        "ec_prime256v1" -> pure EcPRIME256V1
        "rsa_1024" -> pure Rsa1024
        "rsa_2048" -> pure Rsa2048
        e -> fromTextError $ "Failure parsing KeyAlgorithm from value: '" <> e
           <> "'. Accepted values: ec_prime256v1, rsa_1024, rsa_2048"

instance ToText KeyAlgorithm where
    toText = \case
        EcPRIME256V1 -> "EC_prime256v1"
        Rsa1024 -> "RSA_1024"
        Rsa2048 -> "RSA_2048"

instance Hashable     KeyAlgorithm
instance NFData       KeyAlgorithm
instance ToByteString KeyAlgorithm
instance ToQuery      KeyAlgorithm
instance ToHeader     KeyAlgorithm

instance FromJSON KeyAlgorithm where
    parseJSON = parseJSONText "KeyAlgorithm"

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

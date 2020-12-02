{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManagerPCA.Types.Sum where

import Network.AWS.Prelude

data AuditReportResponseFormat
  = CSV
  | JSON
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuditReportResponseFormat where
    parser = takeLowerText >>= \case
        "csv" -> pure CSV
        "json" -> pure JSON
        e -> fromTextError $ "Failure parsing AuditReportResponseFormat from value: '" <> e
           <> "'. Accepted values: csv, json"

instance ToText AuditReportResponseFormat where
    toText = \case
        CSV -> "CSV"
        JSON -> "JSON"

instance Hashable     AuditReportResponseFormat
instance NFData       AuditReportResponseFormat
instance ToByteString AuditReportResponseFormat
instance ToQuery      AuditReportResponseFormat
instance ToHeader     AuditReportResponseFormat

instance ToJSON AuditReportResponseFormat where
    toJSON = toJSONText

data AuditReportStatus
  = ARSCreating
  | ARSFailed
  | ARSSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AuditReportStatus where
    parser = takeLowerText >>= \case
        "creating" -> pure ARSCreating
        "failed" -> pure ARSFailed
        "success" -> pure ARSSuccess
        e -> fromTextError $ "Failure parsing AuditReportStatus from value: '" <> e
           <> "'. Accepted values: creating, failed, success"

instance ToText AuditReportStatus where
    toText = \case
        ARSCreating -> "CREATING"
        ARSFailed -> "FAILED"
        ARSSuccess -> "SUCCESS"

instance Hashable     AuditReportStatus
instance NFData       AuditReportStatus
instance ToByteString AuditReportStatus
instance ToQuery      AuditReportStatus
instance ToHeader     AuditReportStatus

instance FromJSON AuditReportStatus where
    parseJSON = parseJSONText "AuditReportStatus"

data CertificateAuthorityStatus
  = Active
  | Creating
  | Disabled
  | Expired
  | Failed
  | PendingCertificate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CertificateAuthorityStatus where
    parser = takeLowerText >>= \case
        "active" -> pure Active
        "creating" -> pure Creating
        "disabled" -> pure Disabled
        "expired" -> pure Expired
        "failed" -> pure Failed
        "pending_certificate" -> pure PendingCertificate
        e -> fromTextError $ "Failure parsing CertificateAuthorityStatus from value: '" <> e
           <> "'. Accepted values: active, creating, disabled, expired, failed, pending_certificate"

instance ToText CertificateAuthorityStatus where
    toText = \case
        Active -> "ACTIVE"
        Creating -> "CREATING"
        Disabled -> "DISABLED"
        Expired -> "EXPIRED"
        Failed -> "FAILED"
        PendingCertificate -> "PENDING_CERTIFICATE"

instance Hashable     CertificateAuthorityStatus
instance NFData       CertificateAuthorityStatus
instance ToByteString CertificateAuthorityStatus
instance ToQuery      CertificateAuthorityStatus
instance ToHeader     CertificateAuthorityStatus

instance ToJSON CertificateAuthorityStatus where
    toJSON = toJSONText

instance FromJSON CertificateAuthorityStatus where
    parseJSON = parseJSONText "CertificateAuthorityStatus"

data CertificateAuthorityType =
  Subordinate
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CertificateAuthorityType where
    parser = takeLowerText >>= \case
        "subordinate" -> pure Subordinate
        e -> fromTextError $ "Failure parsing CertificateAuthorityType from value: '" <> e
           <> "'. Accepted values: subordinate"

instance ToText CertificateAuthorityType where
    toText = \case
        Subordinate -> "SUBORDINATE"

instance Hashable     CertificateAuthorityType
instance NFData       CertificateAuthorityType
instance ToByteString CertificateAuthorityType
instance ToQuery      CertificateAuthorityType
instance ToHeader     CertificateAuthorityType

instance ToJSON CertificateAuthorityType where
    toJSON = toJSONText

instance FromJSON CertificateAuthorityType where
    parseJSON = parseJSONText "CertificateAuthorityType"

data FailureReason
  = Other
  | RequestTimedOut
  | UnsupportedAlgorithm
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FailureReason where
    parser = takeLowerText >>= \case
        "other" -> pure Other
        "request_timed_out" -> pure RequestTimedOut
        "unsupported_algorithm" -> pure UnsupportedAlgorithm
        e -> fromTextError $ "Failure parsing FailureReason from value: '" <> e
           <> "'. Accepted values: other, request_timed_out, unsupported_algorithm"

instance ToText FailureReason where
    toText = \case
        Other -> "OTHER"
        RequestTimedOut -> "REQUEST_TIMED_OUT"
        UnsupportedAlgorithm -> "UNSUPPORTED_ALGORITHM"

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
  | Rsa2048
  | Rsa4096
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText KeyAlgorithm where
    parser = takeLowerText >>= \case
        "ec_prime256v1" -> pure EcPRIME256V1
        "ec_secp384r1" -> pure EcSECP384R1
        "rsa_2048" -> pure Rsa2048
        "rsa_4096" -> pure Rsa4096
        e -> fromTextError $ "Failure parsing KeyAlgorithm from value: '" <> e
           <> "'. Accepted values: ec_prime256v1, ec_secp384r1, rsa_2048, rsa_4096"

instance ToText KeyAlgorithm where
    toText = \case
        EcPRIME256V1 -> "EC_prime256v1"
        EcSECP384R1 -> "EC_secp384r1"
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

data RevocationReason
  = AACompromise
  | AffiliationChanged
  | CertificateAuthorityCompromise
  | CessationOfOperation
  | KeyCompromise
  | PrivilegeWithdrawn
  | Superseded
  | Unspecified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RevocationReason where
    parser = takeLowerText >>= \case
        "a_a_compromise" -> pure AACompromise
        "affiliation_changed" -> pure AffiliationChanged
        "certificate_authority_compromise" -> pure CertificateAuthorityCompromise
        "cessation_of_operation" -> pure CessationOfOperation
        "key_compromise" -> pure KeyCompromise
        "privilege_withdrawn" -> pure PrivilegeWithdrawn
        "superseded" -> pure Superseded
        "unspecified" -> pure Unspecified
        e -> fromTextError $ "Failure parsing RevocationReason from value: '" <> e
           <> "'. Accepted values: a_a_compromise, affiliation_changed, certificate_authority_compromise, cessation_of_operation, key_compromise, privilege_withdrawn, superseded, unspecified"

instance ToText RevocationReason where
    toText = \case
        AACompromise -> "A_A_COMPROMISE"
        AffiliationChanged -> "AFFILIATION_CHANGED"
        CertificateAuthorityCompromise -> "CERTIFICATE_AUTHORITY_COMPROMISE"
        CessationOfOperation -> "CESSATION_OF_OPERATION"
        KeyCompromise -> "KEY_COMPROMISE"
        PrivilegeWithdrawn -> "PRIVILEGE_WITHDRAWN"
        Superseded -> "SUPERSEDED"
        Unspecified -> "UNSPECIFIED"

instance Hashable     RevocationReason
instance NFData       RevocationReason
instance ToByteString RevocationReason
instance ToQuery      RevocationReason
instance ToHeader     RevocationReason

instance ToJSON RevocationReason where
    toJSON = toJSONText

data SigningAlgorithm
  = SHA256WITHECDSA
  | SHA256WITHRSA
  | SHA384WITHECDSA
  | SHA384WITHRSA
  | SHA512WITHECDSA
  | SHA512WITHRSA
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SigningAlgorithm where
    parser = takeLowerText >>= \case
        "sha256withecdsa" -> pure SHA256WITHECDSA
        "sha256withrsa" -> pure SHA256WITHRSA
        "sha384withecdsa" -> pure SHA384WITHECDSA
        "sha384withrsa" -> pure SHA384WITHRSA
        "sha512withecdsa" -> pure SHA512WITHECDSA
        "sha512withrsa" -> pure SHA512WITHRSA
        e -> fromTextError $ "Failure parsing SigningAlgorithm from value: '" <> e
           <> "'. Accepted values: sha256withecdsa, sha256withrsa, sha384withecdsa, sha384withrsa, sha512withecdsa, sha512withrsa"

instance ToText SigningAlgorithm where
    toText = \case
        SHA256WITHECDSA -> "SHA256WITHECDSA"
        SHA256WITHRSA -> "SHA256WITHRSA"
        SHA384WITHECDSA -> "SHA384WITHECDSA"
        SHA384WITHRSA -> "SHA384WITHRSA"
        SHA512WITHECDSA -> "SHA512WITHECDSA"
        SHA512WITHRSA -> "SHA512WITHRSA"

instance Hashable     SigningAlgorithm
instance NFData       SigningAlgorithm
instance ToByteString SigningAlgorithm
instance ToQuery      SigningAlgorithm
instance ToHeader     SigningAlgorithm

instance ToJSON SigningAlgorithm where
    toJSON = toJSONText

instance FromJSON SigningAlgorithm where
    parseJSON = parseJSONText "SigningAlgorithm"

data ValidityPeriodType
  = Absolute
  | Days
  | EndDate
  | Months
  | Years
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ValidityPeriodType where
    parser = takeLowerText >>= \case
        "absolute" -> pure Absolute
        "days" -> pure Days
        "end_date" -> pure EndDate
        "months" -> pure Months
        "years" -> pure Years
        e -> fromTextError $ "Failure parsing ValidityPeriodType from value: '" <> e
           <> "'. Accepted values: absolute, days, end_date, months, years"

instance ToText ValidityPeriodType where
    toText = \case
        Absolute -> "ABSOLUTE"
        Days -> "DAYS"
        EndDate -> "END_DATE"
        Months -> "MONTHS"
        Years -> "YEARS"

instance Hashable     ValidityPeriodType
instance NFData       ValidityPeriodType
instance ToByteString ValidityPeriodType
instance ToQuery      ValidityPeriodType
instance ToHeader     ValidityPeriodType

instance ToJSON ValidityPeriodType where
    toJSON = toJSONText

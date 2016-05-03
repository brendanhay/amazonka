{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.Sum
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CertificateManager.Types.Sum where

import           Network.AWS.Prelude

data CertificateStatus
    = Expired
    | Failed
    | Inactive
    | Issued
    | PendingValidation
    | Revoked
    | ValidationTimedOut
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText CertificateStatus where
    parser = takeLowerText >>= \case
        "expired" -> pure Expired
        "failed" -> pure Failed
        "inactive" -> pure Inactive
        "issued" -> pure Issued
        "pending_validation" -> pure PendingValidation
        "revoked" -> pure Revoked
        "validation_timed_out" -> pure ValidationTimedOut
        e -> fromTextError $ "Failure parsing CertificateStatus from value: '" <> e
           <> "'. Accepted values: EXPIRED, FAILED, INACTIVE, ISSUED, PENDING_VALIDATION, REVOKED, VALIDATION_TIMED_OUT"

instance ToText CertificateStatus where
    toText = \case
        Expired -> "EXPIRED"
        Failed -> "FAILED"
        Inactive -> "INACTIVE"
        Issued -> "ISSUED"
        PendingValidation -> "PENDING_VALIDATION"
        Revoked -> "REVOKED"
        ValidationTimedOut -> "VALIDATION_TIMED_OUT"

instance Hashable     CertificateStatus
instance NFData       CertificateStatus
instance ToByteString CertificateStatus
instance ToQuery      CertificateStatus
instance ToHeader     CertificateStatus

instance ToJSON CertificateStatus where
    toJSON = toJSONText

instance FromJSON CertificateStatus where
    parseJSON = parseJSONText "CertificateStatus"

data KeyAlgorithm
    = EcPRIME256V1
    | Rsa2048
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

instance FromText KeyAlgorithm where
    parser = takeLowerText >>= \case
        "ec_prime256v1" -> pure EcPRIME256V1
        "rsa_2048" -> pure Rsa2048
        e -> fromTextError $ "Failure parsing KeyAlgorithm from value: '" <> e
           <> "'. Accepted values: EC_prime256v1, RSA_2048"

instance ToText KeyAlgorithm where
    toText = \case
        EcPRIME256V1 -> "EC_prime256v1"
        Rsa2048 -> "RSA_2048"

instance Hashable     KeyAlgorithm
instance NFData       KeyAlgorithm
instance ToByteString KeyAlgorithm
instance ToQuery      KeyAlgorithm
instance ToHeader     KeyAlgorithm

instance FromJSON KeyAlgorithm where
    parseJSON = parseJSONText "KeyAlgorithm"

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
    deriving (Eq,Ord,Read,Show,Enum,Bounded,Data,Typeable,Generic)

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
           <> "'. Accepted values: A_A_COMPROMISE, AFFILIATION_CHANGED, CA_COMPROMISE, CERTIFICATE_HOLD, CESSATION_OF_OPERATION, KEY_COMPROMISE, PRIVILEGE_WITHDRAWN, REMOVE_FROM_CRL, SUPERCEDED, UNSPECIFIED"

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

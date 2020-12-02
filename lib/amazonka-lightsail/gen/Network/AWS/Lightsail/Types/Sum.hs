{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Sum where

import Network.AWS.Prelude

data AccessDirection
  = Inbound
  | Outbound
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AccessDirection where
    parser = takeLowerText >>= \case
        "inbound" -> pure Inbound
        "outbound" -> pure Outbound
        e -> fromTextError $ "Failure parsing AccessDirection from value: '" <> e
           <> "'. Accepted values: inbound, outbound"

instance ToText AccessDirection where
    toText = \case
        Inbound -> "inbound"
        Outbound -> "outbound"

instance Hashable     AccessDirection
instance NFData       AccessDirection
instance ToByteString AccessDirection
instance ToQuery      AccessDirection
instance ToHeader     AccessDirection

instance FromJSON AccessDirection where
    parseJSON = parseJSONText "AccessDirection"

data BlueprintType
  = App
  | OS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText BlueprintType where
    parser = takeLowerText >>= \case
        "app" -> pure App
        "os" -> pure OS
        e -> fromTextError $ "Failure parsing BlueprintType from value: '" <> e
           <> "'. Accepted values: app, os"

instance ToText BlueprintType where
    toText = \case
        App -> "app"
        OS -> "os"

instance Hashable     BlueprintType
instance NFData       BlueprintType
instance ToByteString BlueprintType
instance ToQuery      BlueprintType
instance ToHeader     BlueprintType

instance FromJSON BlueprintType where
    parseJSON = parseJSONText "BlueprintType"

data DiskSnapshotState
  = DSSCompleted
  | DSSError'
  | DSSPending
  | DSSUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DiskSnapshotState where
    parser = takeLowerText >>= \case
        "completed" -> pure DSSCompleted
        "error" -> pure DSSError'
        "pending" -> pure DSSPending
        "unknown" -> pure DSSUnknown
        e -> fromTextError $ "Failure parsing DiskSnapshotState from value: '" <> e
           <> "'. Accepted values: completed, error, pending, unknown"

instance ToText DiskSnapshotState where
    toText = \case
        DSSCompleted -> "completed"
        DSSError' -> "error"
        DSSPending -> "pending"
        DSSUnknown -> "unknown"

instance Hashable     DiskSnapshotState
instance NFData       DiskSnapshotState
instance ToByteString DiskSnapshotState
instance ToQuery      DiskSnapshotState
instance ToHeader     DiskSnapshotState

instance FromJSON DiskSnapshotState where
    parseJSON = parseJSONText "DiskSnapshotState"

data DiskState
  = Available
  | Error'
  | InUse
  | Pending
  | Unknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DiskState where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "error" -> pure Error'
        "in-use" -> pure InUse
        "pending" -> pure Pending
        "unknown" -> pure Unknown
        e -> fromTextError $ "Failure parsing DiskState from value: '" <> e
           <> "'. Accepted values: available, error, in-use, pending, unknown"

instance ToText DiskState where
    toText = \case
        Available -> "available"
        Error' -> "error"
        InUse -> "in-use"
        Pending -> "pending"
        Unknown -> "unknown"

instance Hashable     DiskState
instance NFData       DiskState
instance ToByteString DiskState
instance ToQuery      DiskState
instance ToHeader     DiskState

instance FromJSON DiskState where
    parseJSON = parseJSONText "DiskState"

data InstanceAccessProtocol
  = Rdp
  | SSH
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceAccessProtocol where
    parser = takeLowerText >>= \case
        "rdp" -> pure Rdp
        "ssh" -> pure SSH
        e -> fromTextError $ "Failure parsing InstanceAccessProtocol from value: '" <> e
           <> "'. Accepted values: rdp, ssh"

instance ToText InstanceAccessProtocol where
    toText = \case
        Rdp -> "rdp"
        SSH -> "ssh"

instance Hashable     InstanceAccessProtocol
instance NFData       InstanceAccessProtocol
instance ToByteString InstanceAccessProtocol
instance ToQuery      InstanceAccessProtocol
instance ToHeader     InstanceAccessProtocol

instance ToJSON InstanceAccessProtocol where
    toJSON = toJSONText

instance FromJSON InstanceAccessProtocol where
    parseJSON = parseJSONText "InstanceAccessProtocol"

data InstanceHealthReason
  = Instance_DeregistrationInProgress
  | Instance_FailedHealthChecks
  | Instance_IPUnusable
  | Instance_InvalidState
  | Instance_NotInUse
  | Instance_NotRegistered
  | Instance_ResponseCodeMismatch
  | Instance_Timeout
  | Lb_InitialHealthChecking
  | Lb_InternalError
  | Lb_RegistrationInProgress
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceHealthReason where
    parser = takeLowerText >>= \case
        "instance.deregistrationinprogress" -> pure Instance_DeregistrationInProgress
        "instance.failedhealthchecks" -> pure Instance_FailedHealthChecks
        "instance.ipunusable" -> pure Instance_IPUnusable
        "instance.invalidstate" -> pure Instance_InvalidState
        "instance.notinuse" -> pure Instance_NotInUse
        "instance.notregistered" -> pure Instance_NotRegistered
        "instance.responsecodemismatch" -> pure Instance_ResponseCodeMismatch
        "instance.timeout" -> pure Instance_Timeout
        "lb.initialhealthchecking" -> pure Lb_InitialHealthChecking
        "lb.internalerror" -> pure Lb_InternalError
        "lb.registrationinprogress" -> pure Lb_RegistrationInProgress
        e -> fromTextError $ "Failure parsing InstanceHealthReason from value: '" <> e
           <> "'. Accepted values: instance.deregistrationinprogress, instance.failedhealthchecks, instance.ipunusable, instance.invalidstate, instance.notinuse, instance.notregistered, instance.responsecodemismatch, instance.timeout, lb.initialhealthchecking, lb.internalerror, lb.registrationinprogress"

instance ToText InstanceHealthReason where
    toText = \case
        Instance_DeregistrationInProgress -> "Instance.DeregistrationInProgress"
        Instance_FailedHealthChecks -> "Instance.FailedHealthChecks"
        Instance_IPUnusable -> "Instance.IpUnusable"
        Instance_InvalidState -> "Instance.InvalidState"
        Instance_NotInUse -> "Instance.NotInUse"
        Instance_NotRegistered -> "Instance.NotRegistered"
        Instance_ResponseCodeMismatch -> "Instance.ResponseCodeMismatch"
        Instance_Timeout -> "Instance.Timeout"
        Lb_InitialHealthChecking -> "Lb.InitialHealthChecking"
        Lb_InternalError -> "Lb.InternalError"
        Lb_RegistrationInProgress -> "Lb.RegistrationInProgress"

instance Hashable     InstanceHealthReason
instance NFData       InstanceHealthReason
instance ToByteString InstanceHealthReason
instance ToQuery      InstanceHealthReason
instance ToHeader     InstanceHealthReason

instance FromJSON InstanceHealthReason where
    parseJSON = parseJSONText "InstanceHealthReason"

data InstanceHealthState
  = Draining
  | Healthy
  | Initial
  | Unavailable
  | Unhealthy
  | Unused
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceHealthState where
    parser = takeLowerText >>= \case
        "draining" -> pure Draining
        "healthy" -> pure Healthy
        "initial" -> pure Initial
        "unavailable" -> pure Unavailable
        "unhealthy" -> pure Unhealthy
        "unused" -> pure Unused
        e -> fromTextError $ "Failure parsing InstanceHealthState from value: '" <> e
           <> "'. Accepted values: draining, healthy, initial, unavailable, unhealthy, unused"

instance ToText InstanceHealthState where
    toText = \case
        Draining -> "draining"
        Healthy -> "healthy"
        Initial -> "initial"
        Unavailable -> "unavailable"
        Unhealthy -> "unhealthy"
        Unused -> "unused"

instance Hashable     InstanceHealthState
instance NFData       InstanceHealthState
instance ToByteString InstanceHealthState
instance ToQuery      InstanceHealthState
instance ToHeader     InstanceHealthState

instance FromJSON InstanceHealthState where
    parseJSON = parseJSONText "InstanceHealthState"

data InstanceMetricName
  = CPUUtilization
  | NetworkIn
  | NetworkOut
  | StatusCheckFailed
  | StatusCheckFailedInstance
  | StatusCheckFailedSystem
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceMetricName where
    parser = takeLowerText >>= \case
        "cpuutilization" -> pure CPUUtilization
        "networkin" -> pure NetworkIn
        "networkout" -> pure NetworkOut
        "statuscheckfailed" -> pure StatusCheckFailed
        "statuscheckfailed_instance" -> pure StatusCheckFailedInstance
        "statuscheckfailed_system" -> pure StatusCheckFailedSystem
        e -> fromTextError $ "Failure parsing InstanceMetricName from value: '" <> e
           <> "'. Accepted values: cpuutilization, networkin, networkout, statuscheckfailed, statuscheckfailed_instance, statuscheckfailed_system"

instance ToText InstanceMetricName where
    toText = \case
        CPUUtilization -> "CPUUtilization"
        NetworkIn -> "NetworkIn"
        NetworkOut -> "NetworkOut"
        StatusCheckFailed -> "StatusCheckFailed"
        StatusCheckFailedInstance -> "StatusCheckFailed_Instance"
        StatusCheckFailedSystem -> "StatusCheckFailed_System"

instance Hashable     InstanceMetricName
instance NFData       InstanceMetricName
instance ToByteString InstanceMetricName
instance ToQuery      InstanceMetricName
instance ToHeader     InstanceMetricName

instance ToJSON InstanceMetricName where
    toJSON = toJSONText

instance FromJSON InstanceMetricName where
    parseJSON = parseJSONText "InstanceMetricName"

data InstancePlatform
  = LinuxUnix
  | Windows
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstancePlatform where
    parser = takeLowerText >>= \case
        "linux_unix" -> pure LinuxUnix
        "windows" -> pure Windows
        e -> fromTextError $ "Failure parsing InstancePlatform from value: '" <> e
           <> "'. Accepted values: linux_unix, windows"

instance ToText InstancePlatform where
    toText = \case
        LinuxUnix -> "LINUX_UNIX"
        Windows -> "WINDOWS"

instance Hashable     InstancePlatform
instance NFData       InstancePlatform
instance ToByteString InstancePlatform
instance ToQuery      InstancePlatform
instance ToHeader     InstancePlatform

instance FromJSON InstancePlatform where
    parseJSON = parseJSONText "InstancePlatform"

data InstanceSnapshotState
  = ISSAvailable
  | ISSError'
  | ISSPending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText InstanceSnapshotState where
    parser = takeLowerText >>= \case
        "available" -> pure ISSAvailable
        "error" -> pure ISSError'
        "pending" -> pure ISSPending
        e -> fromTextError $ "Failure parsing InstanceSnapshotState from value: '" <> e
           <> "'. Accepted values: available, error, pending"

instance ToText InstanceSnapshotState where
    toText = \case
        ISSAvailable -> "available"
        ISSError' -> "error"
        ISSPending -> "pending"

instance Hashable     InstanceSnapshotState
instance NFData       InstanceSnapshotState
instance ToByteString InstanceSnapshotState
instance ToQuery      InstanceSnapshotState
instance ToHeader     InstanceSnapshotState

instance FromJSON InstanceSnapshotState where
    parseJSON = parseJSONText "InstanceSnapshotState"

data LoadBalancerAttributeName
  = HealthCheckPath
  | SessionStickinessEnabled
  | SessionStickinessLbCookieDurationSeconds
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoadBalancerAttributeName where
    parser = takeLowerText >>= \case
        "healthcheckpath" -> pure HealthCheckPath
        "sessionstickinessenabled" -> pure SessionStickinessEnabled
        "sessionstickiness_lb_cookiedurationseconds" -> pure SessionStickinessLbCookieDurationSeconds
        e -> fromTextError $ "Failure parsing LoadBalancerAttributeName from value: '" <> e
           <> "'. Accepted values: healthcheckpath, sessionstickinessenabled, sessionstickiness_lb_cookiedurationseconds"

instance ToText LoadBalancerAttributeName where
    toText = \case
        HealthCheckPath -> "HealthCheckPath"
        SessionStickinessEnabled -> "SessionStickinessEnabled"
        SessionStickinessLbCookieDurationSeconds -> "SessionStickiness_LB_CookieDurationSeconds"

instance Hashable     LoadBalancerAttributeName
instance NFData       LoadBalancerAttributeName
instance ToByteString LoadBalancerAttributeName
instance ToQuery      LoadBalancerAttributeName
instance ToHeader     LoadBalancerAttributeName

instance ToJSON LoadBalancerAttributeName where
    toJSON = toJSONText

instance FromJSON LoadBalancerAttributeName where
    parseJSON = parseJSONText "LoadBalancerAttributeName"

data LoadBalancerMetricName
  = ClientTLSNegotiationErrorCount
  | HTTPCodeInstance2XXCount
  | HTTPCodeInstance3XXCount
  | HTTPCodeInstance4XXCount
  | HTTPCodeInstance5XXCount
  | HTTPCodeLb4XXCount
  | HTTPCodeLb5XXCount
  | HealthyHostCount
  | InstanceResponseTime
  | RejectedConnectionCount
  | RequestCount
  | UnhealthyHostCount
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoadBalancerMetricName where
    parser = takeLowerText >>= \case
        "clienttlsnegotiationerrorcount" -> pure ClientTLSNegotiationErrorCount
        "httpcode_instance_2xx_count" -> pure HTTPCodeInstance2XXCount
        "httpcode_instance_3xx_count" -> pure HTTPCodeInstance3XXCount
        "httpcode_instance_4xx_count" -> pure HTTPCodeInstance4XXCount
        "httpcode_instance_5xx_count" -> pure HTTPCodeInstance5XXCount
        "httpcode_lb_4xx_count" -> pure HTTPCodeLb4XXCount
        "httpcode_lb_5xx_count" -> pure HTTPCodeLb5XXCount
        "healthyhostcount" -> pure HealthyHostCount
        "instanceresponsetime" -> pure InstanceResponseTime
        "rejectedconnectioncount" -> pure RejectedConnectionCount
        "requestcount" -> pure RequestCount
        "unhealthyhostcount" -> pure UnhealthyHostCount
        e -> fromTextError $ "Failure parsing LoadBalancerMetricName from value: '" <> e
           <> "'. Accepted values: clienttlsnegotiationerrorcount, httpcode_instance_2xx_count, httpcode_instance_3xx_count, httpcode_instance_4xx_count, httpcode_instance_5xx_count, httpcode_lb_4xx_count, httpcode_lb_5xx_count, healthyhostcount, instanceresponsetime, rejectedconnectioncount, requestcount, unhealthyhostcount"

instance ToText LoadBalancerMetricName where
    toText = \case
        ClientTLSNegotiationErrorCount -> "ClientTLSNegotiationErrorCount"
        HTTPCodeInstance2XXCount -> "HTTPCode_Instance_2XX_Count"
        HTTPCodeInstance3XXCount -> "HTTPCode_Instance_3XX_Count"
        HTTPCodeInstance4XXCount -> "HTTPCode_Instance_4XX_Count"
        HTTPCodeInstance5XXCount -> "HTTPCode_Instance_5XX_Count"
        HTTPCodeLb4XXCount -> "HTTPCode_LB_4XX_Count"
        HTTPCodeLb5XXCount -> "HTTPCode_LB_5XX_Count"
        HealthyHostCount -> "HealthyHostCount"
        InstanceResponseTime -> "InstanceResponseTime"
        RejectedConnectionCount -> "RejectedConnectionCount"
        RequestCount -> "RequestCount"
        UnhealthyHostCount -> "UnhealthyHostCount"

instance Hashable     LoadBalancerMetricName
instance NFData       LoadBalancerMetricName
instance ToByteString LoadBalancerMetricName
instance ToQuery      LoadBalancerMetricName
instance ToHeader     LoadBalancerMetricName

instance ToJSON LoadBalancerMetricName where
    toJSON = toJSONText

instance FromJSON LoadBalancerMetricName where
    parseJSON = parseJSONText "LoadBalancerMetricName"

data LoadBalancerProtocol
  = HTTP
  | HTTPHTTPS
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoadBalancerProtocol where
    parser = takeLowerText >>= \case
        "http" -> pure HTTP
        "http_https" -> pure HTTPHTTPS
        e -> fromTextError $ "Failure parsing LoadBalancerProtocol from value: '" <> e
           <> "'. Accepted values: http, http_https"

instance ToText LoadBalancerProtocol where
    toText = \case
        HTTP -> "HTTP"
        HTTPHTTPS -> "HTTP_HTTPS"

instance Hashable     LoadBalancerProtocol
instance NFData       LoadBalancerProtocol
instance ToByteString LoadBalancerProtocol
instance ToQuery      LoadBalancerProtocol
instance ToHeader     LoadBalancerProtocol

instance FromJSON LoadBalancerProtocol where
    parseJSON = parseJSONText "LoadBalancerProtocol"

data LoadBalancerState
  = LBSActive
  | LBSActiveImpaired
  | LBSFailed
  | LBSProvisioning
  | LBSUnknown
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoadBalancerState where
    parser = takeLowerText >>= \case
        "active" -> pure LBSActive
        "active_impaired" -> pure LBSActiveImpaired
        "failed" -> pure LBSFailed
        "provisioning" -> pure LBSProvisioning
        "unknown" -> pure LBSUnknown
        e -> fromTextError $ "Failure parsing LoadBalancerState from value: '" <> e
           <> "'. Accepted values: active, active_impaired, failed, provisioning, unknown"

instance ToText LoadBalancerState where
    toText = \case
        LBSActive -> "active"
        LBSActiveImpaired -> "active_impaired"
        LBSFailed -> "failed"
        LBSProvisioning -> "provisioning"
        LBSUnknown -> "unknown"

instance Hashable     LoadBalancerState
instance NFData       LoadBalancerState
instance ToByteString LoadBalancerState
instance ToQuery      LoadBalancerState
instance ToHeader     LoadBalancerState

instance FromJSON LoadBalancerState where
    parseJSON = parseJSONText "LoadBalancerState"

data LoadBalancerTLSCertificateDomainStatus
  = LBTCDSFailed
  | LBTCDSPendingValidation
  | LBTCDSSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoadBalancerTLSCertificateDomainStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure LBTCDSFailed
        "pending_validation" -> pure LBTCDSPendingValidation
        "success" -> pure LBTCDSSuccess
        e -> fromTextError $ "Failure parsing LoadBalancerTLSCertificateDomainStatus from value: '" <> e
           <> "'. Accepted values: failed, pending_validation, success"

instance ToText LoadBalancerTLSCertificateDomainStatus where
    toText = \case
        LBTCDSFailed -> "FAILED"
        LBTCDSPendingValidation -> "PENDING_VALIDATION"
        LBTCDSSuccess -> "SUCCESS"

instance Hashable     LoadBalancerTLSCertificateDomainStatus
instance NFData       LoadBalancerTLSCertificateDomainStatus
instance ToByteString LoadBalancerTLSCertificateDomainStatus
instance ToQuery      LoadBalancerTLSCertificateDomainStatus
instance ToHeader     LoadBalancerTLSCertificateDomainStatus

instance FromJSON LoadBalancerTLSCertificateDomainStatus where
    parseJSON = parseJSONText "LoadBalancerTLSCertificateDomainStatus"

data LoadBalancerTLSCertificateFailureReason
  = AdditionalVerificationRequired
  | DomainNotAllowed
  | InvalidPublicDomain
  | NoAvailableContacts
  | Other
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoadBalancerTLSCertificateFailureReason where
    parser = takeLowerText >>= \case
        "additional_verification_required" -> pure AdditionalVerificationRequired
        "domain_not_allowed" -> pure DomainNotAllowed
        "invalid_public_domain" -> pure InvalidPublicDomain
        "no_available_contacts" -> pure NoAvailableContacts
        "other" -> pure Other
        e -> fromTextError $ "Failure parsing LoadBalancerTLSCertificateFailureReason from value: '" <> e
           <> "'. Accepted values: additional_verification_required, domain_not_allowed, invalid_public_domain, no_available_contacts, other"

instance ToText LoadBalancerTLSCertificateFailureReason where
    toText = \case
        AdditionalVerificationRequired -> "ADDITIONAL_VERIFICATION_REQUIRED"
        DomainNotAllowed -> "DOMAIN_NOT_ALLOWED"
        InvalidPublicDomain -> "INVALID_PUBLIC_DOMAIN"
        NoAvailableContacts -> "NO_AVAILABLE_CONTACTS"
        Other -> "OTHER"

instance Hashable     LoadBalancerTLSCertificateFailureReason
instance NFData       LoadBalancerTLSCertificateFailureReason
instance ToByteString LoadBalancerTLSCertificateFailureReason
instance ToQuery      LoadBalancerTLSCertificateFailureReason
instance ToHeader     LoadBalancerTLSCertificateFailureReason

instance FromJSON LoadBalancerTLSCertificateFailureReason where
    parseJSON = parseJSONText "LoadBalancerTLSCertificateFailureReason"

data LoadBalancerTLSCertificateRenewalStatus
  = LBTCRSFailed
  | LBTCRSPendingAutoRenewal
  | LBTCRSPendingValidation
  | LBTCRSSuccess
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoadBalancerTLSCertificateRenewalStatus where
    parser = takeLowerText >>= \case
        "failed" -> pure LBTCRSFailed
        "pending_auto_renewal" -> pure LBTCRSPendingAutoRenewal
        "pending_validation" -> pure LBTCRSPendingValidation
        "success" -> pure LBTCRSSuccess
        e -> fromTextError $ "Failure parsing LoadBalancerTLSCertificateRenewalStatus from value: '" <> e
           <> "'. Accepted values: failed, pending_auto_renewal, pending_validation, success"

instance ToText LoadBalancerTLSCertificateRenewalStatus where
    toText = \case
        LBTCRSFailed -> "FAILED"
        LBTCRSPendingAutoRenewal -> "PENDING_AUTO_RENEWAL"
        LBTCRSPendingValidation -> "PENDING_VALIDATION"
        LBTCRSSuccess -> "SUCCESS"

instance Hashable     LoadBalancerTLSCertificateRenewalStatus
instance NFData       LoadBalancerTLSCertificateRenewalStatus
instance ToByteString LoadBalancerTLSCertificateRenewalStatus
instance ToQuery      LoadBalancerTLSCertificateRenewalStatus
instance ToHeader     LoadBalancerTLSCertificateRenewalStatus

instance FromJSON LoadBalancerTLSCertificateRenewalStatus where
    parseJSON = parseJSONText "LoadBalancerTLSCertificateRenewalStatus"

data LoadBalancerTLSCertificateRevocationReason
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


instance FromText LoadBalancerTLSCertificateRevocationReason where
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
        e -> fromTextError $ "Failure parsing LoadBalancerTLSCertificateRevocationReason from value: '" <> e
           <> "'. Accepted values: a_a_compromise, affiliation_changed, ca_compromise, certificate_hold, cessation_of_operation, key_compromise, privilege_withdrawn, remove_from_crl, superceded, unspecified"

instance ToText LoadBalancerTLSCertificateRevocationReason where
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

instance Hashable     LoadBalancerTLSCertificateRevocationReason
instance NFData       LoadBalancerTLSCertificateRevocationReason
instance ToByteString LoadBalancerTLSCertificateRevocationReason
instance ToQuery      LoadBalancerTLSCertificateRevocationReason
instance ToHeader     LoadBalancerTLSCertificateRevocationReason

instance FromJSON LoadBalancerTLSCertificateRevocationReason where
    parseJSON = parseJSONText "LoadBalancerTLSCertificateRevocationReason"

data LoadBalancerTLSCertificateStatus
  = LBTCSExpired
  | LBTCSFailed
  | LBTCSInactive
  | LBTCSIssued
  | LBTCSPendingValidation
  | LBTCSRevoked
  | LBTCSUnknown
  | LBTCSValidationTimedOut
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LoadBalancerTLSCertificateStatus where
    parser = takeLowerText >>= \case
        "expired" -> pure LBTCSExpired
        "failed" -> pure LBTCSFailed
        "inactive" -> pure LBTCSInactive
        "issued" -> pure LBTCSIssued
        "pending_validation" -> pure LBTCSPendingValidation
        "revoked" -> pure LBTCSRevoked
        "unknown" -> pure LBTCSUnknown
        "validation_timed_out" -> pure LBTCSValidationTimedOut
        e -> fromTextError $ "Failure parsing LoadBalancerTLSCertificateStatus from value: '" <> e
           <> "'. Accepted values: expired, failed, inactive, issued, pending_validation, revoked, unknown, validation_timed_out"

instance ToText LoadBalancerTLSCertificateStatus where
    toText = \case
        LBTCSExpired -> "EXPIRED"
        LBTCSFailed -> "FAILED"
        LBTCSInactive -> "INACTIVE"
        LBTCSIssued -> "ISSUED"
        LBTCSPendingValidation -> "PENDING_VALIDATION"
        LBTCSRevoked -> "REVOKED"
        LBTCSUnknown -> "UNKNOWN"
        LBTCSValidationTimedOut -> "VALIDATION_TIMED_OUT"

instance Hashable     LoadBalancerTLSCertificateStatus
instance NFData       LoadBalancerTLSCertificateStatus
instance ToByteString LoadBalancerTLSCertificateStatus
instance ToQuery      LoadBalancerTLSCertificateStatus
instance ToHeader     LoadBalancerTLSCertificateStatus

instance FromJSON LoadBalancerTLSCertificateStatus where
    parseJSON = parseJSONText "LoadBalancerTLSCertificateStatus"

data MetricStatistic
  = Average
  | Maximum
  | Minimum
  | SampleCount
  | Sum
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricStatistic where
    parser = takeLowerText >>= \case
        "average" -> pure Average
        "maximum" -> pure Maximum
        "minimum" -> pure Minimum
        "samplecount" -> pure SampleCount
        "sum" -> pure Sum
        e -> fromTextError $ "Failure parsing MetricStatistic from value: '" <> e
           <> "'. Accepted values: average, maximum, minimum, samplecount, sum"

instance ToText MetricStatistic where
    toText = \case
        Average -> "Average"
        Maximum -> "Maximum"
        Minimum -> "Minimum"
        SampleCount -> "SampleCount"
        Sum -> "Sum"

instance Hashable     MetricStatistic
instance NFData       MetricStatistic
instance ToByteString MetricStatistic
instance ToQuery      MetricStatistic
instance ToHeader     MetricStatistic

instance ToJSON MetricStatistic where
    toJSON = toJSONText

data MetricUnit
  = Bits
  | BitsSecond
  | Bytes
  | BytesSecond
  | Count
  | CountSecond
  | Gigabits
  | GigabitsSecond
  | Gigabytes
  | GigabytesSecond
  | Kilobits
  | KilobitsSecond
  | Kilobytes
  | KilobytesSecond
  | Megabits
  | MegabitsSecond
  | Megabytes
  | MegabytesSecond
  | Microseconds
  | Milliseconds
  | None
  | Percent
  | Seconds
  | Terabits
  | TerabitsSecond
  | Terabytes
  | TerabytesSecond
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MetricUnit where
    parser = takeLowerText >>= \case
        "bits" -> pure Bits
        "bits/second" -> pure BitsSecond
        "bytes" -> pure Bytes
        "bytes/second" -> pure BytesSecond
        "count" -> pure Count
        "count/second" -> pure CountSecond
        "gigabits" -> pure Gigabits
        "gigabits/second" -> pure GigabitsSecond
        "gigabytes" -> pure Gigabytes
        "gigabytes/second" -> pure GigabytesSecond
        "kilobits" -> pure Kilobits
        "kilobits/second" -> pure KilobitsSecond
        "kilobytes" -> pure Kilobytes
        "kilobytes/second" -> pure KilobytesSecond
        "megabits" -> pure Megabits
        "megabits/second" -> pure MegabitsSecond
        "megabytes" -> pure Megabytes
        "megabytes/second" -> pure MegabytesSecond
        "microseconds" -> pure Microseconds
        "milliseconds" -> pure Milliseconds
        "none" -> pure None
        "percent" -> pure Percent
        "seconds" -> pure Seconds
        "terabits" -> pure Terabits
        "terabits/second" -> pure TerabitsSecond
        "terabytes" -> pure Terabytes
        "terabytes/second" -> pure TerabytesSecond
        e -> fromTextError $ "Failure parsing MetricUnit from value: '" <> e
           <> "'. Accepted values: bits, bits/second, bytes, bytes/second, count, count/second, gigabits, gigabits/second, gigabytes, gigabytes/second, kilobits, kilobits/second, kilobytes, kilobytes/second, megabits, megabits/second, megabytes, megabytes/second, microseconds, milliseconds, none, percent, seconds, terabits, terabits/second, terabytes, terabytes/second"

instance ToText MetricUnit where
    toText = \case
        Bits -> "Bits"
        BitsSecond -> "Bits/Second"
        Bytes -> "Bytes"
        BytesSecond -> "Bytes/Second"
        Count -> "Count"
        CountSecond -> "Count/Second"
        Gigabits -> "Gigabits"
        GigabitsSecond -> "Gigabits/Second"
        Gigabytes -> "Gigabytes"
        GigabytesSecond -> "Gigabytes/Second"
        Kilobits -> "Kilobits"
        KilobitsSecond -> "Kilobits/Second"
        Kilobytes -> "Kilobytes"
        KilobytesSecond -> "Kilobytes/Second"
        Megabits -> "Megabits"
        MegabitsSecond -> "Megabits/Second"
        Megabytes -> "Megabytes"
        MegabytesSecond -> "Megabytes/Second"
        Microseconds -> "Microseconds"
        Milliseconds -> "Milliseconds"
        None -> "None"
        Percent -> "Percent"
        Seconds -> "Seconds"
        Terabits -> "Terabits"
        TerabitsSecond -> "Terabits/Second"
        Terabytes -> "Terabytes"
        TerabytesSecond -> "Terabytes/Second"

instance Hashable     MetricUnit
instance NFData       MetricUnit
instance ToByteString MetricUnit
instance ToQuery      MetricUnit
instance ToHeader     MetricUnit

instance ToJSON MetricUnit where
    toJSON = toJSONText

instance FromJSON MetricUnit where
    parseJSON = parseJSONText "MetricUnit"

data NetworkProtocol
  = All
  | TCP
  | Udp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText NetworkProtocol where
    parser = takeLowerText >>= \case
        "all" -> pure All
        "tcp" -> pure TCP
        "udp" -> pure Udp
        e -> fromTextError $ "Failure parsing NetworkProtocol from value: '" <> e
           <> "'. Accepted values: all, tcp, udp"

instance ToText NetworkProtocol where
    toText = \case
        All -> "all"
        TCP -> "tcp"
        Udp -> "udp"

instance Hashable     NetworkProtocol
instance NFData       NetworkProtocol
instance ToByteString NetworkProtocol
instance ToQuery      NetworkProtocol
instance ToHeader     NetworkProtocol

instance ToJSON NetworkProtocol where
    toJSON = toJSONText

instance FromJSON NetworkProtocol where
    parseJSON = parseJSONText "NetworkProtocol"

data OperationStatus
  = Completed
  | Failed
  | NotStarted
  | Started
  | Succeeded
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "failed" -> pure Failed
        "notstarted" -> pure NotStarted
        "started" -> pure Started
        "succeeded" -> pure Succeeded
        e -> fromTextError $ "Failure parsing OperationStatus from value: '" <> e
           <> "'. Accepted values: completed, failed, notstarted, started, succeeded"

instance ToText OperationStatus where
    toText = \case
        Completed -> "Completed"
        Failed -> "Failed"
        NotStarted -> "NotStarted"
        Started -> "Started"
        Succeeded -> "Succeeded"

instance Hashable     OperationStatus
instance NFData       OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

data OperationType
  = AllocateStaticIP
  | AttachDisk
  | AttachInstancesToLoadBalancer
  | AttachLoadBalancerTLSCertificate
  | AttachStaticIP
  | CloseInstancePublicPorts
  | CreateDisk
  | CreateDiskFromSnapshot
  | CreateDiskSnapshot
  | CreateDomain
  | CreateInstance
  | CreateInstanceSnapshot
  | CreateInstancesFromSnapshot
  | CreateLoadBalancer
  | CreateLoadBalancerTLSCertificate
  | DeleteDisk
  | DeleteDiskSnapshot
  | DeleteDomain
  | DeleteDomainEntry
  | DeleteInstance
  | DeleteInstanceSnapshot
  | DeleteLoadBalancer
  | DeleteLoadBalancerTLSCertificate
  | DetachDisk
  | DetachInstancesFromLoadBalancer
  | DetachStaticIP
  | OpenInstancePublicPorts
  | PutInstancePublicPorts
  | RebootInstance
  | ReleaseStaticIP
  | StartInstance
  | StopInstance
  | UpdateDomainEntry
  | UpdateLoadBalancerAttribute
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationType where
    parser = takeLowerText >>= \case
        "allocatestaticip" -> pure AllocateStaticIP
        "attachdisk" -> pure AttachDisk
        "attachinstancestoloadbalancer" -> pure AttachInstancesToLoadBalancer
        "attachloadbalancertlscertificate" -> pure AttachLoadBalancerTLSCertificate
        "attachstaticip" -> pure AttachStaticIP
        "closeinstancepublicports" -> pure CloseInstancePublicPorts
        "createdisk" -> pure CreateDisk
        "creatediskfromsnapshot" -> pure CreateDiskFromSnapshot
        "createdisksnapshot" -> pure CreateDiskSnapshot
        "createdomain" -> pure CreateDomain
        "createinstance" -> pure CreateInstance
        "createinstancesnapshot" -> pure CreateInstanceSnapshot
        "createinstancesfromsnapshot" -> pure CreateInstancesFromSnapshot
        "createloadbalancer" -> pure CreateLoadBalancer
        "createloadbalancertlscertificate" -> pure CreateLoadBalancerTLSCertificate
        "deletedisk" -> pure DeleteDisk
        "deletedisksnapshot" -> pure DeleteDiskSnapshot
        "deletedomain" -> pure DeleteDomain
        "deletedomainentry" -> pure DeleteDomainEntry
        "deleteinstance" -> pure DeleteInstance
        "deleteinstancesnapshot" -> pure DeleteInstanceSnapshot
        "deleteloadbalancer" -> pure DeleteLoadBalancer
        "deleteloadbalancertlscertificate" -> pure DeleteLoadBalancerTLSCertificate
        "detachdisk" -> pure DetachDisk
        "detachinstancesfromloadbalancer" -> pure DetachInstancesFromLoadBalancer
        "detachstaticip" -> pure DetachStaticIP
        "openinstancepublicports" -> pure OpenInstancePublicPorts
        "putinstancepublicports" -> pure PutInstancePublicPorts
        "rebootinstance" -> pure RebootInstance
        "releasestaticip" -> pure ReleaseStaticIP
        "startinstance" -> pure StartInstance
        "stopinstance" -> pure StopInstance
        "updatedomainentry" -> pure UpdateDomainEntry
        "updateloadbalancerattribute" -> pure UpdateLoadBalancerAttribute
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: allocatestaticip, attachdisk, attachinstancestoloadbalancer, attachloadbalancertlscertificate, attachstaticip, closeinstancepublicports, createdisk, creatediskfromsnapshot, createdisksnapshot, createdomain, createinstance, createinstancesnapshot, createinstancesfromsnapshot, createloadbalancer, createloadbalancertlscertificate, deletedisk, deletedisksnapshot, deletedomain, deletedomainentry, deleteinstance, deleteinstancesnapshot, deleteloadbalancer, deleteloadbalancertlscertificate, detachdisk, detachinstancesfromloadbalancer, detachstaticip, openinstancepublicports, putinstancepublicports, rebootinstance, releasestaticip, startinstance, stopinstance, updatedomainentry, updateloadbalancerattribute"

instance ToText OperationType where
    toText = \case
        AllocateStaticIP -> "AllocateStaticIp"
        AttachDisk -> "AttachDisk"
        AttachInstancesToLoadBalancer -> "AttachInstancesToLoadBalancer"
        AttachLoadBalancerTLSCertificate -> "AttachLoadBalancerTlsCertificate"
        AttachStaticIP -> "AttachStaticIp"
        CloseInstancePublicPorts -> "CloseInstancePublicPorts"
        CreateDisk -> "CreateDisk"
        CreateDiskFromSnapshot -> "CreateDiskFromSnapshot"
        CreateDiskSnapshot -> "CreateDiskSnapshot"
        CreateDomain -> "CreateDomain"
        CreateInstance -> "CreateInstance"
        CreateInstanceSnapshot -> "CreateInstanceSnapshot"
        CreateInstancesFromSnapshot -> "CreateInstancesFromSnapshot"
        CreateLoadBalancer -> "CreateLoadBalancer"
        CreateLoadBalancerTLSCertificate -> "CreateLoadBalancerTlsCertificate"
        DeleteDisk -> "DeleteDisk"
        DeleteDiskSnapshot -> "DeleteDiskSnapshot"
        DeleteDomain -> "DeleteDomain"
        DeleteDomainEntry -> "DeleteDomainEntry"
        DeleteInstance -> "DeleteInstance"
        DeleteInstanceSnapshot -> "DeleteInstanceSnapshot"
        DeleteLoadBalancer -> "DeleteLoadBalancer"
        DeleteLoadBalancerTLSCertificate -> "DeleteLoadBalancerTlsCertificate"
        DetachDisk -> "DetachDisk"
        DetachInstancesFromLoadBalancer -> "DetachInstancesFromLoadBalancer"
        DetachStaticIP -> "DetachStaticIp"
        OpenInstancePublicPorts -> "OpenInstancePublicPorts"
        PutInstancePublicPorts -> "PutInstancePublicPorts"
        RebootInstance -> "RebootInstance"
        ReleaseStaticIP -> "ReleaseStaticIp"
        StartInstance -> "StartInstance"
        StopInstance -> "StopInstance"
        UpdateDomainEntry -> "UpdateDomainEntry"
        UpdateLoadBalancerAttribute -> "UpdateLoadBalancerAttribute"

instance Hashable     OperationType
instance NFData       OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

data PortAccessType
  = Private
  | Public
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PortAccessType where
    parser = takeLowerText >>= \case
        "private" -> pure Private
        "public" -> pure Public
        e -> fromTextError $ "Failure parsing PortAccessType from value: '" <> e
           <> "'. Accepted values: private, public"

instance ToText PortAccessType where
    toText = \case
        Private -> "Private"
        Public -> "Public"

instance Hashable     PortAccessType
instance NFData       PortAccessType
instance ToByteString PortAccessType
instance ToQuery      PortAccessType
instance ToHeader     PortAccessType

instance FromJSON PortAccessType where
    parseJSON = parseJSONText "PortAccessType"

data PortState
  = Closed
  | Open
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PortState where
    parser = takeLowerText >>= \case
        "closed" -> pure Closed
        "open" -> pure Open
        e -> fromTextError $ "Failure parsing PortState from value: '" <> e
           <> "'. Accepted values: closed, open"

instance ToText PortState where
    toText = \case
        Closed -> "closed"
        Open -> "open"

instance Hashable     PortState
instance NFData       PortState
instance ToByteString PortState
instance ToQuery      PortState
instance ToHeader     PortState

instance FromJSON PortState where
    parseJSON = parseJSONText "PortState"

data RegionName
  = ApNortheast1
  | ApNortheast2
  | ApSouth1
  | ApSoutheast1
  | ApSoutheast2
  | EuCentral1
  | EuWest1
  | EuWest2
  | UsEast1
  | UsEast2
  | UsWest1
  | UsWest2
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RegionName where
    parser = takeLowerText >>= \case
        "ap-northeast-1" -> pure ApNortheast1
        "ap-northeast-2" -> pure ApNortheast2
        "ap-south-1" -> pure ApSouth1
        "ap-southeast-1" -> pure ApSoutheast1
        "ap-southeast-2" -> pure ApSoutheast2
        "eu-central-1" -> pure EuCentral1
        "eu-west-1" -> pure EuWest1
        "eu-west-2" -> pure EuWest2
        "us-east-1" -> pure UsEast1
        "us-east-2" -> pure UsEast2
        "us-west-1" -> pure UsWest1
        "us-west-2" -> pure UsWest2
        e -> fromTextError $ "Failure parsing RegionName from value: '" <> e
           <> "'. Accepted values: ap-northeast-1, ap-northeast-2, ap-south-1, ap-southeast-1, ap-southeast-2, eu-central-1, eu-west-1, eu-west-2, us-east-1, us-east-2, us-west-1, us-west-2"

instance ToText RegionName where
    toText = \case
        ApNortheast1 -> "ap-northeast-1"
        ApNortheast2 -> "ap-northeast-2"
        ApSouth1 -> "ap-south-1"
        ApSoutheast1 -> "ap-southeast-1"
        ApSoutheast2 -> "ap-southeast-2"
        EuCentral1 -> "eu-central-1"
        EuWest1 -> "eu-west-1"
        EuWest2 -> "eu-west-2"
        UsEast1 -> "us-east-1"
        UsEast2 -> "us-east-2"
        UsWest1 -> "us-west-1"
        UsWest2 -> "us-west-2"

instance Hashable     RegionName
instance NFData       RegionName
instance ToByteString RegionName
instance ToQuery      RegionName
instance ToHeader     RegionName

instance FromJSON RegionName where
    parseJSON = parseJSONText "RegionName"

data ResourceType
  = Disk
  | DiskSnapshot
  | Domain
  | Instance
  | InstanceSnapshot
  | KeyPair
  | LoadBalancer
  | LoadBalancerTLSCertificate
  | PeeredVPC
  | StaticIP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "disk" -> pure Disk
        "disksnapshot" -> pure DiskSnapshot
        "domain" -> pure Domain
        "instance" -> pure Instance
        "instancesnapshot" -> pure InstanceSnapshot
        "keypair" -> pure KeyPair
        "loadbalancer" -> pure LoadBalancer
        "loadbalancertlscertificate" -> pure LoadBalancerTLSCertificate
        "peeredvpc" -> pure PeeredVPC
        "staticip" -> pure StaticIP
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: disk, disksnapshot, domain, instance, instancesnapshot, keypair, loadbalancer, loadbalancertlscertificate, peeredvpc, staticip"

instance ToText ResourceType where
    toText = \case
        Disk -> "Disk"
        DiskSnapshot -> "DiskSnapshot"
        Domain -> "Domain"
        Instance -> "Instance"
        InstanceSnapshot -> "InstanceSnapshot"
        KeyPair -> "KeyPair"
        LoadBalancer -> "LoadBalancer"
        LoadBalancerTLSCertificate -> "LoadBalancerTlsCertificate"
        PeeredVPC -> "PeeredVpc"
        StaticIP -> "StaticIp"

instance Hashable     ResourceType
instance NFData       ResourceType
instance ToByteString ResourceType
instance ToQuery      ResourceType
instance ToHeader     ResourceType

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

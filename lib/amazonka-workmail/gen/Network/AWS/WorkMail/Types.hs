-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    , _DirectoryUnavailableException
    , _InvalidParameterException
    , _UnsupportedOperationException
    , _DirectoryServiceAuthenticationFailedException
    , _OrganizationStateException
    , _EntityStateException
    , _InvalidConfigurationException
    , _TooManyTagsException
    , _MailDomainStateException
    , _ReservedNameException
    , _OrganizationNotFoundException
    , _EntityNotFoundException
    , _EntityAlreadyRegisteredException
    , _DirectoryInUseException
    , _MailDomainNotFoundException
    , _ResourceNotFoundException
    , _EmailAddressInUseException
    , _NameAvailabilityException
    , _LimitExceededException
    , _InvalidPasswordException

    -- * EntityState
    , EntityState (..)

    -- * OrganizationName
    , OrganizationName (..)

    -- * DirectoryId
    , DirectoryId (..)

    -- * Delegate
    , Delegate (..)
    , mkDelegate
    , dId
    , dType

    -- * Group
    , Group (..)
    , mkGroup
    , gDisabledDate
    , gEmail
    , gEnabledDate
    , gId
    , gName
    , gState

    -- * OrganizationSummary
    , OrganizationSummary (..)
    , mkOrganizationSummary
    , osAlias
    , osDefaultMailDomain
    , osErrorMessage
    , osOrganizationId
    , osState

    -- * KmsKeyArn
    , KmsKeyArn (..)

    -- * IpAddress
    , IpAddress (..)

    -- * ResourceId
    , ResourceId (..)

    -- * Tag
    , Tag (..)
    , mkTag
    , tKey
    , tValue

    -- * MailboxExportJobId
    , MailboxExportJobId (..)

    -- * S3ObjectKey
    , S3ObjectKey (..)

    -- * HostedZoneId
    , HostedZoneId (..)

    -- * AccessControlRuleName
    , AccessControlRuleName (..)

    -- * ResourceType
    , ResourceType (..)

    -- * IdempotencyClientToken
    , IdempotencyClientToken (..)

    -- * ResourceName
    , ResourceName (..)

    -- * MemberType
    , MemberType (..)

    -- * MailboxExportJobState
    , MailboxExportJobState (..)

    -- * Domain
    , Domain (..)
    , mkDomain
    , dDomainName
    , dHostedZoneId

    -- * AccessControlRuleAction
    , AccessControlRuleAction (..)

    -- * AccessControlRuleDescription
    , AccessControlRuleDescription (..)

    -- * WorkMailIdentifier
    , WorkMailIdentifier (..)

    -- * PermissionType
    , PermissionType (..)

    -- * UserName
    , UserName (..)

    -- * PolicyDescription
    , PolicyDescription (..)

    -- * User
    , User (..)
    , mkUser
    , uDisabledDate
    , uDisplayName
    , uEmail
    , uEnabledDate
    , uId
    , uName
    , uState
    , uUserRole

    -- * FolderName
    , FolderName (..)

    -- * NextToken
    , NextToken (..)

    -- * MailboxExportJob
    , MailboxExportJob (..)
    , mkMailboxExportJob
    , mejDescription
    , mejEndTime
    , mejEntityId
    , mejEstimatedProgress
    , mejJobId
    , mejS3BucketName
    , mejS3Path
    , mejStartTime
    , mejState

    -- * DomainName
    , DomainName (..)

    -- * ShortString
    , ShortString (..)

    -- * Password
    , Password (..)

    -- * EmailAddress
    , EmailAddress (..)

    -- * RetentionAction
    , RetentionAction (..)

    -- * AccessControlRuleEffect
    , AccessControlRuleEffect (..)

    -- * Resource
    , Resource (..)
    , mkResource
    , rDisabledDate
    , rEmail
    , rEnabledDate
    , rId
    , rName
    , rState
    , rType

    -- * AccessControlRule
    , AccessControlRule (..)
    , mkAccessControlRule
    , acrActions
    , acrDateCreated
    , acrDateModified
    , acrDescription
    , acrEffect
    , acrIpRanges
    , acrName
    , acrNotActions
    , acrNotIpRanges
    , acrNotUserIds
    , acrUserIds

    -- * UserRole
    , UserRole (..)

    -- * TagKey
    , TagKey (..)

    -- * Member
    , Member (..)
    , mkMember
    , mDisabledDate
    , mEnabledDate
    , mId
    , mName
    , mState
    , mType

    -- * Permission
    , Permission (..)
    , mkPermission
    , pGranteeId
    , pGranteeType
    , pPermissionValues

    -- * AmazonResourceName
    , AmazonResourceName (..)

    -- * IpRange
    , IpRange (..)

    -- * Description
    , Description (..)

    -- * FolderConfiguration
    , FolderConfiguration (..)
    , mkFolderConfiguration
    , fcName
    , fcAction
    , fcPeriod

    -- * OrganizationId
    , OrganizationId (..)

    -- * S3BucketName
    , S3BucketName (..)

    -- * BookingOptions
    , BookingOptions (..)
    , mkBookingOptions
    , boAutoAcceptRequests
    , boAutoDeclineConflictingRequests
    , boAutoDeclineRecurringRequests

    -- * RoleArn
    , RoleArn (..)

    -- * Email
    , Email (..)

    -- * Name
    , Name (..)

    -- * EntityId
    , EntityId (..)

    -- * GranteeId
    , GranteeId (..)

    -- * Id
    , Id (..)

    -- * DefaultMailDomain
    , DefaultMailDomain (..)

    -- * GroupId
    , GroupId (..)

    -- * Key
    , Key (..)

    -- * Value
    , Value (..)

    -- * MemberId
    , MemberId (..)

    -- * UserId
    , UserId (..)

    -- * ARN
    , ARN (..)

    -- * Alias
    , Alias (..)

    -- * ResourceARN
    , ResourceARN (..)

    -- * ErrorInfo
    , ErrorInfo (..)
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WorkMail.Types.EntityState
  
import Network.AWS.WorkMail.Types.OrganizationName
  
  
import Network.AWS.WorkMail.Types.DirectoryId
  
import Network.AWS.WorkMail.Types.Delegate
  
import Network.AWS.WorkMail.Types.Group
  
import Network.AWS.WorkMail.Types.OrganizationSummary
  
  
import Network.AWS.WorkMail.Types.KmsKeyArn
  
import Network.AWS.WorkMail.Types.IpAddress
  
  
import Network.AWS.WorkMail.Types.ResourceId
  
import Network.AWS.WorkMail.Types.Tag
  
import Network.AWS.WorkMail.Types.MailboxExportJobId
  
import Network.AWS.WorkMail.Types.S3ObjectKey
  
import Network.AWS.WorkMail.Types.HostedZoneId
  
import Network.AWS.WorkMail.Types.AccessControlRuleName
  
import Network.AWS.WorkMail.Types.ResourceType
  
import Network.AWS.WorkMail.Types.IdempotencyClientToken
  
import Network.AWS.WorkMail.Types.ResourceName
  
import Network.AWS.WorkMail.Types.MemberType
  
import Network.AWS.WorkMail.Types.MailboxExportJobState
  
import Network.AWS.WorkMail.Types.Domain
  
  
import Network.AWS.WorkMail.Types.AccessControlRuleAction
  
import Network.AWS.WorkMail.Types.AccessControlRuleDescription
  
import Network.AWS.WorkMail.Types.WorkMailIdentifier
  
  
import Network.AWS.WorkMail.Types.PermissionType
  
  
  
import Network.AWS.WorkMail.Types.UserName
  
import Network.AWS.WorkMail.Types.PolicyDescription
  
  
import Network.AWS.WorkMail.Types.User
  
import Network.AWS.WorkMail.Types.FolderName
  
  
import Network.AWS.WorkMail.Types.NextToken
  
  
import Network.AWS.WorkMail.Types.MailboxExportJob
  
  
import Network.AWS.WorkMail.Types.DomainName
  
import Network.AWS.WorkMail.Types.ShortString
  
import Network.AWS.WorkMail.Types.Password
  
  
  
  
import Network.AWS.WorkMail.Types.EmailAddress
  
import Network.AWS.WorkMail.Types.RetentionAction
  
import Network.AWS.WorkMail.Types.AccessControlRuleEffect
  
import Network.AWS.WorkMail.Types.Resource
  
import Network.AWS.WorkMail.Types.AccessControlRule
  
  
import Network.AWS.WorkMail.Types.UserRole
  
import Network.AWS.WorkMail.Types.TagKey
  
import Network.AWS.WorkMail.Types.Member
  
import Network.AWS.WorkMail.Types.Permission
  
import Network.AWS.WorkMail.Types.AmazonResourceName
  
import Network.AWS.WorkMail.Types.IpRange
  
import Network.AWS.WorkMail.Types.Description
  
import Network.AWS.WorkMail.Types.FolderConfiguration
  
  
import Network.AWS.WorkMail.Types.OrganizationId
  
import Network.AWS.WorkMail.Types.S3BucketName
  
import Network.AWS.WorkMail.Types.BookingOptions
  
  
  
  
  
import Network.AWS.WorkMail.Types.RoleArn
  
import Network.AWS.WorkMail.Types.Email
  
import Network.AWS.WorkMail.Types.Name
  
import Network.AWS.WorkMail.Types.EntityId
  
import Network.AWS.WorkMail.Types.GranteeId
  
import Network.AWS.WorkMail.Types.Id
  
import Network.AWS.WorkMail.Types.DefaultMailDomain
  
import Network.AWS.WorkMail.Types.GroupId
  
import Network.AWS.WorkMail.Types.Key
  
import Network.AWS.WorkMail.Types.Value
  
import Network.AWS.WorkMail.Types.MemberId
  
import Network.AWS.WorkMail.Types.UserId
  
import Network.AWS.WorkMail.Types.ARN
  
import Network.AWS.WorkMail.Types.Alias
  
import Network.AWS.WorkMail.Types.ResourceARN
  
import Network.AWS.WorkMail.Types.ErrorInfo
  

-- | API version @2017-10-01@ of the Amazon WorkMail SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "WorkMail",
                 Core._svcSigner = Sign.v4, Core._svcPrefix = "workmail",
                 Core._svcVersion = "2017-10-01", Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseJSONError "WorkMail",
                 Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig}
  where retry
          = Core.Exponential{Core._retryBase = 5.0e-2, Core._retryGrowth = 2,
                             Core._retryAttempts = 5, Core._retryCheck = check}
        check e
          | Lens.has
              (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttled_exception"
          | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
          | Lens.has
              (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
              e
            = Core.Just "throttling_exception"
          | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e
            = Core.Just "throttling"
          | Lens.has
              (Core.hasCode "ProvisionedThroughputExceededException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "throughput_exceeded"
          | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
          | Lens.has
              (Core.hasCode "RequestThrottledException" Core..
                 Core.hasStatus 400)
              e
            = Core.Just "request_throttled_exception"
          | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
          | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
          | Lens.has (Core.hasStatus 500) e =
            Core.Just "general_server_error"
          | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
          | Core.otherwise = Core.Nothing

-- | The directory is unavailable. It might be located in another Region or deleted.
_DirectoryUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryUnavailableException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryUnavailableException"
{-# INLINEABLE _DirectoryUnavailableException #-}
{-# DEPRECATED _DirectoryUnavailableException "Use generic-lens or generic-optics instead"  #-}

-- | One or more of the input parameters don't match the service's restrictions.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException
  = Core._MatchServiceError mkServiceConfig
      "InvalidParameterException"
{-# INLINEABLE _InvalidParameterException #-}
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead"  #-}

-- | You can't perform a write operation against a read-only directory.
_UnsupportedOperationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnsupportedOperationException
  = Core._MatchServiceError mkServiceConfig
      "UnsupportedOperationException"
{-# INLINEABLE _UnsupportedOperationException #-}
{-# DEPRECATED _UnsupportedOperationException "Use generic-lens or generic-optics instead"  #-}

-- | The directory service doesn't recognize the credentials supplied by WorkMail.
_DirectoryServiceAuthenticationFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryServiceAuthenticationFailedException
  = Core._MatchServiceError mkServiceConfig
      "DirectoryServiceAuthenticationFailedException"
{-# INLINEABLE _DirectoryServiceAuthenticationFailedException #-}
{-# DEPRECATED _DirectoryServiceAuthenticationFailedException "Use generic-lens or generic-optics instead"  #-}

-- | The organization must have a valid state to perform certain operations on the organization or its members.
_OrganizationStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationStateException
  = Core._MatchServiceError mkServiceConfig
      "OrganizationStateException"
{-# INLINEABLE _OrganizationStateException #-}
{-# DEPRECATED _OrganizationStateException "Use generic-lens or generic-optics instead"  #-}

-- | You are performing an operation on a user, group, or resource that isn't in the expected state, such as trying to delete an active user.
_EntityStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityStateException
  = Core._MatchServiceError mkServiceConfig "EntityStateException"
{-# INLINEABLE _EntityStateException #-}
{-# DEPRECATED _EntityStateException "Use generic-lens or generic-optics instead"  #-}

-- | The configuration for a resource isn't valid. A resource must either be able to auto-respond to requests or have at least one delegate associated that can do so on its behalf.
_InvalidConfigurationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConfigurationException
  = Core._MatchServiceError mkServiceConfig
      "InvalidConfigurationException"
{-# INLINEABLE _InvalidConfigurationException #-}
{-# DEPRECATED _InvalidConfigurationException "Use generic-lens or generic-optics instead"  #-}

-- | The resource can have up to 50 user-applied tags.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException
  = Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# INLINEABLE _TooManyTagsException #-}
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead"  #-}

-- | After a domain has been added to the organization, it must be verified. The domain is not yet verified.
_MailDomainStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MailDomainStateException
  = Core._MatchServiceError mkServiceConfig
      "MailDomainStateException"
{-# INLINEABLE _MailDomainStateException #-}
{-# DEPRECATED _MailDomainStateException "Use generic-lens or generic-optics instead"  #-}

-- | This user, group, or resource name is not allowed in Amazon WorkMail.
_ReservedNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReservedNameException
  = Core._MatchServiceError mkServiceConfig "ReservedNameException"
{-# INLINEABLE _ReservedNameException #-}
{-# DEPRECATED _ReservedNameException "Use generic-lens or generic-optics instead"  #-}

-- | An operation received a valid organization identifier that either doesn't belong or exist in the system.
_OrganizationNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OrganizationNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "OrganizationNotFoundException"
{-# INLINEABLE _OrganizationNotFoundException #-}
{-# DEPRECATED _OrganizationNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The identifier supplied for the user, group, or resource does not exist in your organization.
_EntityNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityNotFoundException
  = Core._MatchServiceError mkServiceConfig "EntityNotFoundException"
{-# INLINEABLE _EntityNotFoundException #-}
{-# DEPRECATED _EntityNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The user, group, or resource that you're trying to register is already registered.
_EntityAlreadyRegisteredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EntityAlreadyRegisteredException
  = Core._MatchServiceError mkServiceConfig
      "EntityAlreadyRegisteredException"
{-# INLINEABLE _EntityAlreadyRegisteredException #-}
{-# DEPRECATED _EntityAlreadyRegisteredException "Use generic-lens or generic-optics instead"  #-}

-- | The directory is already in use by another WorkMail organization in the same account and Region.
_DirectoryInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryInUseException
  = Core._MatchServiceError mkServiceConfig "DirectoryInUseException"
{-# INLINEABLE _DirectoryInUseException #-}
{-# DEPRECATED _DirectoryInUseException "Use generic-lens or generic-optics instead"  #-}

-- | For an email or alias to be created in Amazon WorkMail, the included domain must be defined in the organization.
_MailDomainNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MailDomainNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "MailDomainNotFoundException"
{-# INLINEABLE _MailDomainNotFoundException #-}
{-# DEPRECATED _MailDomainNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException
  = Core._MatchServiceError mkServiceConfig
      "ResourceNotFoundException"
{-# INLINEABLE _ResourceNotFoundException #-}
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead"  #-}

-- | The email address that you're trying to assign is already created for a different user, group, or resource.
_EmailAddressInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EmailAddressInUseException
  = Core._MatchServiceError mkServiceConfig
      "EmailAddressInUseException"
{-# INLINEABLE _EmailAddressInUseException #-}
{-# DEPRECATED _EmailAddressInUseException "Use generic-lens or generic-optics instead"  #-}

-- | The user, group, or resource name isn't unique in Amazon WorkMail.
_NameAvailabilityException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NameAvailabilityException
  = Core._MatchServiceError mkServiceConfig
      "NameAvailabilityException"
{-# INLINEABLE _NameAvailabilityException #-}
{-# DEPRECATED _NameAvailabilityException "Use generic-lens or generic-optics instead"  #-}

-- | The request exceeds the limit of the resource.
_LimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_LimitExceededException
  = Core._MatchServiceError mkServiceConfig "LimitExceededException"
{-# INLINEABLE _LimitExceededException #-}
{-# DEPRECATED _LimitExceededException "Use generic-lens or generic-optics instead"  #-}

-- | The supplied password doesn't match the minimum security constraints, such as length or use of special characters.
_InvalidPasswordException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPasswordException
  = Core._MatchServiceError mkServiceConfig
      "InvalidPasswordException"
{-# INLINEABLE _InvalidPasswordException #-}
{-# DEPRECATED _InvalidPasswordException "Use generic-lens or generic-optics instead"  #-}

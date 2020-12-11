-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types
  ( -- * Service configuration
    workMailService,

    -- * Errors

    -- * AccessControlRuleEffect
    AccessControlRuleEffect (..),

    -- * EntityState
    EntityState (..),

    -- * FolderName
    FolderName (..),

    -- * MailboxExportJobState
    MailboxExportJobState (..),

    -- * MemberType
    MemberType (..),

    -- * PermissionType
    PermissionType (..),

    -- * ResourceType
    ResourceType (..),

    -- * RetentionAction
    RetentionAction (..),

    -- * UserRole
    UserRole (..),

    -- * AccessControlRule
    AccessControlRule (..),
    mkAccessControlRule,
    acrEffect,
    acrUserIds,
    acrActions,
    acrDateCreated,
    acrName,
    acrNotUserIds,
    acrDateModified,
    acrIPRanges,
    acrNotIPRanges,
    acrNotActions,
    acrDescription,

    -- * BookingOptions
    BookingOptions (..),
    mkBookingOptions,
    boAutoDeclineConflictingRequests,
    boAutoDeclineRecurringRequests,
    boAutoAcceptRequests,

    -- * Delegate
    Delegate (..),
    mkDelegate,
    dId,
    dType,

    -- * Domain
    Domain (..),
    mkDomain,
    dHostedZoneId,
    dDomainName,

    -- * FolderConfiguration
    FolderConfiguration (..),
    mkFolderConfiguration,
    fcPeriod,
    fcName,
    fcAction,

    -- * Group
    Group (..),
    mkGroup,
    gEmail,
    gState,
    gDisabledDate,
    gName,
    gId,
    gEnabledDate,

    -- * MailboxExportJob
    MailboxExportJob (..),
    mkMailboxExportJob,
    mejState,
    mejJobId,
    mejStartTime,
    mejEstimatedProgress,
    mejEndTime,
    mejS3Path,
    mejEntityId,
    mejDescription,
    mejS3BucketName,

    -- * Member
    Member (..),
    mkMember,
    mState,
    mDisabledDate,
    mName,
    mId,
    mType,
    mEnabledDate,

    -- * OrganizationSummary
    OrganizationSummary (..),
    mkOrganizationSummary,
    osState,
    osAlias,
    osDefaultMailDomain,
    osErrorMessage,
    osOrganizationId,

    -- * Permission
    Permission (..),
    mkPermission,
    pGranteeId,
    pGranteeType,
    pPermissionValues,

    -- * Resource
    Resource (..),
    mkResource,
    rEmail,
    rState,
    rDisabledDate,
    rName,
    rId,
    rType,
    rEnabledDate,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * User
    User (..),
    mkUser,
    uEmail,
    uState,
    uDisabledDate,
    uName,
    uId,
    uDisplayName,
    uUserRole,
    uEnabledDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign
import Network.AWS.WorkMail.Types.AccessControlRule
import Network.AWS.WorkMail.Types.AccessControlRuleEffect
import Network.AWS.WorkMail.Types.BookingOptions
import Network.AWS.WorkMail.Types.Delegate
import Network.AWS.WorkMail.Types.Domain
import Network.AWS.WorkMail.Types.EntityState
import Network.AWS.WorkMail.Types.FolderConfiguration
import Network.AWS.WorkMail.Types.FolderName
import Network.AWS.WorkMail.Types.Group
import Network.AWS.WorkMail.Types.MailboxExportJob
import Network.AWS.WorkMail.Types.MailboxExportJobState
import Network.AWS.WorkMail.Types.Member
import Network.AWS.WorkMail.Types.MemberType
import Network.AWS.WorkMail.Types.OrganizationSummary
import Network.AWS.WorkMail.Types.Permission
import Network.AWS.WorkMail.Types.PermissionType
import Network.AWS.WorkMail.Types.Resource
import Network.AWS.WorkMail.Types.ResourceType
import Network.AWS.WorkMail.Types.RetentionAction
import Network.AWS.WorkMail.Types.Tag
import Network.AWS.WorkMail.Types.User
import Network.AWS.WorkMail.Types.UserRole

-- | API version @2017-10-01@ of the Amazon WorkMail SDK configuration.
workMailService :: Lude.Service
workMailService =
  Lude.Service
    { Lude._svcAbbrev = "WorkMail",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "workmail",
      Lude._svcVersion = "2017-10-01",
      Lude._svcEndpoint = Lude.defaultEndpoint workMailService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "WorkMail",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

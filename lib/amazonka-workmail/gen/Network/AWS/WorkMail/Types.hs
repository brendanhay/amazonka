{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types
  ( -- * Service Configuration
    workMail,

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
    AccessControlRule,
    accessControlRule,
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
    BookingOptions,
    bookingOptions,
    boAutoDeclineConflictingRequests,
    boAutoDeclineRecurringRequests,
    boAutoAcceptRequests,

    -- * Delegate
    Delegate,
    delegate,
    dId,
    dType,

    -- * Domain
    Domain,
    domain,
    dHostedZoneId,
    dDomainName,

    -- * FolderConfiguration
    FolderConfiguration,
    folderConfiguration,
    fcPeriod,
    fcName,
    fcAction,

    -- * Group
    Group,
    group',
    gEmail,
    gState,
    gDisabledDate,
    gName,
    gId,
    gEnabledDate,

    -- * MailboxExportJob
    MailboxExportJob,
    mailboxExportJob,
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
    Member,
    member,
    mState,
    mDisabledDate,
    mName,
    mId,
    mType,
    mEnabledDate,

    -- * OrganizationSummary
    OrganizationSummary,
    organizationSummary,
    osState,
    osAlias,
    osDefaultMailDomain,
    osErrorMessage,
    osOrganizationId,

    -- * Permission
    Permission,
    permission,
    pGranteeId,
    pGranteeType,
    pPermissionValues,

    -- * Resource
    Resource,
    resource,
    rEmail,
    rState,
    rDisabledDate,
    rName,
    rId,
    rType,
    rEnabledDate,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * User
    User,
    user,
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
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
workMail :: Service
workMail =
  Service
    { _svcAbbrev = "WorkMail",
      _svcSigner = v4,
      _svcPrefix = "workmail",
      _svcVersion = "2017-10-01",
      _svcEndpoint = defaultEndpoint workMail,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "WorkMail",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

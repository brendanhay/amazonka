{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types
    (
    -- * Service Configuration
      codeStar

    -- * Errors
    , _TeamMemberAlreadyAssociatedException
    , _ValidationException
    , _InvalidServiceRoleException
    , _ProjectCreationFailedException
    , _UserProfileAlreadyExistsException
    , _ProjectNotFoundException
    , _TeamMemberNotFoundException
    , _ProjectAlreadyExistsException
    , _ProjectConfigurationException
    , _ConcurrentModificationException
    , _InvalidNextTokenException
    , _UserProfileNotFoundException
    , _LimitExceededException

    -- * ProjectSummary
    , ProjectSummary
    , projectSummary
    , psProjectARN
    , psProjectId

    -- * Resource
    , Resource
    , resource
    , rId

    -- * TeamMember
    , TeamMember
    , teamMember
    , tmRemoteAccessAllowed
    , tmUserARN
    , tmProjectRole

    -- * UserProfileSummary
    , UserProfileSummary
    , userProfileSummary
    , upsSshPublicKey
    , upsUserARN
    , upsEmailAddress
    , upsDisplayName
    ) where

import Network.AWS.CodeStar.Types.Product
import Network.AWS.CodeStar.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-04-19@ of the Amazon CodeStar SDK configuration.
codeStar :: Service
codeStar =
  Service
    { _svcAbbrev = "CodeStar"
    , _svcSigner = v4
    , _svcPrefix = "codestar"
    , _svcVersion = "2017-04-19"
    , _svcEndpoint = defaultEndpoint codeStar
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CodeStar"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The team member is already associated with a role in this project.
--
--
_TeamMemberAlreadyAssociatedException :: AsError a => Getting (First ServiceError) a ServiceError
_TeamMemberAlreadyAssociatedException =
  _MatchServiceError codeStar "TeamMemberAlreadyAssociatedException"


-- | The specified input is either not valid, or it could not be validated.
--
--
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException = _MatchServiceError codeStar "ValidationException"


-- | The service role is not valid.
--
--
_InvalidServiceRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidServiceRoleException =
  _MatchServiceError codeStar "InvalidServiceRoleException"


-- | The project creation request was valid, but a nonspecific exception or error occurred during project creation. The project could not be created in AWS CodeStar.
--
--
_ProjectCreationFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_ProjectCreationFailedException =
  _MatchServiceError codeStar "ProjectCreationFailedException"


-- | A user profile with that name already exists in this region for the AWS account. AWS CodeStar user profile names must be unique within a region for the AWS account.
--
--
_UserProfileAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_UserProfileAlreadyExistsException =
  _MatchServiceError codeStar "UserProfileAlreadyExistsException"


-- | The specified AWS CodeStar project was not found.
--
--
_ProjectNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ProjectNotFoundException =
  _MatchServiceError codeStar "ProjectNotFoundException"


-- | The specified team member was not found.
--
--
_TeamMemberNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TeamMemberNotFoundException =
  _MatchServiceError codeStar "TeamMemberNotFoundException"


-- | An AWS CodeStar project with the same ID already exists in this region for the AWS account. AWS CodeStar project IDs must be unique within a region for the AWS account.
--
--
_ProjectAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ProjectAlreadyExistsException =
  _MatchServiceError codeStar "ProjectAlreadyExistsException"


-- | Project configuration information is required but not specified.
--
--
_ProjectConfigurationException :: AsError a => Getting (First ServiceError) a ServiceError
_ProjectConfigurationException =
  _MatchServiceError codeStar "ProjectConfigurationException"


-- | Another modification is being made. That modification must complete before you can make your change.
--
--
_ConcurrentModificationException :: AsError a => Getting (First ServiceError) a ServiceError
_ConcurrentModificationException =
  _MatchServiceError codeStar "ConcurrentModificationException"


-- | The next token is not valid.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError codeStar "InvalidNextTokenException"


-- | The user profile was not found.
--
--
_UserProfileNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_UserProfileNotFoundException =
  _MatchServiceError codeStar "UserProfileNotFoundException"


-- | A resource limit has been exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException = _MatchServiceError codeStar "LimitExceededException"


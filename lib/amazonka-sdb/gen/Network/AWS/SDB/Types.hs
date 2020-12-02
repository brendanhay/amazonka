{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types
    (
    -- * Service Configuration
      sdb

    -- * Errors
    , _InvalidNumberValueTests
    , _NoSuchDomain
    , _NumberSubmittedItemsExceeded
    , _AttributeDoesNotExist
    , _NumberDomainAttributesExceeded
    , _DuplicateItemName
    , _MissingParameter
    , _InvalidNextToken
    , _InvalidParameterValue
    , _NumberItemAttributesExceeded
    , _RequestTimeout
    , _TooManyRequestedAttributes
    , _InvalidNumberPredicates
    , _NumberDomainsExceeded
    , _NumberSubmittedAttributesExceeded
    , _NumberDomainBytesExceeded
    , _InvalidQueryExpression

    -- * Attribute
    , Attribute
    , attribute
    , aAlternateValueEncoding
    , aAlternateNameEncoding
    , aName
    , aValue

    -- * DeletableItem
    , DeletableItem
    , deletableItem
    , diAttributes
    , diName

    -- * Item
    , Item
    , item
    , iAlternateNameEncoding
    , iName
    , iAttributes

    -- * ReplaceableAttribute
    , ReplaceableAttribute
    , replaceableAttribute
    , raReplace
    , raName
    , raValue

    -- * ReplaceableItem
    , ReplaceableItem
    , replaceableItem
    , riName
    , riAttributes

    -- * UpdateCondition
    , UpdateCondition
    , updateCondition
    , ucExists
    , ucValue
    , ucName
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SDB.Types.Product
import Network.AWS.SDB.Types.Sum
import Network.AWS.Sign.V2

-- | API version @2009-04-15@ of the Amazon SimpleDB SDK configuration.
sdb :: Service
sdb =
  Service
    { _svcAbbrev = "SDB"
    , _svcSigner = v2
    , _svcPrefix = "sdb"
    , _svcVersion = "2009-04-15"
    , _svcEndpoint = defaultEndpoint sdb
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "SDB"
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


-- | Too many predicates exist in the query expression.
--
--
_InvalidNumberValueTests :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNumberValueTests =
  _MatchServiceError sdb "InvalidNumberValueTests" . hasStatus 400


-- | The specified domain does not exist.
--
--
_NoSuchDomain :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDomain = _MatchServiceError sdb "NoSuchDomain" . hasStatus 400


-- | Too many items exist in a single call.
--
--
_NumberSubmittedItemsExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberSubmittedItemsExceeded =
  _MatchServiceError sdb "NumberSubmittedItemsExceeded" . hasStatus 409


-- | The specified attribute does not exist.
--
--
_AttributeDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_AttributeDoesNotExist =
  _MatchServiceError sdb "AttributeDoesNotExist" . hasStatus 404


-- | Too many attributes in this domain.
--
--
_NumberDomainAttributesExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberDomainAttributesExceeded =
  _MatchServiceError sdb "NumberDomainAttributesExceeded" . hasStatus 409


-- | The item name was specified more than once.
--
--
_DuplicateItemName :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateItemName = _MatchServiceError sdb "DuplicateItemName" . hasStatus 400


-- | The request must contain the specified missing parameter.
--
--
_MissingParameter :: AsError a => Getting (First ServiceError) a ServiceError
_MissingParameter = _MatchServiceError sdb "MissingParameter" . hasStatus 400


-- | The specified NextToken is not valid.
--
--
_InvalidNextToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _MatchServiceError sdb "InvalidNextToken" . hasStatus 400


-- | The value for a parameter is invalid.
--
--
_InvalidParameterValue :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValue =
  _MatchServiceError sdb "InvalidParameterValue" . hasStatus 400


-- | Too many attributes in this item.
--
--
_NumberItemAttributesExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberItemAttributesExceeded =
  _MatchServiceError sdb "NumberItemAttributesExceeded" . hasStatus 409


-- | A timeout occurred when attempting to query the specified domain with specified query expression.
--
--
_RequestTimeout :: AsError a => Getting (First ServiceError) a ServiceError
_RequestTimeout = _MatchServiceError sdb "RequestTimeout" . hasStatus 408


-- | Too many attributes requested.
--
--
_TooManyRequestedAttributes :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestedAttributes =
  _MatchServiceError sdb "TooManyRequestedAttributes" . hasStatus 400


-- | Too many predicates exist in the query expression.
--
--
_InvalidNumberPredicates :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNumberPredicates =
  _MatchServiceError sdb "InvalidNumberPredicates" . hasStatus 400


-- | Too many domains exist per this account.
--
--
_NumberDomainsExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberDomainsExceeded =
  _MatchServiceError sdb "NumberDomainsExceeded" . hasStatus 409


-- | Too many attributes exist in a single call.
--
--
_NumberSubmittedAttributesExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberSubmittedAttributesExceeded =
  _MatchServiceError sdb "NumberSubmittedAttributesExceeded" . hasStatus 409


-- | Too many bytes in this domain.
--
--
_NumberDomainBytesExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberDomainBytesExceeded =
  _MatchServiceError sdb "NumberDomainBytesExceeded" . hasStatus 409


-- | The specified query expression syntax is not valid.
--
--
_InvalidQueryExpression :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidQueryExpression =
  _MatchServiceError sdb "InvalidQueryExpression" . hasStatus 400


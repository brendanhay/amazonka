{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types
    (
    -- * Service Configuration
      sDB

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

import           Network.AWS.Prelude
import           Network.AWS.SDB.Types.Product
import           Network.AWS.SDB.Types.Sum
import           Network.AWS.Sign.V2

-- | API version '2009-04-15' of the Amazon SimpleDB SDK configuration.
sDB :: Service
sDB =
    Service
    { _svcAbbrev = "SDB"
    , _svcSigner = v2
    , _svcPrefix = "sdb"
    , _svcVersion = "2009-04-15"
    , _svcEndpoint = defaultEndpoint sDB
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError
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
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | Too many predicates exist in the query expression.
_InvalidNumberValueTests :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNumberValueTests =
    _ServiceError . hasStatus 400 . hasCode "InvalidNumberValueTests"

-- | The specified domain does not exist.
_NoSuchDomain :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDomain = _ServiceError . hasStatus 400 . hasCode "NoSuchDomain"

-- | Too many items exist in a single call.
_NumberSubmittedItemsExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberSubmittedItemsExceeded =
    _ServiceError . hasStatus 409 . hasCode "NumberSubmittedItemsExceeded"

-- | The specified attribute does not exist.
_AttributeDoesNotExist :: AsError a => Getting (First ServiceError) a ServiceError
_AttributeDoesNotExist =
    _ServiceError . hasStatus 404 . hasCode "AttributeDoesNotExist"

-- | Too many attributes in this domain.
_NumberDomainAttributesExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberDomainAttributesExceeded =
    _ServiceError . hasStatus 409 . hasCode "NumberDomainAttributesExceeded"

-- | The item name was specified more than once.
_DuplicateItemName :: AsError a => Getting (First ServiceError) a ServiceError
_DuplicateItemName =
    _ServiceError . hasStatus 400 . hasCode "DuplicateItemName"

-- | The request must contain the specified missing parameter.
_MissingParameter :: AsError a => Getting (First ServiceError) a ServiceError
_MissingParameter = _ServiceError . hasStatus 400 . hasCode "MissingParameter"

-- | The specified NextToken is not valid.
_InvalidNextToken :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextToken = _ServiceError . hasStatus 400 . hasCode "InvalidNextToken"

-- | The value for a parameter is invalid.
_InvalidParameterValue :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValue =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValue"

-- | Too many attributes in this item.
_NumberItemAttributesExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberItemAttributesExceeded =
    _ServiceError . hasStatus 409 . hasCode "NumberItemAttributesExceeded"

-- | A timeout occurred when attempting to query the specified domain with
-- specified query expression.
_RequestTimeout :: AsError a => Getting (First ServiceError) a ServiceError
_RequestTimeout = _ServiceError . hasStatus 408 . hasCode "RequestTimeout"

-- | Too many attributes requested.
_TooManyRequestedAttributes :: AsError a => Getting (First ServiceError) a ServiceError
_TooManyRequestedAttributes =
    _ServiceError . hasStatus 400 . hasCode "TooManyRequestedAttributes"

-- | Too many predicates exist in the query expression.
_InvalidNumberPredicates :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNumberPredicates =
    _ServiceError . hasStatus 400 . hasCode "InvalidNumberPredicates"

-- | Too many domains exist per this account.
_NumberDomainsExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberDomainsExceeded =
    _ServiceError . hasStatus 409 . hasCode "NumberDomainsExceeded"

-- | Too many attributes exist in a single call.
_NumberSubmittedAttributesExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberSubmittedAttributesExceeded =
    _ServiceError . hasStatus 409 . hasCode "NumberSubmittedAttributesExceeded"

-- | Too many bytes in this domain.
_NumberDomainBytesExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_NumberDomainBytesExceeded =
    _ServiceError . hasStatus 409 . hasCode "NumberDomainBytesExceeded"

-- | The specified query expression syntax is not valid.
_InvalidQueryExpression :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidQueryExpression =
    _ServiceError . hasStatus 400 . hasCode "InvalidQueryExpression"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NumberDomainAttributesExceeded,
    _NumberSubmittedItemsExceeded,
    _NoSuchDomain,
    _NumberDomainBytesExceeded,
    _NumberDomainsExceeded,
    _InvalidNumberPredicates,
    _DuplicateItemName,
    _AttributeDoesNotExist,
    _InvalidNumberValueTests,
    _InvalidQueryExpression,
    _NumberSubmittedAttributesExceeded,
    _InvalidParameterValue,
    _TooManyRequestedAttributes,
    _MissingParameter,
    _InvalidNextToken,
    _RequestTimeout,
    _NumberItemAttributesExceeded,

    -- * Attribute
    Attribute (..),
    newAttribute,
    attribute_alternateNameEncoding,
    attribute_alternateValueEncoding,
    attribute_name,
    attribute_value,

    -- * DeletableItem
    DeletableItem (..),
    newDeletableItem,
    deletableItem_attributes,
    deletableItem_name,

    -- * Item
    Item (..),
    newItem,
    item_alternateNameEncoding,
    item_name,
    item_attributes,

    -- * ReplaceableAttribute
    ReplaceableAttribute (..),
    newReplaceableAttribute,
    replaceableAttribute_replace,
    replaceableAttribute_name,
    replaceableAttribute_value,

    -- * ReplaceableItem
    ReplaceableItem (..),
    newReplaceableItem,
    replaceableItem_name,
    replaceableItem_attributes,

    -- * UpdateCondition
    UpdateCondition (..),
    newUpdateCondition,
    updateCondition_exists,
    updateCondition_name,
    updateCondition_value,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SDB.Types.Attribute
import Network.AWS.SDB.Types.DeletableItem
import Network.AWS.SDB.Types.Item
import Network.AWS.SDB.Types.ReplaceableAttribute
import Network.AWS.SDB.Types.ReplaceableItem
import Network.AWS.SDB.Types.UpdateCondition
import qualified Network.AWS.Sign.V2 as Sign

-- | API version @2009-04-15@ of the Amazon SimpleDB SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "SDB",
      Prelude._svcSigner = Sign.v2,
      Prelude._svcPrefix = "sdb",
      Prelude._svcVersion = "2009-04-15",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseXMLError "SDB",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Too many attributes in this domain.
_NumberDomainAttributesExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberDomainAttributesExceeded =
  Prelude._MatchServiceError
    defaultService
    "NumberDomainAttributesExceeded"
    Prelude.. Prelude.hasStatus 409

-- | Too many items exist in a single call.
_NumberSubmittedItemsExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberSubmittedItemsExceeded =
  Prelude._MatchServiceError
    defaultService
    "NumberSubmittedItemsExceeded"
    Prelude.. Prelude.hasStatus 409

-- | The specified domain does not exist.
_NoSuchDomain :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoSuchDomain =
  Prelude._MatchServiceError
    defaultService
    "NoSuchDomain"
    Prelude.. Prelude.hasStatus 400

-- | Too many bytes in this domain.
_NumberDomainBytesExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberDomainBytesExceeded =
  Prelude._MatchServiceError
    defaultService
    "NumberDomainBytesExceeded"
    Prelude.. Prelude.hasStatus 409

-- | Too many domains exist per this account.
_NumberDomainsExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberDomainsExceeded =
  Prelude._MatchServiceError
    defaultService
    "NumberDomainsExceeded"
    Prelude.. Prelude.hasStatus 409

-- | Too many predicates exist in the query expression.
_InvalidNumberPredicates :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNumberPredicates =
  Prelude._MatchServiceError
    defaultService
    "InvalidNumberPredicates"
    Prelude.. Prelude.hasStatus 400

-- | The item name was specified more than once.
_DuplicateItemName :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DuplicateItemName =
  Prelude._MatchServiceError
    defaultService
    "DuplicateItemName"
    Prelude.. Prelude.hasStatus 400

-- | The specified attribute does not exist.
_AttributeDoesNotExist :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AttributeDoesNotExist =
  Prelude._MatchServiceError
    defaultService
    "AttributeDoesNotExist"
    Prelude.. Prelude.hasStatus 404

-- | Too many predicates exist in the query expression.
_InvalidNumberValueTests :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNumberValueTests =
  Prelude._MatchServiceError
    defaultService
    "InvalidNumberValueTests"
    Prelude.. Prelude.hasStatus 400

-- | The specified query expression syntax is not valid.
_InvalidQueryExpression :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidQueryExpression =
  Prelude._MatchServiceError
    defaultService
    "InvalidQueryExpression"
    Prelude.. Prelude.hasStatus 400

-- | Too many attributes exist in a single call.
_NumberSubmittedAttributesExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberSubmittedAttributesExceeded =
  Prelude._MatchServiceError
    defaultService
    "NumberSubmittedAttributesExceeded"
    Prelude.. Prelude.hasStatus 409

-- | The value for a parameter is invalid.
_InvalidParameterValue :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterValue =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterValue"
    Prelude.. Prelude.hasStatus 400

-- | Too many attributes requested.
_TooManyRequestedAttributes :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestedAttributes =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestedAttributes"
    Prelude.. Prelude.hasStatus 400

-- | The request must contain the specified missing parameter.
_MissingParameter :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MissingParameter =
  Prelude._MatchServiceError
    defaultService
    "MissingParameter"
    Prelude.. Prelude.hasStatus 400

-- | The specified NextToken is not valid.
_InvalidNextToken :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidNextToken =
  Prelude._MatchServiceError
    defaultService
    "InvalidNextToken"
    Prelude.. Prelude.hasStatus 400

-- | A timeout occurred when attempting to query the specified domain with
-- specified query expression.
_RequestTimeout :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RequestTimeout =
  Prelude._MatchServiceError
    defaultService
    "RequestTimeout"
    Prelude.. Prelude.hasStatus 408

-- | Too many attributes in this item.
_NumberItemAttributesExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberItemAttributesExceeded =
  Prelude._MatchServiceError
    defaultService
    "NumberItemAttributesExceeded"
    Prelude.. Prelude.hasStatus 409

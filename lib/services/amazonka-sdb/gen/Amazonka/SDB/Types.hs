{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SDB.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SDB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AttributeDoesNotExist,
    _DuplicateItemName,
    _InvalidNextToken,
    _InvalidNumberPredicates,
    _InvalidNumberValueTests,
    _InvalidParameterValue,
    _InvalidQueryExpression,
    _MissingParameter,
    _NoSuchDomain,
    _NumberDomainAttributesExceeded,
    _NumberDomainBytesExceeded,
    _NumberDomainsExceeded,
    _NumberItemAttributesExceeded,
    _NumberSubmittedAttributesExceeded,
    _NumberSubmittedItemsExceeded,
    _RequestTimeout,
    _TooManyRequestedAttributes,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SDB.Types.Attribute
import Amazonka.SDB.Types.DeletableItem
import Amazonka.SDB.Types.Item
import Amazonka.SDB.Types.ReplaceableAttribute
import Amazonka.SDB.Types.ReplaceableItem
import Amazonka.SDB.Types.UpdateCondition
import qualified Amazonka.Sign.V2 as Sign

-- | API version @2009-04-15@ of the Amazon SimpleDB SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SDB",
      Core.signer = Sign.v2,
      Core.endpointPrefix = "sdb",
      Core.signingName = "sdb",
      Core.version = "2009-04-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "SDB",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified attribute does not exist.
_AttributeDoesNotExist :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AttributeDoesNotExist =
  Core._MatchServiceError
    defaultService
    "AttributeDoesNotExist"
    Prelude.. Core.hasStatus 404

-- | The item name was specified more than once.
_DuplicateItemName :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateItemName =
  Core._MatchServiceError
    defaultService
    "DuplicateItemName"
    Prelude.. Core.hasStatus 400

-- | The specified NextToken is not valid.
_InvalidNextToken :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken =
  Core._MatchServiceError
    defaultService
    "InvalidNextToken"
    Prelude.. Core.hasStatus 400

-- | Too many predicates exist in the query expression.
_InvalidNumberPredicates :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNumberPredicates =
  Core._MatchServiceError
    defaultService
    "InvalidNumberPredicates"
    Prelude.. Core.hasStatus 400

-- | Too many predicates exist in the query expression.
_InvalidNumberValueTests :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNumberValueTests =
  Core._MatchServiceError
    defaultService
    "InvalidNumberValueTests"
    Prelude.. Core.hasStatus 400

-- | The value for a parameter is invalid.
_InvalidParameterValue :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValue =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValue"
    Prelude.. Core.hasStatus 400

-- | The specified query expression syntax is not valid.
_InvalidQueryExpression :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidQueryExpression =
  Core._MatchServiceError
    defaultService
    "InvalidQueryExpression"
    Prelude.. Core.hasStatus 400

-- | The request must contain the specified missing parameter.
_MissingParameter :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingParameter =
  Core._MatchServiceError
    defaultService
    "MissingParameter"
    Prelude.. Core.hasStatus 400

-- | The specified domain does not exist.
_NoSuchDomain :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoSuchDomain =
  Core._MatchServiceError
    defaultService
    "NoSuchDomain"
    Prelude.. Core.hasStatus 400

-- | Too many attributes in this domain.
_NumberDomainAttributesExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberDomainAttributesExceeded =
  Core._MatchServiceError
    defaultService
    "NumberDomainAttributesExceeded"
    Prelude.. Core.hasStatus 409

-- | Too many bytes in this domain.
_NumberDomainBytesExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberDomainBytesExceeded =
  Core._MatchServiceError
    defaultService
    "NumberDomainBytesExceeded"
    Prelude.. Core.hasStatus 409

-- | Too many domains exist per this account.
_NumberDomainsExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberDomainsExceeded =
  Core._MatchServiceError
    defaultService
    "NumberDomainsExceeded"
    Prelude.. Core.hasStatus 409

-- | Too many attributes in this item.
_NumberItemAttributesExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberItemAttributesExceeded =
  Core._MatchServiceError
    defaultService
    "NumberItemAttributesExceeded"
    Prelude.. Core.hasStatus 409

-- | Too many attributes exist in a single call.
_NumberSubmittedAttributesExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberSubmittedAttributesExceeded =
  Core._MatchServiceError
    defaultService
    "NumberSubmittedAttributesExceeded"
    Prelude.. Core.hasStatus 409

-- | Too many items exist in a single call.
_NumberSubmittedItemsExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberSubmittedItemsExceeded =
  Core._MatchServiceError
    defaultService
    "NumberSubmittedItemsExceeded"
    Prelude.. Core.hasStatus 409

-- | A timeout occurred when attempting to query the specified domain with
-- specified query expression.
_RequestTimeout :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RequestTimeout =
  Core._MatchServiceError
    defaultService
    "RequestTimeout"
    Prelude.. Core.hasStatus 408

-- | Too many attributes requested.
_TooManyRequestedAttributes :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestedAttributes =
  Core._MatchServiceError
    defaultService
    "TooManyRequestedAttributes"
    Prelude.. Core.hasStatus 400

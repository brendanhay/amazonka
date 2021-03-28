-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SDB.Types
    (
    -- * Service configuration
      mkServiceConfig

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
    , Attribute (..)
    , mkAttribute
    , aName
    , aValue
    , aAlternateNameEncoding
    , aAlternateValueEncoding

    -- * DeletableItem
    , DeletableItem (..)
    , mkDeletableItem
    , diName
    , diAttributes

    -- * ReplaceableItem
    , ReplaceableItem (..)
    , mkReplaceableItem
    , riName
    , riAttributes

    -- * UpdateCondition
    , UpdateCondition (..)
    , mkUpdateCondition
    , ucExists
    , ucName
    , ucValue

    -- * ReplaceableAttribute
    , ReplaceableAttribute (..)
    , mkReplaceableAttribute
    , raName
    , raValue
    , raReplace

    -- * Item
    , Item (..)
    , mkItem
    , iName
    , iAttributes
    , iAlternateNameEncoding
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V2 as Sign
  
import Network.AWS.SDB.Types.Attribute
  
  
  
  
  
  
  
  
  
import Network.AWS.SDB.Types.DeletableItem
  
import Network.AWS.SDB.Types.ReplaceableItem
  
import Network.AWS.SDB.Types.UpdateCondition
  
  
import Network.AWS.SDB.Types.ReplaceableAttribute
  
  
import Network.AWS.SDB.Types.Item
  
  
  
  
  
  
  

-- | API version @2009-04-15@ of the Amazon SimpleDB SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig
  = Core.Service{Core._svcAbbrev = "SDB", Core._svcSigner = Sign.v2,
                 Core._svcPrefix = "sdb", Core._svcVersion = "2009-04-15",
                 Core._svcTimeout = Core.Just 70,
                 Core._svcCheck = Core.statusSuccess, Core._svcRetry = retry,
                 Core._svcError = Core.parseXMLError "SDB",
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

-- | Too many predicates exist in the query expression.
_InvalidNumberValueTests :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNumberValueTests
  = Core._MatchServiceError mkServiceConfig "InvalidNumberValueTests"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidNumberValueTests #-}
{-# DEPRECATED _InvalidNumberValueTests "Use generic-lens or generic-optics instead"  #-}

-- | The specified domain does not exist.
_NoSuchDomain :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoSuchDomain
  = Core._MatchServiceError mkServiceConfig "NoSuchDomain" Core..
      Core.hasStatues 400
{-# INLINEABLE _NoSuchDomain #-}
{-# DEPRECATED _NoSuchDomain "Use generic-lens or generic-optics instead"  #-}

-- | Too many items exist in a single call.
_NumberSubmittedItemsExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberSubmittedItemsExceeded
  = Core._MatchServiceError mkServiceConfig
      "NumberSubmittedItemsExceeded"
      Core.. Core.hasStatues 409
{-# INLINEABLE _NumberSubmittedItemsExceeded #-}
{-# DEPRECATED _NumberSubmittedItemsExceeded "Use generic-lens or generic-optics instead"  #-}

-- | The specified attribute does not exist.
_AttributeDoesNotExist :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AttributeDoesNotExist
  = Core._MatchServiceError mkServiceConfig "AttributeDoesNotExist"
      Core.. Core.hasStatues 404
{-# INLINEABLE _AttributeDoesNotExist #-}
{-# DEPRECATED _AttributeDoesNotExist "Use generic-lens or generic-optics instead"  #-}

-- | Too many attributes in this domain.
_NumberDomainAttributesExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberDomainAttributesExceeded
  = Core._MatchServiceError mkServiceConfig
      "NumberDomainAttributesExceeded"
      Core.. Core.hasStatues 409
{-# INLINEABLE _NumberDomainAttributesExceeded #-}
{-# DEPRECATED _NumberDomainAttributesExceeded "Use generic-lens or generic-optics instead"  #-}

-- | The item name was specified more than once. 
_DuplicateItemName :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DuplicateItemName
  = Core._MatchServiceError mkServiceConfig "DuplicateItemName"
      Core.. Core.hasStatues 400
{-# INLINEABLE _DuplicateItemName #-}
{-# DEPRECATED _DuplicateItemName "Use generic-lens or generic-optics instead"  #-}

-- | The request must contain the specified missing parameter.
_MissingParameter :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MissingParameter
  = Core._MatchServiceError mkServiceConfig "MissingParameter" Core..
      Core.hasStatues 400
{-# INLINEABLE _MissingParameter #-}
{-# DEPRECATED _MissingParameter "Use generic-lens or generic-optics instead"  #-}

-- | The specified NextToken is not valid. 
_InvalidNextToken :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNextToken
  = Core._MatchServiceError mkServiceConfig "InvalidNextToken" Core..
      Core.hasStatues 400
{-# INLINEABLE _InvalidNextToken #-}
{-# DEPRECATED _InvalidNextToken "Use generic-lens or generic-optics instead"  #-}

-- | The value for a parameter is invalid.
_InvalidParameterValue :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValue
  = Core._MatchServiceError mkServiceConfig "InvalidParameterValue"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidParameterValue #-}
{-# DEPRECATED _InvalidParameterValue "Use generic-lens or generic-optics instead"  #-}

-- | Too many attributes in this item.
_NumberItemAttributesExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberItemAttributesExceeded
  = Core._MatchServiceError mkServiceConfig
      "NumberItemAttributesExceeded"
      Core.. Core.hasStatues 409
{-# INLINEABLE _NumberItemAttributesExceeded #-}
{-# DEPRECATED _NumberItemAttributesExceeded "Use generic-lens or generic-optics instead"  #-}

-- | A timeout occurred when attempting to query the specified domain with specified query expression.
_RequestTimeout :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RequestTimeout
  = Core._MatchServiceError mkServiceConfig "RequestTimeout" Core..
      Core.hasStatues 408
{-# INLINEABLE _RequestTimeout #-}
{-# DEPRECATED _RequestTimeout "Use generic-lens or generic-optics instead"  #-}

-- | Too many attributes requested.
_TooManyRequestedAttributes :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestedAttributes
  = Core._MatchServiceError mkServiceConfig
      "TooManyRequestedAttributes"
      Core.. Core.hasStatues 400
{-# INLINEABLE _TooManyRequestedAttributes #-}
{-# DEPRECATED _TooManyRequestedAttributes "Use generic-lens or generic-optics instead"  #-}

-- | Too many predicates exist in the query expression.
_InvalidNumberPredicates :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidNumberPredicates
  = Core._MatchServiceError mkServiceConfig "InvalidNumberPredicates"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidNumberPredicates #-}
{-# DEPRECATED _InvalidNumberPredicates "Use generic-lens or generic-optics instead"  #-}

-- | Too many domains exist per this account.
_NumberDomainsExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberDomainsExceeded
  = Core._MatchServiceError mkServiceConfig "NumberDomainsExceeded"
      Core.. Core.hasStatues 409
{-# INLINEABLE _NumberDomainsExceeded #-}
{-# DEPRECATED _NumberDomainsExceeded "Use generic-lens or generic-optics instead"  #-}

-- | Too many attributes exist in a single call.
_NumberSubmittedAttributesExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberSubmittedAttributesExceeded
  = Core._MatchServiceError mkServiceConfig
      "NumberSubmittedAttributesExceeded"
      Core.. Core.hasStatues 409
{-# INLINEABLE _NumberSubmittedAttributesExceeded #-}
{-# DEPRECATED _NumberSubmittedAttributesExceeded "Use generic-lens or generic-optics instead"  #-}

-- | Too many bytes in this domain.
_NumberDomainBytesExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberDomainBytesExceeded
  = Core._MatchServiceError mkServiceConfig
      "NumberDomainBytesExceeded"
      Core.. Core.hasStatues 409
{-# INLINEABLE _NumberDomainBytesExceeded #-}
{-# DEPRECATED _NumberDomainBytesExceeded "Use generic-lens or generic-optics instead"  #-}

-- | The specified query expression syntax is not valid.
_InvalidQueryExpression :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidQueryExpression
  = Core._MatchServiceError mkServiceConfig "InvalidQueryExpression"
      Core.. Core.hasStatues 400
{-# INLINEABLE _InvalidQueryExpression #-}
{-# DEPRECATED _InvalidQueryExpression "Use generic-lens or generic-optics instead"  #-}

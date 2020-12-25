{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionAssociationErrorCode
  ( ServiceActionAssociationErrorCode
      ( ServiceActionAssociationErrorCode',
        ServiceActionAssociationErrorCodeDuplicateResource,
        ServiceActionAssociationErrorCodeInternalFailure,
        ServiceActionAssociationErrorCodeLimitExceeded,
        ServiceActionAssociationErrorCodeResourceNotFound,
        ServiceActionAssociationErrorCodeThrottling,
        fromServiceActionAssociationErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ServiceActionAssociationErrorCode = ServiceActionAssociationErrorCode'
  { fromServiceActionAssociationErrorCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ServiceActionAssociationErrorCodeDuplicateResource :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCodeDuplicateResource = ServiceActionAssociationErrorCode' "DUPLICATE_RESOURCE"

pattern ServiceActionAssociationErrorCodeInternalFailure :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCodeInternalFailure = ServiceActionAssociationErrorCode' "INTERNAL_FAILURE"

pattern ServiceActionAssociationErrorCodeLimitExceeded :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCodeLimitExceeded = ServiceActionAssociationErrorCode' "LIMIT_EXCEEDED"

pattern ServiceActionAssociationErrorCodeResourceNotFound :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCodeResourceNotFound = ServiceActionAssociationErrorCode' "RESOURCE_NOT_FOUND"

pattern ServiceActionAssociationErrorCodeThrottling :: ServiceActionAssociationErrorCode
pattern ServiceActionAssociationErrorCodeThrottling = ServiceActionAssociationErrorCode' "THROTTLING"

{-# COMPLETE
  ServiceActionAssociationErrorCodeDuplicateResource,
  ServiceActionAssociationErrorCodeInternalFailure,
  ServiceActionAssociationErrorCodeLimitExceeded,
  ServiceActionAssociationErrorCodeResourceNotFound,
  ServiceActionAssociationErrorCodeThrottling,
  ServiceActionAssociationErrorCode'
  #-}

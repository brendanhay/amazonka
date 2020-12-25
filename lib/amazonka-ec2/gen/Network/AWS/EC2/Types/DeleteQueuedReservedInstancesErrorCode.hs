{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
  ( DeleteQueuedReservedInstancesErrorCode
      ( DeleteQueuedReservedInstancesErrorCode',
        DeleteQueuedReservedInstancesErrorCodeReservedInstancesIdInvalid,
        DeleteQueuedReservedInstancesErrorCodeReservedInstancesNotInQueuedState,
        DeleteQueuedReservedInstancesErrorCodeUnexpectedError,
        fromDeleteQueuedReservedInstancesErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DeleteQueuedReservedInstancesErrorCode = DeleteQueuedReservedInstancesErrorCode'
  { fromDeleteQueuedReservedInstancesErrorCode ::
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

pattern DeleteQueuedReservedInstancesErrorCodeReservedInstancesIdInvalid :: DeleteQueuedReservedInstancesErrorCode
pattern DeleteQueuedReservedInstancesErrorCodeReservedInstancesIdInvalid = DeleteQueuedReservedInstancesErrorCode' "reserved-instances-id-invalid"

pattern DeleteQueuedReservedInstancesErrorCodeReservedInstancesNotInQueuedState :: DeleteQueuedReservedInstancesErrorCode
pattern DeleteQueuedReservedInstancesErrorCodeReservedInstancesNotInQueuedState = DeleteQueuedReservedInstancesErrorCode' "reserved-instances-not-in-queued-state"

pattern DeleteQueuedReservedInstancesErrorCodeUnexpectedError :: DeleteQueuedReservedInstancesErrorCode
pattern DeleteQueuedReservedInstancesErrorCodeUnexpectedError = DeleteQueuedReservedInstancesErrorCode' "unexpected-error"

{-# COMPLETE
  DeleteQueuedReservedInstancesErrorCodeReservedInstancesIdInvalid,
  DeleteQueuedReservedInstancesErrorCodeReservedInstancesNotInQueuedState,
  DeleteQueuedReservedInstancesErrorCodeUnexpectedError,
  DeleteQueuedReservedInstancesErrorCode'
  #-}

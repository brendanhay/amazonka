{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelBatchErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelBatchErrorCode
  ( CancelBatchErrorCode
      ( CancelBatchErrorCode',
        CancelBatchErrorCodeFleetRequestIdDoesNotExist,
        CancelBatchErrorCodeFleetRequestIdMalformed,
        CancelBatchErrorCodeFleetRequestNotInCancellableState,
        CancelBatchErrorCodeUnexpectedError,
        fromCancelBatchErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CancelBatchErrorCode = CancelBatchErrorCode'
  { fromCancelBatchErrorCode ::
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

pattern CancelBatchErrorCodeFleetRequestIdDoesNotExist :: CancelBatchErrorCode
pattern CancelBatchErrorCodeFleetRequestIdDoesNotExist = CancelBatchErrorCode' "fleetRequestIdDoesNotExist"

pattern CancelBatchErrorCodeFleetRequestIdMalformed :: CancelBatchErrorCode
pattern CancelBatchErrorCodeFleetRequestIdMalformed = CancelBatchErrorCode' "fleetRequestIdMalformed"

pattern CancelBatchErrorCodeFleetRequestNotInCancellableState :: CancelBatchErrorCode
pattern CancelBatchErrorCodeFleetRequestNotInCancellableState = CancelBatchErrorCode' "fleetRequestNotInCancellableState"

pattern CancelBatchErrorCodeUnexpectedError :: CancelBatchErrorCode
pattern CancelBatchErrorCodeUnexpectedError = CancelBatchErrorCode' "unexpectedError"

{-# COMPLETE
  CancelBatchErrorCodeFleetRequestIdDoesNotExist,
  CancelBatchErrorCodeFleetRequestIdMalformed,
  CancelBatchErrorCodeFleetRequestNotInCancellableState,
  CancelBatchErrorCodeUnexpectedError,
  CancelBatchErrorCode'
  #-}

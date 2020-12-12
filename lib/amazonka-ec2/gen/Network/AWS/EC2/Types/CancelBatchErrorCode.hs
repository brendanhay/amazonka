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
        CBECFleetRequestIdDoesNotExist,
        CBECFleetRequestIdMalformed,
        CBECFleetRequestNotInCancellableState,
        CBECUnexpectedError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CancelBatchErrorCode = CancelBatchErrorCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CBECFleetRequestIdDoesNotExist :: CancelBatchErrorCode
pattern CBECFleetRequestIdDoesNotExist = CancelBatchErrorCode' "fleetRequestIdDoesNotExist"

pattern CBECFleetRequestIdMalformed :: CancelBatchErrorCode
pattern CBECFleetRequestIdMalformed = CancelBatchErrorCode' "fleetRequestIdMalformed"

pattern CBECFleetRequestNotInCancellableState :: CancelBatchErrorCode
pattern CBECFleetRequestNotInCancellableState = CancelBatchErrorCode' "fleetRequestNotInCancellableState"

pattern CBECUnexpectedError :: CancelBatchErrorCode
pattern CBECUnexpectedError = CancelBatchErrorCode' "unexpectedError"

{-# COMPLETE
  CBECFleetRequestIdDoesNotExist,
  CBECFleetRequestIdMalformed,
  CBECFleetRequestNotInCancellableState,
  CBECUnexpectedError,
  CancelBatchErrorCode'
  #-}

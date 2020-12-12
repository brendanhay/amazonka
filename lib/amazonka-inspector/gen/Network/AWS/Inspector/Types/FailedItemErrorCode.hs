{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FailedItemErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FailedItemErrorCode
  ( FailedItemErrorCode
      ( FailedItemErrorCode',
        FIECAccessDenied,
        FIECDuplicateARN,
        FIECInternalError,
        FIECInvalidARN,
        FIECItemDoesNotExist,
        FIECLimitExceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FailedItemErrorCode = FailedItemErrorCode' Lude.Text
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

pattern FIECAccessDenied :: FailedItemErrorCode
pattern FIECAccessDenied = FailedItemErrorCode' "ACCESS_DENIED"

pattern FIECDuplicateARN :: FailedItemErrorCode
pattern FIECDuplicateARN = FailedItemErrorCode' "DUPLICATE_ARN"

pattern FIECInternalError :: FailedItemErrorCode
pattern FIECInternalError = FailedItemErrorCode' "INTERNAL_ERROR"

pattern FIECInvalidARN :: FailedItemErrorCode
pattern FIECInvalidARN = FailedItemErrorCode' "INVALID_ARN"

pattern FIECItemDoesNotExist :: FailedItemErrorCode
pattern FIECItemDoesNotExist = FailedItemErrorCode' "ITEM_DOES_NOT_EXIST"

pattern FIECLimitExceeded :: FailedItemErrorCode
pattern FIECLimitExceeded = FailedItemErrorCode' "LIMIT_EXCEEDED"

{-# COMPLETE
  FIECAccessDenied,
  FIECDuplicateARN,
  FIECInternalError,
  FIECInvalidARN,
  FIECItemDoesNotExist,
  FIECLimitExceeded,
  FailedItemErrorCode'
  #-}

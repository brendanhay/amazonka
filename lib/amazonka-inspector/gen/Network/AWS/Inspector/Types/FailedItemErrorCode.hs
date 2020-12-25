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
        FailedItemErrorCodeInvalidArn,
        FailedItemErrorCodeDuplicateArn,
        FailedItemErrorCodeItemDoesNotExist,
        FailedItemErrorCodeAccessDenied,
        FailedItemErrorCodeLimitExceeded,
        FailedItemErrorCodeInternalError,
        fromFailedItemErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FailedItemErrorCode = FailedItemErrorCode'
  { fromFailedItemErrorCode ::
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

pattern FailedItemErrorCodeInvalidArn :: FailedItemErrorCode
pattern FailedItemErrorCodeInvalidArn = FailedItemErrorCode' "INVALID_ARN"

pattern FailedItemErrorCodeDuplicateArn :: FailedItemErrorCode
pattern FailedItemErrorCodeDuplicateArn = FailedItemErrorCode' "DUPLICATE_ARN"

pattern FailedItemErrorCodeItemDoesNotExist :: FailedItemErrorCode
pattern FailedItemErrorCodeItemDoesNotExist = FailedItemErrorCode' "ITEM_DOES_NOT_EXIST"

pattern FailedItemErrorCodeAccessDenied :: FailedItemErrorCode
pattern FailedItemErrorCodeAccessDenied = FailedItemErrorCode' "ACCESS_DENIED"

pattern FailedItemErrorCodeLimitExceeded :: FailedItemErrorCode
pattern FailedItemErrorCodeLimitExceeded = FailedItemErrorCode' "LIMIT_EXCEEDED"

pattern FailedItemErrorCodeInternalError :: FailedItemErrorCode
pattern FailedItemErrorCodeInternalError = FailedItemErrorCode' "INTERNAL_ERROR"

{-# COMPLETE
  FailedItemErrorCodeInvalidArn,
  FailedItemErrorCodeDuplicateArn,
  FailedItemErrorCodeItemDoesNotExist,
  FailedItemErrorCodeAccessDenied,
  FailedItemErrorCodeLimitExceeded,
  FailedItemErrorCodeInternalError,
  FailedItemErrorCode'
  #-}

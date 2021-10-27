{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Location.Types.BatchItemErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Location.Types.BatchItemErrorCode
  ( BatchItemErrorCode
      ( ..,
        BatchItemErrorCode_AccessDeniedError,
        BatchItemErrorCode_ConflictError,
        BatchItemErrorCode_InternalServerError,
        BatchItemErrorCode_ResourceNotFoundError,
        BatchItemErrorCode_ThrottlingError,
        BatchItemErrorCode_ValidationError
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BatchItemErrorCode = BatchItemErrorCode'
  { fromBatchItemErrorCode ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern BatchItemErrorCode_AccessDeniedError :: BatchItemErrorCode
pattern BatchItemErrorCode_AccessDeniedError = BatchItemErrorCode' "AccessDeniedError"

pattern BatchItemErrorCode_ConflictError :: BatchItemErrorCode
pattern BatchItemErrorCode_ConflictError = BatchItemErrorCode' "ConflictError"

pattern BatchItemErrorCode_InternalServerError :: BatchItemErrorCode
pattern BatchItemErrorCode_InternalServerError = BatchItemErrorCode' "InternalServerError"

pattern BatchItemErrorCode_ResourceNotFoundError :: BatchItemErrorCode
pattern BatchItemErrorCode_ResourceNotFoundError = BatchItemErrorCode' "ResourceNotFoundError"

pattern BatchItemErrorCode_ThrottlingError :: BatchItemErrorCode
pattern BatchItemErrorCode_ThrottlingError = BatchItemErrorCode' "ThrottlingError"

pattern BatchItemErrorCode_ValidationError :: BatchItemErrorCode
pattern BatchItemErrorCode_ValidationError = BatchItemErrorCode' "ValidationError"

{-# COMPLETE
  BatchItemErrorCode_AccessDeniedError,
  BatchItemErrorCode_ConflictError,
  BatchItemErrorCode_InternalServerError,
  BatchItemErrorCode_ResourceNotFoundError,
  BatchItemErrorCode_ThrottlingError,
  BatchItemErrorCode_ValidationError,
  BatchItemErrorCode'
  #-}

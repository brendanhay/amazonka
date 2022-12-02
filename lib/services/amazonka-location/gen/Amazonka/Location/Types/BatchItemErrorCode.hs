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
-- Module      : Amazonka.Location.Types.BatchItemErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.BatchItemErrorCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BatchItemErrorCode = BatchItemErrorCode'
  { fromBatchItemErrorCode ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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

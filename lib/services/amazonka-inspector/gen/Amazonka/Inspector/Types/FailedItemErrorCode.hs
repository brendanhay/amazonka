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
-- Module      : Amazonka.Inspector.Types.FailedItemErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.FailedItemErrorCode
  ( FailedItemErrorCode
      ( ..,
        FailedItemErrorCode_ACCESS_DENIED,
        FailedItemErrorCode_DUPLICATE_ARN,
        FailedItemErrorCode_INTERNAL_ERROR,
        FailedItemErrorCode_INVALID_ARN,
        FailedItemErrorCode_ITEM_DOES_NOT_EXIST,
        FailedItemErrorCode_LIMIT_EXCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FailedItemErrorCode = FailedItemErrorCode'
  { fromFailedItemErrorCode ::
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

pattern FailedItemErrorCode_ACCESS_DENIED :: FailedItemErrorCode
pattern FailedItemErrorCode_ACCESS_DENIED = FailedItemErrorCode' "ACCESS_DENIED"

pattern FailedItemErrorCode_DUPLICATE_ARN :: FailedItemErrorCode
pattern FailedItemErrorCode_DUPLICATE_ARN = FailedItemErrorCode' "DUPLICATE_ARN"

pattern FailedItemErrorCode_INTERNAL_ERROR :: FailedItemErrorCode
pattern FailedItemErrorCode_INTERNAL_ERROR = FailedItemErrorCode' "INTERNAL_ERROR"

pattern FailedItemErrorCode_INVALID_ARN :: FailedItemErrorCode
pattern FailedItemErrorCode_INVALID_ARN = FailedItemErrorCode' "INVALID_ARN"

pattern FailedItemErrorCode_ITEM_DOES_NOT_EXIST :: FailedItemErrorCode
pattern FailedItemErrorCode_ITEM_DOES_NOT_EXIST = FailedItemErrorCode' "ITEM_DOES_NOT_EXIST"

pattern FailedItemErrorCode_LIMIT_EXCEEDED :: FailedItemErrorCode
pattern FailedItemErrorCode_LIMIT_EXCEEDED = FailedItemErrorCode' "LIMIT_EXCEEDED"

{-# COMPLETE
  FailedItemErrorCode_ACCESS_DENIED,
  FailedItemErrorCode_DUPLICATE_ARN,
  FailedItemErrorCode_INTERNAL_ERROR,
  FailedItemErrorCode_INVALID_ARN,
  FailedItemErrorCode_ITEM_DOES_NOT_EXIST,
  FailedItemErrorCode_LIMIT_EXCEEDED,
  FailedItemErrorCode'
  #-}

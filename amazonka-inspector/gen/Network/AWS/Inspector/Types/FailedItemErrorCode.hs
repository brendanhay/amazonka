{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FailedItemErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FailedItemErrorCode
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

import qualified Network.AWS.Prelude as Prelude

newtype FailedItemErrorCode = FailedItemErrorCode'
  { fromFailedItemErrorCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

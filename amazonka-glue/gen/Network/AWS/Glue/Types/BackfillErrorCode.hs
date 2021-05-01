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
-- Module      : Network.AWS.Glue.Types.BackfillErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BackfillErrorCode
  ( BackfillErrorCode
      ( ..,
        BackfillErrorCode_ENCRYPTED_PARTITION_ERROR,
        BackfillErrorCode_INTERNAL_ERROR,
        BackfillErrorCode_INVALID_PARTITION_TYPE_DATA_ERROR,
        BackfillErrorCode_MISSING_PARTITION_VALUE_ERROR,
        BackfillErrorCode_UNSUPPORTED_PARTITION_CHARACTER_ERROR
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype BackfillErrorCode = BackfillErrorCode'
  { fromBackfillErrorCode ::
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

pattern BackfillErrorCode_ENCRYPTED_PARTITION_ERROR :: BackfillErrorCode
pattern BackfillErrorCode_ENCRYPTED_PARTITION_ERROR = BackfillErrorCode' "ENCRYPTED_PARTITION_ERROR"

pattern BackfillErrorCode_INTERNAL_ERROR :: BackfillErrorCode
pattern BackfillErrorCode_INTERNAL_ERROR = BackfillErrorCode' "INTERNAL_ERROR"

pattern BackfillErrorCode_INVALID_PARTITION_TYPE_DATA_ERROR :: BackfillErrorCode
pattern BackfillErrorCode_INVALID_PARTITION_TYPE_DATA_ERROR = BackfillErrorCode' "INVALID_PARTITION_TYPE_DATA_ERROR"

pattern BackfillErrorCode_MISSING_PARTITION_VALUE_ERROR :: BackfillErrorCode
pattern BackfillErrorCode_MISSING_PARTITION_VALUE_ERROR = BackfillErrorCode' "MISSING_PARTITION_VALUE_ERROR"

pattern BackfillErrorCode_UNSUPPORTED_PARTITION_CHARACTER_ERROR :: BackfillErrorCode
pattern BackfillErrorCode_UNSUPPORTED_PARTITION_CHARACTER_ERROR = BackfillErrorCode' "UNSUPPORTED_PARTITION_CHARACTER_ERROR"

{-# COMPLETE
  BackfillErrorCode_ENCRYPTED_PARTITION_ERROR,
  BackfillErrorCode_INTERNAL_ERROR,
  BackfillErrorCode_INVALID_PARTITION_TYPE_DATA_ERROR,
  BackfillErrorCode_MISSING_PARTITION_VALUE_ERROR,
  BackfillErrorCode_UNSUPPORTED_PARTITION_CHARACTER_ERROR,
  BackfillErrorCode'
  #-}

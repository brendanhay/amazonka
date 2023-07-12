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
-- Module      : Amazonka.Glue.Types.BackfillErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BackfillErrorCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BackfillErrorCode = BackfillErrorCode'
  { fromBackfillErrorCode ::
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

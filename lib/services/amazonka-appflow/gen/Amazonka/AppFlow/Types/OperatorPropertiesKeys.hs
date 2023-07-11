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
-- Module      : Amazonka.AppFlow.Types.OperatorPropertiesKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.OperatorPropertiesKeys
  ( OperatorPropertiesKeys
      ( ..,
        OperatorPropertiesKeys_CONCAT_FORMAT,
        OperatorPropertiesKeys_DATA_TYPE,
        OperatorPropertiesKeys_DESTINATION_DATA_TYPE,
        OperatorPropertiesKeys_EXCLUDE_SOURCE_FIELDS_LIST,
        OperatorPropertiesKeys_INCLUDE_NEW_FIELDS,
        OperatorPropertiesKeys_LOWER_BOUND,
        OperatorPropertiesKeys_MASK_LENGTH,
        OperatorPropertiesKeys_MASK_VALUE,
        OperatorPropertiesKeys_MATH_OPERATION_FIELDS_ORDER,
        OperatorPropertiesKeys_ORDERED_PARTITION_KEYS_LIST,
        OperatorPropertiesKeys_SOURCE_DATA_TYPE,
        OperatorPropertiesKeys_SUBFIELD_CATEGORY_MAP,
        OperatorPropertiesKeys_TRUNCATE_LENGTH,
        OperatorPropertiesKeys_UPPER_BOUND,
        OperatorPropertiesKeys_VALIDATION_ACTION,
        OperatorPropertiesKeys_VALUE,
        OperatorPropertiesKeys_VALUES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperatorPropertiesKeys = OperatorPropertiesKeys'
  { fromOperatorPropertiesKeys ::
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

pattern OperatorPropertiesKeys_CONCAT_FORMAT :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_CONCAT_FORMAT = OperatorPropertiesKeys' "CONCAT_FORMAT"

pattern OperatorPropertiesKeys_DATA_TYPE :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_DATA_TYPE = OperatorPropertiesKeys' "DATA_TYPE"

pattern OperatorPropertiesKeys_DESTINATION_DATA_TYPE :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_DESTINATION_DATA_TYPE = OperatorPropertiesKeys' "DESTINATION_DATA_TYPE"

pattern OperatorPropertiesKeys_EXCLUDE_SOURCE_FIELDS_LIST :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_EXCLUDE_SOURCE_FIELDS_LIST = OperatorPropertiesKeys' "EXCLUDE_SOURCE_FIELDS_LIST"

pattern OperatorPropertiesKeys_INCLUDE_NEW_FIELDS :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_INCLUDE_NEW_FIELDS = OperatorPropertiesKeys' "INCLUDE_NEW_FIELDS"

pattern OperatorPropertiesKeys_LOWER_BOUND :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_LOWER_BOUND = OperatorPropertiesKeys' "LOWER_BOUND"

pattern OperatorPropertiesKeys_MASK_LENGTH :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_MASK_LENGTH = OperatorPropertiesKeys' "MASK_LENGTH"

pattern OperatorPropertiesKeys_MASK_VALUE :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_MASK_VALUE = OperatorPropertiesKeys' "MASK_VALUE"

pattern OperatorPropertiesKeys_MATH_OPERATION_FIELDS_ORDER :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_MATH_OPERATION_FIELDS_ORDER = OperatorPropertiesKeys' "MATH_OPERATION_FIELDS_ORDER"

pattern OperatorPropertiesKeys_ORDERED_PARTITION_KEYS_LIST :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_ORDERED_PARTITION_KEYS_LIST = OperatorPropertiesKeys' "ORDERED_PARTITION_KEYS_LIST"

pattern OperatorPropertiesKeys_SOURCE_DATA_TYPE :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_SOURCE_DATA_TYPE = OperatorPropertiesKeys' "SOURCE_DATA_TYPE"

pattern OperatorPropertiesKeys_SUBFIELD_CATEGORY_MAP :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_SUBFIELD_CATEGORY_MAP = OperatorPropertiesKeys' "SUBFIELD_CATEGORY_MAP"

pattern OperatorPropertiesKeys_TRUNCATE_LENGTH :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_TRUNCATE_LENGTH = OperatorPropertiesKeys' "TRUNCATE_LENGTH"

pattern OperatorPropertiesKeys_UPPER_BOUND :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_UPPER_BOUND = OperatorPropertiesKeys' "UPPER_BOUND"

pattern OperatorPropertiesKeys_VALIDATION_ACTION :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_VALIDATION_ACTION = OperatorPropertiesKeys' "VALIDATION_ACTION"

pattern OperatorPropertiesKeys_VALUE :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_VALUE = OperatorPropertiesKeys' "VALUE"

pattern OperatorPropertiesKeys_VALUES :: OperatorPropertiesKeys
pattern OperatorPropertiesKeys_VALUES = OperatorPropertiesKeys' "VALUES"

{-# COMPLETE
  OperatorPropertiesKeys_CONCAT_FORMAT,
  OperatorPropertiesKeys_DATA_TYPE,
  OperatorPropertiesKeys_DESTINATION_DATA_TYPE,
  OperatorPropertiesKeys_EXCLUDE_SOURCE_FIELDS_LIST,
  OperatorPropertiesKeys_INCLUDE_NEW_FIELDS,
  OperatorPropertiesKeys_LOWER_BOUND,
  OperatorPropertiesKeys_MASK_LENGTH,
  OperatorPropertiesKeys_MASK_VALUE,
  OperatorPropertiesKeys_MATH_OPERATION_FIELDS_ORDER,
  OperatorPropertiesKeys_ORDERED_PARTITION_KEYS_LIST,
  OperatorPropertiesKeys_SOURCE_DATA_TYPE,
  OperatorPropertiesKeys_SUBFIELD_CATEGORY_MAP,
  OperatorPropertiesKeys_TRUNCATE_LENGTH,
  OperatorPropertiesKeys_UPPER_BOUND,
  OperatorPropertiesKeys_VALIDATION_ACTION,
  OperatorPropertiesKeys_VALUE,
  OperatorPropertiesKeys_VALUES,
  OperatorPropertiesKeys'
  #-}

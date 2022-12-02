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
-- Module      : Amazonka.IoTFleetWise.Types.NodeDataType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.NodeDataType
  ( NodeDataType
      ( ..,
        NodeDataType_BOOLEAN,
        NodeDataType_BOOLEAN_ARRAY,
        NodeDataType_DOUBLE,
        NodeDataType_DOUBLE_ARRAY,
        NodeDataType_FLOAT,
        NodeDataType_FLOAT_ARRAY,
        NodeDataType_INT16,
        NodeDataType_INT16_ARRAY,
        NodeDataType_INT32,
        NodeDataType_INT32_ARRAY,
        NodeDataType_INT64,
        NodeDataType_INT64_ARRAY,
        NodeDataType_INT8,
        NodeDataType_INT8_ARRAY,
        NodeDataType_STRING,
        NodeDataType_STRING_ARRAY,
        NodeDataType_UINT16,
        NodeDataType_UINT16_ARRAY,
        NodeDataType_UINT32,
        NodeDataType_UINT32_ARRAY,
        NodeDataType_UINT64,
        NodeDataType_UINT64_ARRAY,
        NodeDataType_UINT8,
        NodeDataType_UINT8_ARRAY,
        NodeDataType_UNIX_TIMESTAMP,
        NodeDataType_UNIX_TIMESTAMP_ARRAY,
        NodeDataType_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NodeDataType = NodeDataType'
  { fromNodeDataType ::
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

pattern NodeDataType_BOOLEAN :: NodeDataType
pattern NodeDataType_BOOLEAN = NodeDataType' "BOOLEAN"

pattern NodeDataType_BOOLEAN_ARRAY :: NodeDataType
pattern NodeDataType_BOOLEAN_ARRAY = NodeDataType' "BOOLEAN_ARRAY"

pattern NodeDataType_DOUBLE :: NodeDataType
pattern NodeDataType_DOUBLE = NodeDataType' "DOUBLE"

pattern NodeDataType_DOUBLE_ARRAY :: NodeDataType
pattern NodeDataType_DOUBLE_ARRAY = NodeDataType' "DOUBLE_ARRAY"

pattern NodeDataType_FLOAT :: NodeDataType
pattern NodeDataType_FLOAT = NodeDataType' "FLOAT"

pattern NodeDataType_FLOAT_ARRAY :: NodeDataType
pattern NodeDataType_FLOAT_ARRAY = NodeDataType' "FLOAT_ARRAY"

pattern NodeDataType_INT16 :: NodeDataType
pattern NodeDataType_INT16 = NodeDataType' "INT16"

pattern NodeDataType_INT16_ARRAY :: NodeDataType
pattern NodeDataType_INT16_ARRAY = NodeDataType' "INT16_ARRAY"

pattern NodeDataType_INT32 :: NodeDataType
pattern NodeDataType_INT32 = NodeDataType' "INT32"

pattern NodeDataType_INT32_ARRAY :: NodeDataType
pattern NodeDataType_INT32_ARRAY = NodeDataType' "INT32_ARRAY"

pattern NodeDataType_INT64 :: NodeDataType
pattern NodeDataType_INT64 = NodeDataType' "INT64"

pattern NodeDataType_INT64_ARRAY :: NodeDataType
pattern NodeDataType_INT64_ARRAY = NodeDataType' "INT64_ARRAY"

pattern NodeDataType_INT8 :: NodeDataType
pattern NodeDataType_INT8 = NodeDataType' "INT8"

pattern NodeDataType_INT8_ARRAY :: NodeDataType
pattern NodeDataType_INT8_ARRAY = NodeDataType' "INT8_ARRAY"

pattern NodeDataType_STRING :: NodeDataType
pattern NodeDataType_STRING = NodeDataType' "STRING"

pattern NodeDataType_STRING_ARRAY :: NodeDataType
pattern NodeDataType_STRING_ARRAY = NodeDataType' "STRING_ARRAY"

pattern NodeDataType_UINT16 :: NodeDataType
pattern NodeDataType_UINT16 = NodeDataType' "UINT16"

pattern NodeDataType_UINT16_ARRAY :: NodeDataType
pattern NodeDataType_UINT16_ARRAY = NodeDataType' "UINT16_ARRAY"

pattern NodeDataType_UINT32 :: NodeDataType
pattern NodeDataType_UINT32 = NodeDataType' "UINT32"

pattern NodeDataType_UINT32_ARRAY :: NodeDataType
pattern NodeDataType_UINT32_ARRAY = NodeDataType' "UINT32_ARRAY"

pattern NodeDataType_UINT64 :: NodeDataType
pattern NodeDataType_UINT64 = NodeDataType' "UINT64"

pattern NodeDataType_UINT64_ARRAY :: NodeDataType
pattern NodeDataType_UINT64_ARRAY = NodeDataType' "UINT64_ARRAY"

pattern NodeDataType_UINT8 :: NodeDataType
pattern NodeDataType_UINT8 = NodeDataType' "UINT8"

pattern NodeDataType_UINT8_ARRAY :: NodeDataType
pattern NodeDataType_UINT8_ARRAY = NodeDataType' "UINT8_ARRAY"

pattern NodeDataType_UNIX_TIMESTAMP :: NodeDataType
pattern NodeDataType_UNIX_TIMESTAMP = NodeDataType' "UNIX_TIMESTAMP"

pattern NodeDataType_UNIX_TIMESTAMP_ARRAY :: NodeDataType
pattern NodeDataType_UNIX_TIMESTAMP_ARRAY = NodeDataType' "UNIX_TIMESTAMP_ARRAY"

pattern NodeDataType_UNKNOWN :: NodeDataType
pattern NodeDataType_UNKNOWN = NodeDataType' "UNKNOWN"

{-# COMPLETE
  NodeDataType_BOOLEAN,
  NodeDataType_BOOLEAN_ARRAY,
  NodeDataType_DOUBLE,
  NodeDataType_DOUBLE_ARRAY,
  NodeDataType_FLOAT,
  NodeDataType_FLOAT_ARRAY,
  NodeDataType_INT16,
  NodeDataType_INT16_ARRAY,
  NodeDataType_INT32,
  NodeDataType_INT32_ARRAY,
  NodeDataType_INT64,
  NodeDataType_INT64_ARRAY,
  NodeDataType_INT8,
  NodeDataType_INT8_ARRAY,
  NodeDataType_STRING,
  NodeDataType_STRING_ARRAY,
  NodeDataType_UINT16,
  NodeDataType_UINT16_ARRAY,
  NodeDataType_UINT32,
  NodeDataType_UINT32_ARRAY,
  NodeDataType_UINT64,
  NodeDataType_UINT64_ARRAY,
  NodeDataType_UINT8,
  NodeDataType_UINT8_ARRAY,
  NodeDataType_UNIX_TIMESTAMP,
  NodeDataType_UNIX_TIMESTAMP_ARRAY,
  NodeDataType_UNKNOWN,
  NodeDataType'
  #-}

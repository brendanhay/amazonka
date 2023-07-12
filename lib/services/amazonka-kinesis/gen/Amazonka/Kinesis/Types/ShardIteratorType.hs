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
-- Module      : Amazonka.Kinesis.Types.ShardIteratorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.ShardIteratorType
  ( ShardIteratorType
      ( ..,
        ShardIteratorType_AFTER_SEQUENCE_NUMBER,
        ShardIteratorType_AT_SEQUENCE_NUMBER,
        ShardIteratorType_AT_TIMESTAMP,
        ShardIteratorType_LATEST,
        ShardIteratorType_TRIM_HORIZON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ShardIteratorType = ShardIteratorType'
  { fromShardIteratorType ::
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

pattern ShardIteratorType_AFTER_SEQUENCE_NUMBER :: ShardIteratorType
pattern ShardIteratorType_AFTER_SEQUENCE_NUMBER = ShardIteratorType' "AFTER_SEQUENCE_NUMBER"

pattern ShardIteratorType_AT_SEQUENCE_NUMBER :: ShardIteratorType
pattern ShardIteratorType_AT_SEQUENCE_NUMBER = ShardIteratorType' "AT_SEQUENCE_NUMBER"

pattern ShardIteratorType_AT_TIMESTAMP :: ShardIteratorType
pattern ShardIteratorType_AT_TIMESTAMP = ShardIteratorType' "AT_TIMESTAMP"

pattern ShardIteratorType_LATEST :: ShardIteratorType
pattern ShardIteratorType_LATEST = ShardIteratorType' "LATEST"

pattern ShardIteratorType_TRIM_HORIZON :: ShardIteratorType
pattern ShardIteratorType_TRIM_HORIZON = ShardIteratorType' "TRIM_HORIZON"

{-# COMPLETE
  ShardIteratorType_AFTER_SEQUENCE_NUMBER,
  ShardIteratorType_AT_SEQUENCE_NUMBER,
  ShardIteratorType_AT_TIMESTAMP,
  ShardIteratorType_LATEST,
  ShardIteratorType_TRIM_HORIZON,
  ShardIteratorType'
  #-}

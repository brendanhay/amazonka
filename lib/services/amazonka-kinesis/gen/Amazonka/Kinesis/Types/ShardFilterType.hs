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
-- Module      : Amazonka.Kinesis.Types.ShardFilterType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.ShardFilterType
  ( ShardFilterType
      ( ..,
        ShardFilterType_AFTER_SHARD_ID,
        ShardFilterType_AT_LATEST,
        ShardFilterType_AT_TIMESTAMP,
        ShardFilterType_AT_TRIM_HORIZON,
        ShardFilterType_FROM_TIMESTAMP,
        ShardFilterType_FROM_TRIM_HORIZON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ShardFilterType = ShardFilterType'
  { fromShardFilterType ::
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

pattern ShardFilterType_AFTER_SHARD_ID :: ShardFilterType
pattern ShardFilterType_AFTER_SHARD_ID = ShardFilterType' "AFTER_SHARD_ID"

pattern ShardFilterType_AT_LATEST :: ShardFilterType
pattern ShardFilterType_AT_LATEST = ShardFilterType' "AT_LATEST"

pattern ShardFilterType_AT_TIMESTAMP :: ShardFilterType
pattern ShardFilterType_AT_TIMESTAMP = ShardFilterType' "AT_TIMESTAMP"

pattern ShardFilterType_AT_TRIM_HORIZON :: ShardFilterType
pattern ShardFilterType_AT_TRIM_HORIZON = ShardFilterType' "AT_TRIM_HORIZON"

pattern ShardFilterType_FROM_TIMESTAMP :: ShardFilterType
pattern ShardFilterType_FROM_TIMESTAMP = ShardFilterType' "FROM_TIMESTAMP"

pattern ShardFilterType_FROM_TRIM_HORIZON :: ShardFilterType
pattern ShardFilterType_FROM_TRIM_HORIZON = ShardFilterType' "FROM_TRIM_HORIZON"

{-# COMPLETE
  ShardFilterType_AFTER_SHARD_ID,
  ShardFilterType_AT_LATEST,
  ShardFilterType_AT_TIMESTAMP,
  ShardFilterType_AT_TRIM_HORIZON,
  ShardFilterType_FROM_TIMESTAMP,
  ShardFilterType_FROM_TRIM_HORIZON,
  ShardFilterType'
  #-}

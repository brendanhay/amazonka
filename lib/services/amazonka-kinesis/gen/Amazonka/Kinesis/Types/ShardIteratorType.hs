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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype ShardIteratorType = ShardIteratorType'
  { fromShardIteratorType ::
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

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardIteratorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ShardIteratorType
  ( ShardIteratorType
      ( ShardIteratorType',
        ShardIteratorTypeAtSequenceNumber,
        ShardIteratorTypeAfterSequenceNumber,
        ShardIteratorTypeTrimHorizon,
        ShardIteratorTypeLatest,
        ShardIteratorTypeAtTimestamp,
        fromShardIteratorType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ShardIteratorType = ShardIteratorType'
  { fromShardIteratorType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ShardIteratorTypeAtSequenceNumber :: ShardIteratorType
pattern ShardIteratorTypeAtSequenceNumber = ShardIteratorType' "AT_SEQUENCE_NUMBER"

pattern ShardIteratorTypeAfterSequenceNumber :: ShardIteratorType
pattern ShardIteratorTypeAfterSequenceNumber = ShardIteratorType' "AFTER_SEQUENCE_NUMBER"

pattern ShardIteratorTypeTrimHorizon :: ShardIteratorType
pattern ShardIteratorTypeTrimHorizon = ShardIteratorType' "TRIM_HORIZON"

pattern ShardIteratorTypeLatest :: ShardIteratorType
pattern ShardIteratorTypeLatest = ShardIteratorType' "LATEST"

pattern ShardIteratorTypeAtTimestamp :: ShardIteratorType
pattern ShardIteratorTypeAtTimestamp = ShardIteratorType' "AT_TIMESTAMP"

{-# COMPLETE
  ShardIteratorTypeAtSequenceNumber,
  ShardIteratorTypeAfterSequenceNumber,
  ShardIteratorTypeTrimHorizon,
  ShardIteratorTypeLatest,
  ShardIteratorTypeAtTimestamp,
  ShardIteratorType'
  #-}

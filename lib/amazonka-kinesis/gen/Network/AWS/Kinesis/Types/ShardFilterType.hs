{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ShardFilterType
  ( ShardFilterType
      ( ShardFilterType',
        AfterShardId,
        AtLatest,
        AtTimestamp,
        AtTrimHorizon,
        FromTimestamp,
        FromTrimHorizon
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ShardFilterType = ShardFilterType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AfterShardId :: ShardFilterType
pattern AfterShardId = ShardFilterType' "AFTER_SHARD_ID"

pattern AtLatest :: ShardFilterType
pattern AtLatest = ShardFilterType' "AT_LATEST"

pattern AtTimestamp :: ShardFilterType
pattern AtTimestamp = ShardFilterType' "AT_TIMESTAMP"

pattern AtTrimHorizon :: ShardFilterType
pattern AtTrimHorizon = ShardFilterType' "AT_TRIM_HORIZON"

pattern FromTimestamp :: ShardFilterType
pattern FromTimestamp = ShardFilterType' "FROM_TIMESTAMP"

pattern FromTrimHorizon :: ShardFilterType
pattern FromTrimHorizon = ShardFilterType' "FROM_TRIM_HORIZON"

{-# COMPLETE
  AfterShardId,
  AtLatest,
  AtTimestamp,
  AtTrimHorizon,
  FromTimestamp,
  FromTrimHorizon,
  ShardFilterType'
  #-}

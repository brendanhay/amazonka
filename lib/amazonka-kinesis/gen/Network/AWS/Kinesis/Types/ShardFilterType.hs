{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ShardFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ShardFilterType
  ( ShardFilterType
    ( ShardFilterType'
    , ShardFilterTypeAfterShardId
    , ShardFilterTypeAtTrimHorizon
    , ShardFilterTypeFromTrimHorizon
    , ShardFilterTypeAtLatest
    , ShardFilterTypeAtTimestamp
    , ShardFilterTypeFromTimestamp
    , fromShardFilterType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ShardFilterType = ShardFilterType'{fromShardFilterType ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern ShardFilterTypeAfterShardId :: ShardFilterType
pattern ShardFilterTypeAfterShardId = ShardFilterType' "AFTER_SHARD_ID"

pattern ShardFilterTypeAtTrimHorizon :: ShardFilterType
pattern ShardFilterTypeAtTrimHorizon = ShardFilterType' "AT_TRIM_HORIZON"

pattern ShardFilterTypeFromTrimHorizon :: ShardFilterType
pattern ShardFilterTypeFromTrimHorizon = ShardFilterType' "FROM_TRIM_HORIZON"

pattern ShardFilterTypeAtLatest :: ShardFilterType
pattern ShardFilterTypeAtLatest = ShardFilterType' "AT_LATEST"

pattern ShardFilterTypeAtTimestamp :: ShardFilterType
pattern ShardFilterTypeAtTimestamp = ShardFilterType' "AT_TIMESTAMP"

pattern ShardFilterTypeFromTimestamp :: ShardFilterType
pattern ShardFilterTypeFromTimestamp = ShardFilterType' "FROM_TIMESTAMP"

{-# COMPLETE 
  ShardFilterTypeAfterShardId,

  ShardFilterTypeAtTrimHorizon,

  ShardFilterTypeFromTrimHorizon,

  ShardFilterTypeAtLatest,

  ShardFilterTypeAtTimestamp,

  ShardFilterTypeFromTimestamp,
  ShardFilterType'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RangeMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.RangeMode
  ( RangeMode
    ( RangeMode'
    , RangeModeFirst
    , RangeModeLast
    , RangeModeLastBeforeMissingValues
    , RangeModeInclusive
    , RangeModeExclusive
    , fromRangeMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RangeMode = RangeMode'{fromRangeMode :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern RangeModeFirst :: RangeMode
pattern RangeModeFirst = RangeMode' "FIRST"

pattern RangeModeLast :: RangeMode
pattern RangeModeLast = RangeMode' "LAST"

pattern RangeModeLastBeforeMissingValues :: RangeMode
pattern RangeModeLastBeforeMissingValues = RangeMode' "LAST_BEFORE_MISSING_VALUES"

pattern RangeModeInclusive :: RangeMode
pattern RangeModeInclusive = RangeMode' "INCLUSIVE"

pattern RangeModeExclusive :: RangeMode
pattern RangeModeExclusive = RangeMode' "EXCLUSIVE"

{-# COMPLETE 
  RangeModeFirst,

  RangeModeLast,

  RangeModeLastBeforeMissingValues,

  RangeModeInclusive,

  RangeModeExclusive,
  RangeMode'
  #-}

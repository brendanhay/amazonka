{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types.StartSelectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoMedia.Types.StartSelectorType
  ( StartSelectorType
      ( StartSelectorType',
        StartSelectorTypeFragmentNumber,
        StartSelectorTypeServerTimestamp,
        StartSelectorTypeProducerTimestamp,
        StartSelectorTypeNow,
        StartSelectorTypeEarliest,
        StartSelectorTypeContinuationToken,
        fromStartSelectorType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StartSelectorType = StartSelectorType'
  { fromStartSelectorType ::
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

pattern StartSelectorTypeFragmentNumber :: StartSelectorType
pattern StartSelectorTypeFragmentNumber = StartSelectorType' "FRAGMENT_NUMBER"

pattern StartSelectorTypeServerTimestamp :: StartSelectorType
pattern StartSelectorTypeServerTimestamp = StartSelectorType' "SERVER_TIMESTAMP"

pattern StartSelectorTypeProducerTimestamp :: StartSelectorType
pattern StartSelectorTypeProducerTimestamp = StartSelectorType' "PRODUCER_TIMESTAMP"

pattern StartSelectorTypeNow :: StartSelectorType
pattern StartSelectorTypeNow = StartSelectorType' "NOW"

pattern StartSelectorTypeEarliest :: StartSelectorType
pattern StartSelectorTypeEarliest = StartSelectorType' "EARLIEST"

pattern StartSelectorTypeContinuationToken :: StartSelectorType
pattern StartSelectorTypeContinuationToken = StartSelectorType' "CONTINUATION_TOKEN"

{-# COMPLETE
  StartSelectorTypeFragmentNumber,
  StartSelectorTypeServerTimestamp,
  StartSelectorTypeProducerTimestamp,
  StartSelectorTypeNow,
  StartSelectorTypeEarliest,
  StartSelectorTypeContinuationToken,
  StartSelectorType'
  #-}

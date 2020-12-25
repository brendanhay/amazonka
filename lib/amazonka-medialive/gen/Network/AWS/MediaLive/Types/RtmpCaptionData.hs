{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCaptionData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCaptionData
  ( RtmpCaptionData
      ( RtmpCaptionData',
        RtmpCaptionDataAll,
        RtmpCaptionDataFIELD1608,
        RtmpCaptionDataFIELD1AndFIELD2608,
        fromRtmpCaptionData
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Rtmp Caption Data
newtype RtmpCaptionData = RtmpCaptionData'
  { fromRtmpCaptionData ::
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

pattern RtmpCaptionDataAll :: RtmpCaptionData
pattern RtmpCaptionDataAll = RtmpCaptionData' "ALL"

pattern RtmpCaptionDataFIELD1608 :: RtmpCaptionData
pattern RtmpCaptionDataFIELD1608 = RtmpCaptionData' "FIELD1_608"

pattern RtmpCaptionDataFIELD1AndFIELD2608 :: RtmpCaptionData
pattern RtmpCaptionDataFIELD1AndFIELD2608 = RtmpCaptionData' "FIELD1_AND_FIELD2_608"

{-# COMPLETE
  RtmpCaptionDataAll,
  RtmpCaptionDataFIELD1608,
  RtmpCaptionDataFIELD1AndFIELD2608,
  RtmpCaptionData'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264CodecLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264CodecLevel
  ( H264CodecLevel
      ( H264CodecLevel',
        H264CodecLevelAuto,
        H264CodecLevelLevel1,
        H264CodecLevelLevel11,
        H264CodecLevelLevel12,
        H264CodecLevelLevel13,
        H264CodecLevelLevel2,
        H264CodecLevelLevel21,
        H264CodecLevelLevel22,
        H264CodecLevelLevel3,
        H264CodecLevelLevel31,
        H264CodecLevelLevel32,
        H264CodecLevelLevel4,
        H264CodecLevelLevel41,
        H264CodecLevelLevel42,
        H264CodecLevelLevel5,
        H264CodecLevelLevel51,
        H264CodecLevelLevel52,
        fromH264CodecLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify an H.264 level that is consistent with your output video settings. If you aren't sure what level to specify, choose Auto (AUTO).
newtype H264CodecLevel = H264CodecLevel'
  { fromH264CodecLevel ::
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

pattern H264CodecLevelAuto :: H264CodecLevel
pattern H264CodecLevelAuto = H264CodecLevel' "AUTO"

pattern H264CodecLevelLevel1 :: H264CodecLevel
pattern H264CodecLevelLevel1 = H264CodecLevel' "LEVEL_1"

pattern H264CodecLevelLevel11 :: H264CodecLevel
pattern H264CodecLevelLevel11 = H264CodecLevel' "LEVEL_1_1"

pattern H264CodecLevelLevel12 :: H264CodecLevel
pattern H264CodecLevelLevel12 = H264CodecLevel' "LEVEL_1_2"

pattern H264CodecLevelLevel13 :: H264CodecLevel
pattern H264CodecLevelLevel13 = H264CodecLevel' "LEVEL_1_3"

pattern H264CodecLevelLevel2 :: H264CodecLevel
pattern H264CodecLevelLevel2 = H264CodecLevel' "LEVEL_2"

pattern H264CodecLevelLevel21 :: H264CodecLevel
pattern H264CodecLevelLevel21 = H264CodecLevel' "LEVEL_2_1"

pattern H264CodecLevelLevel22 :: H264CodecLevel
pattern H264CodecLevelLevel22 = H264CodecLevel' "LEVEL_2_2"

pattern H264CodecLevelLevel3 :: H264CodecLevel
pattern H264CodecLevelLevel3 = H264CodecLevel' "LEVEL_3"

pattern H264CodecLevelLevel31 :: H264CodecLevel
pattern H264CodecLevelLevel31 = H264CodecLevel' "LEVEL_3_1"

pattern H264CodecLevelLevel32 :: H264CodecLevel
pattern H264CodecLevelLevel32 = H264CodecLevel' "LEVEL_3_2"

pattern H264CodecLevelLevel4 :: H264CodecLevel
pattern H264CodecLevelLevel4 = H264CodecLevel' "LEVEL_4"

pattern H264CodecLevelLevel41 :: H264CodecLevel
pattern H264CodecLevelLevel41 = H264CodecLevel' "LEVEL_4_1"

pattern H264CodecLevelLevel42 :: H264CodecLevel
pattern H264CodecLevelLevel42 = H264CodecLevel' "LEVEL_4_2"

pattern H264CodecLevelLevel5 :: H264CodecLevel
pattern H264CodecLevelLevel5 = H264CodecLevel' "LEVEL_5"

pattern H264CodecLevelLevel51 :: H264CodecLevel
pattern H264CodecLevelLevel51 = H264CodecLevel' "LEVEL_5_1"

pattern H264CodecLevelLevel52 :: H264CodecLevel
pattern H264CodecLevelLevel52 = H264CodecLevel' "LEVEL_5_2"

{-# COMPLETE
  H264CodecLevelAuto,
  H264CodecLevelLevel1,
  H264CodecLevelLevel11,
  H264CodecLevelLevel12,
  H264CodecLevelLevel13,
  H264CodecLevelLevel2,
  H264CodecLevelLevel21,
  H264CodecLevelLevel22,
  H264CodecLevelLevel3,
  H264CodecLevelLevel31,
  H264CodecLevelLevel32,
  H264CodecLevelLevel4,
  H264CodecLevelLevel41,
  H264CodecLevelLevel42,
  H264CodecLevelLevel5,
  H264CodecLevelLevel51,
  H264CodecLevelLevel52,
  H264CodecLevel'
  #-}

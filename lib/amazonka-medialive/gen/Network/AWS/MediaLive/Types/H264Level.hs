{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Level
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Level
  ( H264Level
      ( H264Level',
        H264LevelH264Level1,
        H264LevelH264Level11,
        H264LevelH264Level12,
        H264LevelH264Level13,
        H264LevelH264Level2,
        H264LevelH264Level21,
        H264LevelH264Level22,
        H264LevelH264Level3,
        H264LevelH264Level31,
        H264LevelH264Level32,
        H264LevelH264Level4,
        H264LevelH264Level41,
        H264LevelH264Level42,
        H264LevelH264Level5,
        H264LevelH264Level51,
        H264LevelH264Level52,
        H264LevelH264LevelAuto,
        fromH264Level
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H264 Level
newtype H264Level = H264Level' {fromH264Level :: Core.Text}
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

pattern H264LevelH264Level1 :: H264Level
pattern H264LevelH264Level1 = H264Level' "H264_LEVEL_1"

pattern H264LevelH264Level11 :: H264Level
pattern H264LevelH264Level11 = H264Level' "H264_LEVEL_1_1"

pattern H264LevelH264Level12 :: H264Level
pattern H264LevelH264Level12 = H264Level' "H264_LEVEL_1_2"

pattern H264LevelH264Level13 :: H264Level
pattern H264LevelH264Level13 = H264Level' "H264_LEVEL_1_3"

pattern H264LevelH264Level2 :: H264Level
pattern H264LevelH264Level2 = H264Level' "H264_LEVEL_2"

pattern H264LevelH264Level21 :: H264Level
pattern H264LevelH264Level21 = H264Level' "H264_LEVEL_2_1"

pattern H264LevelH264Level22 :: H264Level
pattern H264LevelH264Level22 = H264Level' "H264_LEVEL_2_2"

pattern H264LevelH264Level3 :: H264Level
pattern H264LevelH264Level3 = H264Level' "H264_LEVEL_3"

pattern H264LevelH264Level31 :: H264Level
pattern H264LevelH264Level31 = H264Level' "H264_LEVEL_3_1"

pattern H264LevelH264Level32 :: H264Level
pattern H264LevelH264Level32 = H264Level' "H264_LEVEL_3_2"

pattern H264LevelH264Level4 :: H264Level
pattern H264LevelH264Level4 = H264Level' "H264_LEVEL_4"

pattern H264LevelH264Level41 :: H264Level
pattern H264LevelH264Level41 = H264Level' "H264_LEVEL_4_1"

pattern H264LevelH264Level42 :: H264Level
pattern H264LevelH264Level42 = H264Level' "H264_LEVEL_4_2"

pattern H264LevelH264Level5 :: H264Level
pattern H264LevelH264Level5 = H264Level' "H264_LEVEL_5"

pattern H264LevelH264Level51 :: H264Level
pattern H264LevelH264Level51 = H264Level' "H264_LEVEL_5_1"

pattern H264LevelH264Level52 :: H264Level
pattern H264LevelH264Level52 = H264Level' "H264_LEVEL_5_2"

pattern H264LevelH264LevelAuto :: H264Level
pattern H264LevelH264LevelAuto = H264Level' "H264_LEVEL_AUTO"

{-# COMPLETE
  H264LevelH264Level1,
  H264LevelH264Level11,
  H264LevelH264Level12,
  H264LevelH264Level13,
  H264LevelH264Level2,
  H264LevelH264Level21,
  H264LevelH264Level22,
  H264LevelH264Level3,
  H264LevelH264Level31,
  H264LevelH264Level32,
  H264LevelH264Level4,
  H264LevelH264Level41,
  H264LevelH264Level42,
  H264LevelH264Level5,
  H264LevelH264Level51,
  H264LevelH264Level52,
  H264LevelH264LevelAuto,
  H264Level'
  #-}

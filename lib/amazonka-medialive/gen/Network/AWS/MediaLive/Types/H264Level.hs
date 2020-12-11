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
        H264Level1,
        H264Level11,
        H264Level12,
        H264Level13,
        H264Level2,
        H264Level21,
        H264Level22,
        H264Level3,
        H264Level31,
        H264Level32,
        H264Level4,
        H264Level41,
        H264Level42,
        H264Level5,
        H264Level51,
        H264Level52,
        H264LevelAuto
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Level
newtype H264Level = H264Level' Lude.Text
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

pattern H264Level1 :: H264Level
pattern H264Level1 = H264Level' "H264_LEVEL_1"

pattern H264Level11 :: H264Level
pattern H264Level11 = H264Level' "H264_LEVEL_1_1"

pattern H264Level12 :: H264Level
pattern H264Level12 = H264Level' "H264_LEVEL_1_2"

pattern H264Level13 :: H264Level
pattern H264Level13 = H264Level' "H264_LEVEL_1_3"

pattern H264Level2 :: H264Level
pattern H264Level2 = H264Level' "H264_LEVEL_2"

pattern H264Level21 :: H264Level
pattern H264Level21 = H264Level' "H264_LEVEL_2_1"

pattern H264Level22 :: H264Level
pattern H264Level22 = H264Level' "H264_LEVEL_2_2"

pattern H264Level3 :: H264Level
pattern H264Level3 = H264Level' "H264_LEVEL_3"

pattern H264Level31 :: H264Level
pattern H264Level31 = H264Level' "H264_LEVEL_3_1"

pattern H264Level32 :: H264Level
pattern H264Level32 = H264Level' "H264_LEVEL_3_2"

pattern H264Level4 :: H264Level
pattern H264Level4 = H264Level' "H264_LEVEL_4"

pattern H264Level41 :: H264Level
pattern H264Level41 = H264Level' "H264_LEVEL_4_1"

pattern H264Level42 :: H264Level
pattern H264Level42 = H264Level' "H264_LEVEL_4_2"

pattern H264Level5 :: H264Level
pattern H264Level5 = H264Level' "H264_LEVEL_5"

pattern H264Level51 :: H264Level
pattern H264Level51 = H264Level' "H264_LEVEL_5_1"

pattern H264Level52 :: H264Level
pattern H264Level52 = H264Level' "H264_LEVEL_5_2"

pattern H264LevelAuto :: H264Level
pattern H264LevelAuto = H264Level' "H264_LEVEL_AUTO"

{-# COMPLETE
  H264Level1,
  H264Level11,
  H264Level12,
  H264Level13,
  H264Level2,
  H264Level21,
  H264Level22,
  H264Level3,
  H264Level31,
  H264Level32,
  H264Level4,
  H264Level41,
  H264Level42,
  H264Level5,
  H264Level51,
  H264Level52,
  H264LevelAuto,
  H264Level'
  #-}

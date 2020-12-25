{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265Level
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265Level
  ( H265Level
      ( H265Level',
        H265LevelH265Level1,
        H265LevelH265Level2,
        H265LevelH265Level21,
        H265LevelH265Level3,
        H265LevelH265Level31,
        H265LevelH265Level4,
        H265LevelH265Level41,
        H265LevelH265Level5,
        H265LevelH265Level51,
        H265LevelH265Level52,
        H265LevelH265Level6,
        H265LevelH265Level61,
        H265LevelH265Level62,
        H265LevelH265LevelAuto,
        fromH265Level
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H265 Level
newtype H265Level = H265Level' {fromH265Level :: Core.Text}
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

pattern H265LevelH265Level1 :: H265Level
pattern H265LevelH265Level1 = H265Level' "H265_LEVEL_1"

pattern H265LevelH265Level2 :: H265Level
pattern H265LevelH265Level2 = H265Level' "H265_LEVEL_2"

pattern H265LevelH265Level21 :: H265Level
pattern H265LevelH265Level21 = H265Level' "H265_LEVEL_2_1"

pattern H265LevelH265Level3 :: H265Level
pattern H265LevelH265Level3 = H265Level' "H265_LEVEL_3"

pattern H265LevelH265Level31 :: H265Level
pattern H265LevelH265Level31 = H265Level' "H265_LEVEL_3_1"

pattern H265LevelH265Level4 :: H265Level
pattern H265LevelH265Level4 = H265Level' "H265_LEVEL_4"

pattern H265LevelH265Level41 :: H265Level
pattern H265LevelH265Level41 = H265Level' "H265_LEVEL_4_1"

pattern H265LevelH265Level5 :: H265Level
pattern H265LevelH265Level5 = H265Level' "H265_LEVEL_5"

pattern H265LevelH265Level51 :: H265Level
pattern H265LevelH265Level51 = H265Level' "H265_LEVEL_5_1"

pattern H265LevelH265Level52 :: H265Level
pattern H265LevelH265Level52 = H265Level' "H265_LEVEL_5_2"

pattern H265LevelH265Level6 :: H265Level
pattern H265LevelH265Level6 = H265Level' "H265_LEVEL_6"

pattern H265LevelH265Level61 :: H265Level
pattern H265LevelH265Level61 = H265Level' "H265_LEVEL_6_1"

pattern H265LevelH265Level62 :: H265Level
pattern H265LevelH265Level62 = H265Level' "H265_LEVEL_6_2"

pattern H265LevelH265LevelAuto :: H265Level
pattern H265LevelH265LevelAuto = H265Level' "H265_LEVEL_AUTO"

{-# COMPLETE
  H265LevelH265Level1,
  H265LevelH265Level2,
  H265LevelH265Level21,
  H265LevelH265Level3,
  H265LevelH265Level31,
  H265LevelH265Level4,
  H265LevelH265Level41,
  H265LevelH265Level5,
  H265LevelH265Level51,
  H265LevelH265Level52,
  H265LevelH265Level6,
  H265LevelH265Level61,
  H265LevelH265Level62,
  H265LevelH265LevelAuto,
  H265Level'
  #-}

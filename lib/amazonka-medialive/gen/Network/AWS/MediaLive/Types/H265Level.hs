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
        H265Level1,
        H265Level2,
        H265Level21,
        H265Level3,
        H265Level31,
        H265Level4,
        H265Level41,
        H265Level5,
        H265Level51,
        H265Level52,
        H265Level6,
        H265Level61,
        H265Level62,
        H265LevelAuto
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Level
newtype H265Level = H265Level' Lude.Text
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

pattern H265Level1 :: H265Level
pattern H265Level1 = H265Level' "H265_LEVEL_1"

pattern H265Level2 :: H265Level
pattern H265Level2 = H265Level' "H265_LEVEL_2"

pattern H265Level21 :: H265Level
pattern H265Level21 = H265Level' "H265_LEVEL_2_1"

pattern H265Level3 :: H265Level
pattern H265Level3 = H265Level' "H265_LEVEL_3"

pattern H265Level31 :: H265Level
pattern H265Level31 = H265Level' "H265_LEVEL_3_1"

pattern H265Level4 :: H265Level
pattern H265Level4 = H265Level' "H265_LEVEL_4"

pattern H265Level41 :: H265Level
pattern H265Level41 = H265Level' "H265_LEVEL_4_1"

pattern H265Level5 :: H265Level
pattern H265Level5 = H265Level' "H265_LEVEL_5"

pattern H265Level51 :: H265Level
pattern H265Level51 = H265Level' "H265_LEVEL_5_1"

pattern H265Level52 :: H265Level
pattern H265Level52 = H265Level' "H265_LEVEL_5_2"

pattern H265Level6 :: H265Level
pattern H265Level6 = H265Level' "H265_LEVEL_6"

pattern H265Level61 :: H265Level
pattern H265Level61 = H265Level' "H265_LEVEL_6_1"

pattern H265Level62 :: H265Level
pattern H265Level62 = H265Level' "H265_LEVEL_6_2"

pattern H265LevelAuto :: H265Level
pattern H265LevelAuto = H265Level' "H265_LEVEL_AUTO"

{-# COMPLETE
  H265Level1,
  H265Level2,
  H265Level21,
  H265Level3,
  H265Level31,
  H265Level4,
  H265Level41,
  H265Level5,
  H265Level51,
  H265Level52,
  H265Level6,
  H265Level61,
  H265Level62,
  H265LevelAuto,
  H265Level'
  #-}

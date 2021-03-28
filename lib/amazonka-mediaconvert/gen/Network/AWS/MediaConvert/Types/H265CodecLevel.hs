{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265CodecLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265CodecLevel
  ( H265CodecLevel
    ( H265CodecLevel'
    , H265CodecLevelAuto
    , H265CodecLevelLevel1
    , H265CodecLevelLevel2
    , H265CodecLevelLevel21
    , H265CodecLevelLevel3
    , H265CodecLevelLevel31
    , H265CodecLevelLevel4
    , H265CodecLevelLevel41
    , H265CodecLevelLevel5
    , H265CodecLevelLevel51
    , H265CodecLevelLevel52
    , H265CodecLevelLevel6
    , H265CodecLevelLevel61
    , H265CodecLevelLevel62
    , fromH265CodecLevel
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H.265 Level.
newtype H265CodecLevel = H265CodecLevel'{fromH265CodecLevel ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern H265CodecLevelAuto :: H265CodecLevel
pattern H265CodecLevelAuto = H265CodecLevel' "AUTO"

pattern H265CodecLevelLevel1 :: H265CodecLevel
pattern H265CodecLevelLevel1 = H265CodecLevel' "LEVEL_1"

pattern H265CodecLevelLevel2 :: H265CodecLevel
pattern H265CodecLevelLevel2 = H265CodecLevel' "LEVEL_2"

pattern H265CodecLevelLevel21 :: H265CodecLevel
pattern H265CodecLevelLevel21 = H265CodecLevel' "LEVEL_2_1"

pattern H265CodecLevelLevel3 :: H265CodecLevel
pattern H265CodecLevelLevel3 = H265CodecLevel' "LEVEL_3"

pattern H265CodecLevelLevel31 :: H265CodecLevel
pattern H265CodecLevelLevel31 = H265CodecLevel' "LEVEL_3_1"

pattern H265CodecLevelLevel4 :: H265CodecLevel
pattern H265CodecLevelLevel4 = H265CodecLevel' "LEVEL_4"

pattern H265CodecLevelLevel41 :: H265CodecLevel
pattern H265CodecLevelLevel41 = H265CodecLevel' "LEVEL_4_1"

pattern H265CodecLevelLevel5 :: H265CodecLevel
pattern H265CodecLevelLevel5 = H265CodecLevel' "LEVEL_5"

pattern H265CodecLevelLevel51 :: H265CodecLevel
pattern H265CodecLevelLevel51 = H265CodecLevel' "LEVEL_5_1"

pattern H265CodecLevelLevel52 :: H265CodecLevel
pattern H265CodecLevelLevel52 = H265CodecLevel' "LEVEL_5_2"

pattern H265CodecLevelLevel6 :: H265CodecLevel
pattern H265CodecLevelLevel6 = H265CodecLevel' "LEVEL_6"

pattern H265CodecLevelLevel61 :: H265CodecLevel
pattern H265CodecLevelLevel61 = H265CodecLevel' "LEVEL_6_1"

pattern H265CodecLevelLevel62 :: H265CodecLevel
pattern H265CodecLevelLevel62 = H265CodecLevel' "LEVEL_6_2"

{-# COMPLETE 
  H265CodecLevelAuto,

  H265CodecLevelLevel1,

  H265CodecLevelLevel2,

  H265CodecLevelLevel21,

  H265CodecLevelLevel3,

  H265CodecLevelLevel31,

  H265CodecLevelLevel4,

  H265CodecLevelLevel41,

  H265CodecLevelLevel5,

  H265CodecLevelLevel51,

  H265CodecLevelLevel52,

  H265CodecLevelLevel6,

  H265CodecLevelLevel61,

  H265CodecLevelLevel62,
  H265CodecLevel'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EmotionName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.EmotionName
  ( EmotionName
    ( EmotionName'
    , EmotionNameHappy
    , EmotionNameSad
    , EmotionNameAngry
    , EmotionNameConfused
    , EmotionNameDisgusted
    , EmotionNameSurprised
    , EmotionNameCalm
    , EmotionNameUnknown
    , EmotionNameFear
    , fromEmotionName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EmotionName = EmotionName'{fromEmotionName :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern EmotionNameHappy :: EmotionName
pattern EmotionNameHappy = EmotionName' "HAPPY"

pattern EmotionNameSad :: EmotionName
pattern EmotionNameSad = EmotionName' "SAD"

pattern EmotionNameAngry :: EmotionName
pattern EmotionNameAngry = EmotionName' "ANGRY"

pattern EmotionNameConfused :: EmotionName
pattern EmotionNameConfused = EmotionName' "CONFUSED"

pattern EmotionNameDisgusted :: EmotionName
pattern EmotionNameDisgusted = EmotionName' "DISGUSTED"

pattern EmotionNameSurprised :: EmotionName
pattern EmotionNameSurprised = EmotionName' "SURPRISED"

pattern EmotionNameCalm :: EmotionName
pattern EmotionNameCalm = EmotionName' "CALM"

pattern EmotionNameUnknown :: EmotionName
pattern EmotionNameUnknown = EmotionName' "UNKNOWN"

pattern EmotionNameFear :: EmotionName
pattern EmotionNameFear = EmotionName' "FEAR"

{-# COMPLETE 
  EmotionNameHappy,

  EmotionNameSad,

  EmotionNameAngry,

  EmotionNameConfused,

  EmotionNameDisgusted,

  EmotionNameSurprised,

  EmotionNameCalm,

  EmotionNameUnknown,

  EmotionNameFear,
  EmotionName'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EmotionName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EmotionName
  ( EmotionName
      ( EmotionName',
        Angry,
        Calm,
        Confused,
        Disgusted,
        Fear,
        Happy,
        Sad,
        Surprised,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EmotionName = EmotionName' Lude.Text
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

pattern Angry :: EmotionName
pattern Angry = EmotionName' "ANGRY"

pattern Calm :: EmotionName
pattern Calm = EmotionName' "CALM"

pattern Confused :: EmotionName
pattern Confused = EmotionName' "CONFUSED"

pattern Disgusted :: EmotionName
pattern Disgusted = EmotionName' "DISGUSTED"

pattern Fear :: EmotionName
pattern Fear = EmotionName' "FEAR"

pattern Happy :: EmotionName
pattern Happy = EmotionName' "HAPPY"

pattern Sad :: EmotionName
pattern Sad = EmotionName' "SAD"

pattern Surprised :: EmotionName
pattern Surprised = EmotionName' "SURPRISED"

pattern Unknown :: EmotionName
pattern Unknown = EmotionName' "UNKNOWN"

{-# COMPLETE
  Angry,
  Calm,
  Confused,
  Disgusted,
  Fear,
  Happy,
  Sad,
  Surprised,
  Unknown,
  EmotionName'
  #-}

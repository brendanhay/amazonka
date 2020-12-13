{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioSelectorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioSelectorType
  ( AudioSelectorType
      ( AudioSelectorType',
        Pid,
        Track,
        LanguageCode
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies the type of the audio selector.
newtype AudioSelectorType = AudioSelectorType' Lude.Text
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

pattern Pid :: AudioSelectorType
pattern Pid = AudioSelectorType' "PID"

pattern Track :: AudioSelectorType
pattern Track = AudioSelectorType' "TRACK"

pattern LanguageCode :: AudioSelectorType
pattern LanguageCode = AudioSelectorType' "LANGUAGE_CODE"

{-# COMPLETE
  Pid,
  Track,
  LanguageCode,
  AudioSelectorType'
  #-}

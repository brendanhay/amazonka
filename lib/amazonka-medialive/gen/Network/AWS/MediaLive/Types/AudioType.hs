-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioType
  ( AudioType
      ( AudioType',
        CleanEffects,
        HearingImpaired,
        Undefined,
        VisualImpairedCommentary
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Audio Type
newtype AudioType = AudioType' Lude.Text
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

pattern CleanEffects :: AudioType
pattern CleanEffects = AudioType' "CLEAN_EFFECTS"

pattern HearingImpaired :: AudioType
pattern HearingImpaired = AudioType' "HEARING_IMPAIRED"

pattern Undefined :: AudioType
pattern Undefined = AudioType' "UNDEFINED"

pattern VisualImpairedCommentary :: AudioType
pattern VisualImpairedCommentary = AudioType' "VISUAL_IMPAIRED_COMMENTARY"

{-# COMPLETE
  CleanEffects,
  HearingImpaired,
  Undefined,
  VisualImpairedCommentary,
  AudioType'
  #-}

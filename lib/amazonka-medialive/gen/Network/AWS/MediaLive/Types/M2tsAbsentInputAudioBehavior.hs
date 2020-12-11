-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsAbsentInputAudioBehavior
  ( M2tsAbsentInputAudioBehavior
      ( M2tsAbsentInputAudioBehavior',
        Drop,
        EncodeSilence
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Absent Input Audio Behavior
newtype M2tsAbsentInputAudioBehavior = M2tsAbsentInputAudioBehavior' Lude.Text
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

pattern Drop :: M2tsAbsentInputAudioBehavior
pattern Drop = M2tsAbsentInputAudioBehavior' "DROP"

pattern EncodeSilence :: M2tsAbsentInputAudioBehavior
pattern EncodeSilence = M2tsAbsentInputAudioBehavior' "ENCODE_SILENCE"

{-# COMPLETE
  Drop,
  EncodeSilence,
  M2tsAbsentInputAudioBehavior'
  #-}

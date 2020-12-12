{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioChannelTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioChannelTag
  ( AudioChannelTag
      ( AudioChannelTag',
        C,
        CS,
        L,
        LC,
        LS,
        Lfe,
        Lsd,
        R,
        RC,
        RS,
        Rsd,
        Tcs,
        Vhc,
        Vhl,
        Vhr
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
newtype AudioChannelTag = AudioChannelTag' Lude.Text
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

pattern C :: AudioChannelTag
pattern C = AudioChannelTag' "C"

pattern CS :: AudioChannelTag
pattern CS = AudioChannelTag' "CS"

pattern L :: AudioChannelTag
pattern L = AudioChannelTag' "L"

pattern LC :: AudioChannelTag
pattern LC = AudioChannelTag' "LC"

pattern LS :: AudioChannelTag
pattern LS = AudioChannelTag' "LS"

pattern Lfe :: AudioChannelTag
pattern Lfe = AudioChannelTag' "LFE"

pattern Lsd :: AudioChannelTag
pattern Lsd = AudioChannelTag' "LSD"

pattern R :: AudioChannelTag
pattern R = AudioChannelTag' "R"

pattern RC :: AudioChannelTag
pattern RC = AudioChannelTag' "RC"

pattern RS :: AudioChannelTag
pattern RS = AudioChannelTag' "RS"

pattern Rsd :: AudioChannelTag
pattern Rsd = AudioChannelTag' "RSD"

pattern Tcs :: AudioChannelTag
pattern Tcs = AudioChannelTag' "TCS"

pattern Vhc :: AudioChannelTag
pattern Vhc = AudioChannelTag' "VHC"

pattern Vhl :: AudioChannelTag
pattern Vhl = AudioChannelTag' "VHL"

pattern Vhr :: AudioChannelTag
pattern Vhr = AudioChannelTag' "VHR"

{-# COMPLETE
  C,
  CS,
  L,
  LC,
  LS,
  Lfe,
  Lsd,
  R,
  RC,
  RS,
  Rsd,
  Tcs,
  Vhc,
  Vhl,
  Vhr,
  AudioChannelTag'
  #-}

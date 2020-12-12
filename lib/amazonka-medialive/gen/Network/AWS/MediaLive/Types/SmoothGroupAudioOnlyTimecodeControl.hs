{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupAudioOnlyTimecodeControl
  ( SmoothGroupAudioOnlyTimecodeControl
      ( SmoothGroupAudioOnlyTimecodeControl',
        SGAOTCPassthrough,
        SGAOTCUseConfiguredClock
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Smooth Group Audio Only Timecode Control
newtype SmoothGroupAudioOnlyTimecodeControl = SmoothGroupAudioOnlyTimecodeControl' Lude.Text
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

pattern SGAOTCPassthrough :: SmoothGroupAudioOnlyTimecodeControl
pattern SGAOTCPassthrough = SmoothGroupAudioOnlyTimecodeControl' "PASSTHROUGH"

pattern SGAOTCUseConfiguredClock :: SmoothGroupAudioOnlyTimecodeControl
pattern SGAOTCUseConfiguredClock = SmoothGroupAudioOnlyTimecodeControl' "USE_CONFIGURED_CLOCK"

{-# COMPLETE
  SGAOTCPassthrough,
  SGAOTCUseConfiguredClock,
  SmoothGroupAudioOnlyTimecodeControl'
  #-}

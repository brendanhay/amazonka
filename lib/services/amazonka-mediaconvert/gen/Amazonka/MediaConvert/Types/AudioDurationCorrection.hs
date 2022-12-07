{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.AudioDurationCorrection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioDurationCorrection
  ( AudioDurationCorrection
      ( ..,
        AudioDurationCorrection_AUTO,
        AudioDurationCorrection_DISABLED,
        AudioDurationCorrection_FRAME,
        AudioDurationCorrection_TRACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Apply audio timing corrections to help synchronize audio and video in
-- your output. To apply timing corrections, your input must meet the
-- following requirements: * Container: MP4, or MOV, with an accurate
-- time-to-sample (STTS) table. * Audio track: AAC. Choose from the
-- following audio timing correction settings: * Disabled (Default): Apply
-- no correction. * Auto: Recommended for most inputs. MediaConvert
-- analyzes the audio timing in your input and determines which correction
-- setting to use, if needed. * Track: Adjust the duration of each audio
-- frame by a constant amount to align the audio track length with STTS
-- duration. Track-level correction does not affect pitch, and is
-- recommended for tonal audio content such as music. * Frame: Adjust the
-- duration of each audio frame by a variable amount to align audio frames
-- with STTS timestamps. No corrections are made to already-aligned frames.
-- Frame-level correction may affect the pitch of corrected frames, and is
-- recommended for atonal audio content such as speech or percussion.
newtype AudioDurationCorrection = AudioDurationCorrection'
  { fromAudioDurationCorrection ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern AudioDurationCorrection_AUTO :: AudioDurationCorrection
pattern AudioDurationCorrection_AUTO = AudioDurationCorrection' "AUTO"

pattern AudioDurationCorrection_DISABLED :: AudioDurationCorrection
pattern AudioDurationCorrection_DISABLED = AudioDurationCorrection' "DISABLED"

pattern AudioDurationCorrection_FRAME :: AudioDurationCorrection
pattern AudioDurationCorrection_FRAME = AudioDurationCorrection' "FRAME"

pattern AudioDurationCorrection_TRACK :: AudioDurationCorrection
pattern AudioDurationCorrection_TRACK = AudioDurationCorrection' "TRACK"

{-# COMPLETE
  AudioDurationCorrection_AUTO,
  AudioDurationCorrection_DISABLED,
  AudioDurationCorrection_FRAME,
  AudioDurationCorrection_TRACK,
  AudioDurationCorrection'
  #-}

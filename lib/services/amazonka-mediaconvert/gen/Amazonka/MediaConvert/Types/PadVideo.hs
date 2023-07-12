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
-- Module      : Amazonka.MediaConvert.Types.PadVideo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.PadVideo
  ( PadVideo
      ( ..,
        PadVideo_BLACK,
        PadVideo_DISABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this setting if your input has video and audio durations that don\'t
-- align, and your output or player has strict alignment requirements.
-- Examples: Input audio track has a delayed start. Input video track ends
-- before audio ends. When you set Pad video (padVideo) to Black (BLACK),
-- MediaConvert generates black video frames so that output video and audio
-- durations match. Black video frames are added at the beginning or end,
-- depending on your input. To keep the default behavior and not generate
-- black video, set Pad video to Disabled (DISABLED) or leave blank.
newtype PadVideo = PadVideo'
  { fromPadVideo ::
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

pattern PadVideo_BLACK :: PadVideo
pattern PadVideo_BLACK = PadVideo' "BLACK"

pattern PadVideo_DISABLED :: PadVideo
pattern PadVideo_DISABLED = PadVideo' "DISABLED"

{-# COMPLETE
  PadVideo_BLACK,
  PadVideo_DISABLED,
  PadVideo'
  #-}

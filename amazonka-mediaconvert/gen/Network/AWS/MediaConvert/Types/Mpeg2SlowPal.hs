{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SlowPal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2SlowPal
  ( Mpeg2SlowPal
      ( ..,
        Mpeg2SlowPal_DISABLED,
        Mpeg2SlowPal_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output. When you
-- enable slow PAL, MediaConvert relabels the video frames to 25 fps and
-- resamples your audio to keep it synchronized with the video. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Required settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
newtype Mpeg2SlowPal = Mpeg2SlowPal'
  { fromMpeg2SlowPal ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern Mpeg2SlowPal_DISABLED :: Mpeg2SlowPal
pattern Mpeg2SlowPal_DISABLED = Mpeg2SlowPal' "DISABLED"

pattern Mpeg2SlowPal_ENABLED :: Mpeg2SlowPal
pattern Mpeg2SlowPal_ENABLED = Mpeg2SlowPal' "ENABLED"

{-# COMPLETE
  Mpeg2SlowPal_DISABLED,
  Mpeg2SlowPal_ENABLED,
  Mpeg2SlowPal'
  #-}

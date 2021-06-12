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
-- Module      : Network.AWS.MediaConvert.Types.Vc3SlowPal
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vc3SlowPal
  ( Vc3SlowPal
      ( ..,
        Vc3SlowPal_DISABLED,
        Vc3SlowPal_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Ignore this setting unless your input frame rate is 23.976 or 24 frames
-- per second (fps). Enable slow PAL to create a 25 fps output by
-- relabeling the video frames and resampling your audio. Note that
-- enabling this setting will slightly reduce the duration of your video.
-- Related settings: You must also set Framerate to 25. In your JSON job
-- specification, set (framerateControl) to (SPECIFIED),
-- (framerateNumerator) to 25 and (framerateDenominator) to 1.
newtype Vc3SlowPal = Vc3SlowPal'
  { fromVc3SlowPal ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern Vc3SlowPal_DISABLED :: Vc3SlowPal
pattern Vc3SlowPal_DISABLED = Vc3SlowPal' "DISABLED"

pattern Vc3SlowPal_ENABLED :: Vc3SlowPal
pattern Vc3SlowPal_ENABLED = Vc3SlowPal' "ENABLED"

{-# COMPLETE
  Vc3SlowPal_DISABLED,
  Vc3SlowPal_ENABLED,
  Vc3SlowPal'
  #-}

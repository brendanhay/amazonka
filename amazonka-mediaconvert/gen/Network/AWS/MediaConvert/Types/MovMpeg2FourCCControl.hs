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
-- Module      : Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovMpeg2FourCCControl
  ( MovMpeg2FourCCControl
      ( ..,
        MovMpeg2FourCCControl_MPEG,
        MovMpeg2FourCCControl_XDCAM
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file
-- using XDCAM fourcc codes. This increases compatibility with Apple
-- editors and players, but may decrease compatibility with other players.
-- Only applicable when the video codec is MPEG2.
newtype MovMpeg2FourCCControl = MovMpeg2FourCCControl'
  { fromMovMpeg2FourCCControl ::
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

pattern MovMpeg2FourCCControl_MPEG :: MovMpeg2FourCCControl
pattern MovMpeg2FourCCControl_MPEG = MovMpeg2FourCCControl' "MPEG"

pattern MovMpeg2FourCCControl_XDCAM :: MovMpeg2FourCCControl
pattern MovMpeg2FourCCControl_XDCAM = MovMpeg2FourCCControl' "XDCAM"

{-# COMPLETE
  MovMpeg2FourCCControl_MPEG,
  MovMpeg2FourCCControl_XDCAM,
  MovMpeg2FourCCControl'
  #-}

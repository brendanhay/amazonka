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

import qualified Network.AWS.Prelude as Prelude

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file
-- using XDCAM fourcc codes. This increases compatibility with Apple
-- editors and players, but may decrease compatibility with other players.
-- Only applicable when the video codec is MPEG2.
newtype MovMpeg2FourCCControl = MovMpeg2FourCCControl'
  { fromMovMpeg2FourCCControl ::
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

pattern MovMpeg2FourCCControl_MPEG :: MovMpeg2FourCCControl
pattern MovMpeg2FourCCControl_MPEG = MovMpeg2FourCCControl' "MPEG"

pattern MovMpeg2FourCCControl_XDCAM :: MovMpeg2FourCCControl
pattern MovMpeg2FourCCControl_XDCAM = MovMpeg2FourCCControl' "XDCAM"

{-# COMPLETE
  MovMpeg2FourCCControl_MPEG,
  MovMpeg2FourCCControl_XDCAM,
  MovMpeg2FourCCControl'
  #-}

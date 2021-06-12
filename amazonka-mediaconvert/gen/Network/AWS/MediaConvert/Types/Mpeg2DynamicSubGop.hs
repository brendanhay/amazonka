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
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2DynamicSubGop
  ( Mpeg2DynamicSubGop
      ( ..,
        Mpeg2DynamicSubGop_ADAPTIVE,
        Mpeg2DynamicSubGop_STATIC
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
newtype Mpeg2DynamicSubGop = Mpeg2DynamicSubGop'
  { fromMpeg2DynamicSubGop ::
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

pattern Mpeg2DynamicSubGop_ADAPTIVE :: Mpeg2DynamicSubGop
pattern Mpeg2DynamicSubGop_ADAPTIVE = Mpeg2DynamicSubGop' "ADAPTIVE"

pattern Mpeg2DynamicSubGop_STATIC :: Mpeg2DynamicSubGop
pattern Mpeg2DynamicSubGop_STATIC = Mpeg2DynamicSubGop' "STATIC"

{-# COMPLETE
  Mpeg2DynamicSubGop_ADAPTIVE,
  Mpeg2DynamicSubGop_STATIC,
  Mpeg2DynamicSubGop'
  #-}

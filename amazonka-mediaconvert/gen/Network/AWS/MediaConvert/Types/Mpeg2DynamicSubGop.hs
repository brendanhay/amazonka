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

import qualified Network.AWS.Prelude as Prelude

-- | Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
newtype Mpeg2DynamicSubGop = Mpeg2DynamicSubGop'
  { fromMpeg2DynamicSubGop ::
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

pattern Mpeg2DynamicSubGop_ADAPTIVE :: Mpeg2DynamicSubGop
pattern Mpeg2DynamicSubGop_ADAPTIVE = Mpeg2DynamicSubGop' "ADAPTIVE"

pattern Mpeg2DynamicSubGop_STATIC :: Mpeg2DynamicSubGop
pattern Mpeg2DynamicSubGop_STATIC = Mpeg2DynamicSubGop' "STATIC"

{-# COMPLETE
  Mpeg2DynamicSubGop_ADAPTIVE,
  Mpeg2DynamicSubGop_STATIC,
  Mpeg2DynamicSubGop'
  #-}

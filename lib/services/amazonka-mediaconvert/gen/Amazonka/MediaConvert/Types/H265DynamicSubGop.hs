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
-- Module      : Amazonka.MediaConvert.Types.H265DynamicSubGop
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265DynamicSubGop
  ( H265DynamicSubGop
      ( ..,
        H265DynamicSubGop_ADAPTIVE,
        H265DynamicSubGop_STATIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose Adaptive to improve subjective video quality for high-motion
-- content. This will cause the service to use fewer B-frames (which infer
-- information based on other frames) for high-motion portions of the video
-- and more B-frames for low-motion portions. The maximum number of
-- B-frames is limited by the value you provide for the setting B frames
-- between reference frames (numberBFramesBetweenReferenceFrames).
newtype H265DynamicSubGop = H265DynamicSubGop'
  { fromH265DynamicSubGop ::
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

pattern H265DynamicSubGop_ADAPTIVE :: H265DynamicSubGop
pattern H265DynamicSubGop_ADAPTIVE = H265DynamicSubGop' "ADAPTIVE"

pattern H265DynamicSubGop_STATIC :: H265DynamicSubGop
pattern H265DynamicSubGop_STATIC = H265DynamicSubGop' "STATIC"

{-# COMPLETE
  H265DynamicSubGop_ADAPTIVE,
  H265DynamicSubGop_STATIC,
  H265DynamicSubGop'
  #-}

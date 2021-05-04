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
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupSegmentationMode
  ( SmoothGroupSegmentationMode
      ( ..,
        SmoothGroupSegmentationMode_USE_INPUT_SEGMENTATION,
        SmoothGroupSegmentationMode_USE_SEGMENT_DURATION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Smooth Group Segmentation Mode
newtype SmoothGroupSegmentationMode = SmoothGroupSegmentationMode'
  { fromSmoothGroupSegmentationMode ::
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

pattern SmoothGroupSegmentationMode_USE_INPUT_SEGMENTATION :: SmoothGroupSegmentationMode
pattern SmoothGroupSegmentationMode_USE_INPUT_SEGMENTATION = SmoothGroupSegmentationMode' "USE_INPUT_SEGMENTATION"

pattern SmoothGroupSegmentationMode_USE_SEGMENT_DURATION :: SmoothGroupSegmentationMode
pattern SmoothGroupSegmentationMode_USE_SEGMENT_DURATION = SmoothGroupSegmentationMode' "USE_SEGMENT_DURATION"

{-# COMPLETE
  SmoothGroupSegmentationMode_USE_INPUT_SEGMENTATION,
  SmoothGroupSegmentationMode_USE_SEGMENT_DURATION,
  SmoothGroupSegmentationMode'
  #-}

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
-- Module      : Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsSegmentationMarkers
  ( M2tsSegmentationMarkers
      ( ..,
        M2tsSegmentationMarkers_EBP,
        M2tsSegmentationMarkers_EBP_LEGACY,
        M2tsSegmentationMarkers_NONE,
        M2tsSegmentationMarkers_PSI_SEGSTART,
        M2tsSegmentationMarkers_RAI_ADAPT,
        M2tsSegmentationMarkers_RAI_SEGSTART
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Inserts segmentation markers at each segmentation_time period.
-- rai_segstart sets the Random Access Indicator bit in the adaptation
-- field. rai_adapt sets the RAI bit and adds the current timecode in the
-- private data bytes. psi_segstart inserts PAT and PMT tables at the start
-- of segments. ebp adds Encoder Boundary Point information to the
-- adaptation field as per OpenCable specification OC-SP-EBP-I01-130118.
-- ebp_legacy adds Encoder Boundary Point information to the adaptation
-- field using a legacy proprietary format.
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers'
  { fromM2tsSegmentationMarkers ::
      Core.Text
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

pattern M2tsSegmentationMarkers_EBP :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkers_EBP = M2tsSegmentationMarkers' "EBP"

pattern M2tsSegmentationMarkers_EBP_LEGACY :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkers_EBP_LEGACY = M2tsSegmentationMarkers' "EBP_LEGACY"

pattern M2tsSegmentationMarkers_NONE :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkers_NONE = M2tsSegmentationMarkers' "NONE"

pattern M2tsSegmentationMarkers_PSI_SEGSTART :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkers_PSI_SEGSTART = M2tsSegmentationMarkers' "PSI_SEGSTART"

pattern M2tsSegmentationMarkers_RAI_ADAPT :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkers_RAI_ADAPT = M2tsSegmentationMarkers' "RAI_ADAPT"

pattern M2tsSegmentationMarkers_RAI_SEGSTART :: M2tsSegmentationMarkers
pattern M2tsSegmentationMarkers_RAI_SEGSTART = M2tsSegmentationMarkers' "RAI_SEGSTART"

{-# COMPLETE
  M2tsSegmentationMarkers_EBP,
  M2tsSegmentationMarkers_EBP_LEGACY,
  M2tsSegmentationMarkers_NONE,
  M2tsSegmentationMarkers_PSI_SEGSTART,
  M2tsSegmentationMarkers_RAI_ADAPT,
  M2tsSegmentationMarkers_RAI_SEGSTART,
  M2tsSegmentationMarkers'
  #-}

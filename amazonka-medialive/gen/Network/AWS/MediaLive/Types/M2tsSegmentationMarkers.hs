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
-- Module      : Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsSegmentationMarkers
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

import qualified Network.AWS.Prelude as Prelude

-- | M2ts Segmentation Markers
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers'
  { fromM2tsSegmentationMarkers ::
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

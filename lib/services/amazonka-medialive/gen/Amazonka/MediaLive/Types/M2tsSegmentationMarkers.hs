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
-- Module      : Amazonka.MediaLive.Types.M2tsSegmentationMarkers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.M2tsSegmentationMarkers
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | M2ts Segmentation Markers
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers'
  { fromM2tsSegmentationMarkers ::
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

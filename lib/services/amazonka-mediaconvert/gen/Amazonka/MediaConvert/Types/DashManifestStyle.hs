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
-- Module      : Amazonka.MediaConvert.Types.DashManifestStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DashManifestStyle
  ( DashManifestStyle
      ( ..,
        DashManifestStyle_BASIC,
        DashManifestStyle_COMPACT,
        DashManifestStyle_DISTINCT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify how MediaConvert writes SegmentTimeline in your output DASH
-- manifest. To write a SegmentTimeline in each video Representation: Keep
-- the default value, Basic. To write a common SegmentTimeline in the video
-- AdaptationSet: Choose Compact. Note that MediaConvert will still write a
-- SegmentTimeline in any Representation that does not share a common
-- timeline. To write a video AdaptationSet for each different output
-- framerate, and a common SegmentTimeline in each AdaptationSet: Choose
-- Distinct.
newtype DashManifestStyle = DashManifestStyle'
  { fromDashManifestStyle ::
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

pattern DashManifestStyle_BASIC :: DashManifestStyle
pattern DashManifestStyle_BASIC = DashManifestStyle' "BASIC"

pattern DashManifestStyle_COMPACT :: DashManifestStyle
pattern DashManifestStyle_COMPACT = DashManifestStyle' "COMPACT"

pattern DashManifestStyle_DISTINCT :: DashManifestStyle
pattern DashManifestStyle_DISTINCT = DashManifestStyle' "DISTINCT"

{-# COMPLETE
  DashManifestStyle_BASIC,
  DashManifestStyle_COMPACT,
  DashManifestStyle_DISTINCT,
  DashManifestStyle'
  #-}

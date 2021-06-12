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
-- Module      : Network.AWS.MediaLive.Types.HlsOutputSelection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsOutputSelection
  ( HlsOutputSelection
      ( ..,
        HlsOutputSelection_MANIFESTS_AND_SEGMENTS,
        HlsOutputSelection_SEGMENTS_ONLY,
        HlsOutputSelection_VARIANT_MANIFESTS_AND_SEGMENTS
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Hls Output Selection
newtype HlsOutputSelection = HlsOutputSelection'
  { fromHlsOutputSelection ::
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

pattern HlsOutputSelection_MANIFESTS_AND_SEGMENTS :: HlsOutputSelection
pattern HlsOutputSelection_MANIFESTS_AND_SEGMENTS = HlsOutputSelection' "MANIFESTS_AND_SEGMENTS"

pattern HlsOutputSelection_SEGMENTS_ONLY :: HlsOutputSelection
pattern HlsOutputSelection_SEGMENTS_ONLY = HlsOutputSelection' "SEGMENTS_ONLY"

pattern HlsOutputSelection_VARIANT_MANIFESTS_AND_SEGMENTS :: HlsOutputSelection
pattern HlsOutputSelection_VARIANT_MANIFESTS_AND_SEGMENTS = HlsOutputSelection' "VARIANT_MANIFESTS_AND_SEGMENTS"

{-# COMPLETE
  HlsOutputSelection_MANIFESTS_AND_SEGMENTS,
  HlsOutputSelection_SEGMENTS_ONLY,
  HlsOutputSelection_VARIANT_MANIFESTS_AND_SEGMENTS,
  HlsOutputSelection'
  #-}

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
-- Module      : Amazonka.MediaLive.Types.HlsOutputSelection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsOutputSelection
  ( HlsOutputSelection
      ( ..,
        HlsOutputSelection_MANIFESTS_AND_SEGMENTS,
        HlsOutputSelection_SEGMENTS_ONLY,
        HlsOutputSelection_VARIANT_MANIFESTS_AND_SEGMENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Hls Output Selection
newtype HlsOutputSelection = HlsOutputSelection'
  { fromHlsOutputSelection ::
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

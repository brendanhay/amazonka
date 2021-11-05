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
-- Module      : Amazonka.MediaConvert.Types.HlsOutputSelection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsOutputSelection
  ( HlsOutputSelection
      ( ..,
        HlsOutputSelection_MANIFESTS_AND_SEGMENTS,
        HlsOutputSelection_SEGMENTS_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether the .m3u8 manifest file should be generated for this
-- HLS output group.
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

{-# COMPLETE
  HlsOutputSelection_MANIFESTS_AND_SEGMENTS,
  HlsOutputSelection_SEGMENTS_ONLY,
  HlsOutputSelection'
  #-}

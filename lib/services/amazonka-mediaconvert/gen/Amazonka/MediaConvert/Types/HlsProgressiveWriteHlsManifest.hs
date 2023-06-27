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
-- Module      : Amazonka.MediaConvert.Types.HlsProgressiveWriteHlsManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsProgressiveWriteHlsManifest
  ( HlsProgressiveWriteHlsManifest
      ( ..,
        HlsProgressiveWriteHlsManifest_DISABLED,
        HlsProgressiveWriteHlsManifest_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify whether MediaConvert generates HLS manifests while your job is
-- running or when your job is complete. To generate HLS manifests while
-- your job is running: Choose Enabled. Use if you want to play back your
-- content as soon as it\'s available. MediaConvert writes the parent and
-- child manifests after the first three media segments are written to your
-- destination S3 bucket. It then writes new updated manifests after each
-- additional segment is written. The parent manifest includes the latest
-- BANDWIDTH and AVERAGE-BANDWIDTH attributes, and child manifests include
-- the latest available media segment. When your job completes, the final
-- child playlists include an EXT-X-ENDLIST tag. To generate HLS manifests
-- only when your job completes: Choose Disabled.
newtype HlsProgressiveWriteHlsManifest = HlsProgressiveWriteHlsManifest'
  { fromHlsProgressiveWriteHlsManifest ::
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

pattern HlsProgressiveWriteHlsManifest_DISABLED :: HlsProgressiveWriteHlsManifest
pattern HlsProgressiveWriteHlsManifest_DISABLED = HlsProgressiveWriteHlsManifest' "DISABLED"

pattern HlsProgressiveWriteHlsManifest_ENABLED :: HlsProgressiveWriteHlsManifest
pattern HlsProgressiveWriteHlsManifest_ENABLED = HlsProgressiveWriteHlsManifest' "ENABLED"

{-# COMPLETE
  HlsProgressiveWriteHlsManifest_DISABLED,
  HlsProgressiveWriteHlsManifest_ENABLED,
  HlsProgressiveWriteHlsManifest'
  #-}

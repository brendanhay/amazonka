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
-- Module      : Network.AWS.MediaConvert.Types.CmfcIFrameOnlyManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmfcIFrameOnlyManifest
  ( CmfcIFrameOnlyManifest
      ( ..,
        CmfcIFrameOnlyManifest_EXCLUDE,
        CmfcIFrameOnlyManifest_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Choose Include (INCLUDE) to have MediaConvert generate an HLS child
-- manifest that lists only the I-frames for this rendition, in addition to
-- your regular manifest for this rendition. You might use this manifest as
-- part of a workflow that creates preview functions for your video.
-- MediaConvert adds both the I-frame only child manifest and the regular
-- child manifest to the parent manifest. When you don\'t need the I-frame
-- only child manifest, keep the default value Exclude (EXCLUDE).
newtype CmfcIFrameOnlyManifest = CmfcIFrameOnlyManifest'
  { fromCmfcIFrameOnlyManifest ::
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

pattern CmfcIFrameOnlyManifest_EXCLUDE :: CmfcIFrameOnlyManifest
pattern CmfcIFrameOnlyManifest_EXCLUDE = CmfcIFrameOnlyManifest' "EXCLUDE"

pattern CmfcIFrameOnlyManifest_INCLUDE :: CmfcIFrameOnlyManifest
pattern CmfcIFrameOnlyManifest_INCLUDE = CmfcIFrameOnlyManifest' "INCLUDE"

{-# COMPLETE
  CmfcIFrameOnlyManifest_EXCLUDE,
  CmfcIFrameOnlyManifest_INCLUDE,
  CmfcIFrameOnlyManifest'
  #-}

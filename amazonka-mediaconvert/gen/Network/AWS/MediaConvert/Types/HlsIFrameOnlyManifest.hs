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
-- Module      : Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
  ( HlsIFrameOnlyManifest
      ( ..,
        HlsIFrameOnlyManifest_EXCLUDE,
        HlsIFrameOnlyManifest_INCLUDE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Choose Include (INCLUDE) to have MediaConvert generate a child manifest
-- that lists only the I-frames for this rendition, in addition to your
-- regular manifest for this rendition. You might use this manifest as part
-- of a workflow that creates preview functions for your video.
-- MediaConvert adds both the I-frame only child manifest and the regular
-- child manifest to the parent manifest. When you don\'t need the I-frame
-- only child manifest, keep the default value Exclude (EXCLUDE).
newtype HlsIFrameOnlyManifest = HlsIFrameOnlyManifest'
  { fromHlsIFrameOnlyManifest ::
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

pattern HlsIFrameOnlyManifest_EXCLUDE :: HlsIFrameOnlyManifest
pattern HlsIFrameOnlyManifest_EXCLUDE = HlsIFrameOnlyManifest' "EXCLUDE"

pattern HlsIFrameOnlyManifest_INCLUDE :: HlsIFrameOnlyManifest
pattern HlsIFrameOnlyManifest_INCLUDE = HlsIFrameOnlyManifest' "INCLUDE"

{-# COMPLETE
  HlsIFrameOnlyManifest_EXCLUDE,
  HlsIFrameOnlyManifest_INCLUDE,
  HlsIFrameOnlyManifest'
  #-}

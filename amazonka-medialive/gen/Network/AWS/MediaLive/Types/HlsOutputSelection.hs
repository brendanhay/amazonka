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

import qualified Network.AWS.Prelude as Prelude

-- | Hls Output Selection
newtype HlsOutputSelection = HlsOutputSelection'
  { fromHlsOutputSelection ::
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

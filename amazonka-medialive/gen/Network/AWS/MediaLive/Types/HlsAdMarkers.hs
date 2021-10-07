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
-- Module      : Network.AWS.MediaLive.Types.HlsAdMarkers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsAdMarkers
  ( HlsAdMarkers
      ( ..,
        HlsAdMarkers_ADOBE,
        HlsAdMarkers_ELEMENTAL,
        HlsAdMarkers_ELEMENTAL_SCTE35
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Hls Ad Markers
newtype HlsAdMarkers = HlsAdMarkers'
  { fromHlsAdMarkers ::
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

pattern HlsAdMarkers_ADOBE :: HlsAdMarkers
pattern HlsAdMarkers_ADOBE = HlsAdMarkers' "ADOBE"

pattern HlsAdMarkers_ELEMENTAL :: HlsAdMarkers
pattern HlsAdMarkers_ELEMENTAL = HlsAdMarkers' "ELEMENTAL"

pattern HlsAdMarkers_ELEMENTAL_SCTE35 :: HlsAdMarkers
pattern HlsAdMarkers_ELEMENTAL_SCTE35 = HlsAdMarkers' "ELEMENTAL_SCTE35"

{-# COMPLETE
  HlsAdMarkers_ADOBE,
  HlsAdMarkers_ELEMENTAL,
  HlsAdMarkers_ELEMENTAL_SCTE35,
  HlsAdMarkers'
  #-}

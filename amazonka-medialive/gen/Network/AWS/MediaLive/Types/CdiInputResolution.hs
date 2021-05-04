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
-- Module      : Network.AWS.MediaLive.Types.CdiInputResolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CdiInputResolution
  ( CdiInputResolution
      ( ..,
        CdiInputResolution_FHD,
        CdiInputResolution_HD,
        CdiInputResolution_SD,
        CdiInputResolution_UHD
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Maximum CDI input resolution; SD is 480i and 576i up to 30
-- frames-per-second (fps), HD is 720p up to 60 fps \/ 1080i up to 30 fps,
-- FHD is 1080p up to 60 fps, UHD is 2160p up to 60 fps
newtype CdiInputResolution = CdiInputResolution'
  { fromCdiInputResolution ::
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

pattern CdiInputResolution_FHD :: CdiInputResolution
pattern CdiInputResolution_FHD = CdiInputResolution' "FHD"

pattern CdiInputResolution_HD :: CdiInputResolution
pattern CdiInputResolution_HD = CdiInputResolution' "HD"

pattern CdiInputResolution_SD :: CdiInputResolution
pattern CdiInputResolution_SD = CdiInputResolution' "SD"

pattern CdiInputResolution_UHD :: CdiInputResolution
pattern CdiInputResolution_UHD = CdiInputResolution' "UHD"

{-# COMPLETE
  CdiInputResolution_FHD,
  CdiInputResolution_HD,
  CdiInputResolution_SD,
  CdiInputResolution_UHD,
  CdiInputResolution'
  #-}

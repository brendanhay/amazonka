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
-- Module      : Amazonka.MediaLive.Types.CdiInputResolution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CdiInputResolution
  ( CdiInputResolution
      ( ..,
        CdiInputResolution_FHD,
        CdiInputResolution_HD,
        CdiInputResolution_SD,
        CdiInputResolution_UHD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Maximum CDI input resolution; SD is 480i and 576i up to 30
-- frames-per-second (fps), HD is 720p up to 60 fps \/ 1080i up to 30 fps,
-- FHD is 1080p up to 60 fps, UHD is 2160p up to 60 fps
newtype CdiInputResolution = CdiInputResolution'
  { fromCdiInputResolution ::
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

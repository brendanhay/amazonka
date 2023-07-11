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
-- Module      : Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
  ( Ac3DynamicRangeCompressionProfile
      ( ..,
        Ac3DynamicRangeCompressionProfile_FILM_STANDARD,
        Ac3DynamicRangeCompressionProfile_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When you want to add Dolby dynamic range compression (DRC) signaling to
-- your output stream, we recommend that you use the mode-specific settings
-- instead of Dynamic range compression profile
-- (DynamicRangeCompressionProfile). The mode-specific settings are Dynamic
-- range compression profile, line mode (dynamicRangeCompressionLine) and
-- Dynamic range compression profile, RF mode (dynamicRangeCompressionRf).
-- Note that when you specify values for all three settings, MediaConvert
-- ignores the value of this setting in favor of the mode-specific
-- settings. If you do use this setting instead of the mode-specific
-- settings, choose None (NONE) to leave out DRC signaling. Keep the
-- default Film standard (FILM_STANDARD) to set the profile to Dolby\'s
-- film standard profile for all operating modes.
newtype Ac3DynamicRangeCompressionProfile = Ac3DynamicRangeCompressionProfile'
  { fromAc3DynamicRangeCompressionProfile ::
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

pattern Ac3DynamicRangeCompressionProfile_FILM_STANDARD :: Ac3DynamicRangeCompressionProfile
pattern Ac3DynamicRangeCompressionProfile_FILM_STANDARD = Ac3DynamicRangeCompressionProfile' "FILM_STANDARD"

pattern Ac3DynamicRangeCompressionProfile_NONE :: Ac3DynamicRangeCompressionProfile
pattern Ac3DynamicRangeCompressionProfile_NONE = Ac3DynamicRangeCompressionProfile' "NONE"

{-# COMPLETE
  Ac3DynamicRangeCompressionProfile_FILM_STANDARD,
  Ac3DynamicRangeCompressionProfile_NONE,
  Ac3DynamicRangeCompressionProfile'
  #-}

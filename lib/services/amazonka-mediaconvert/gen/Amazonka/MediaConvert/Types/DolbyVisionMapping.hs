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
-- Module      : Amazonka.MediaConvert.Types.DolbyVisionMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DolbyVisionMapping
  ( DolbyVisionMapping
      ( ..,
        DolbyVisionMapping_HDR10_1000,
        DolbyVisionMapping_HDR10_NOMAP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Required when you set Dolby Vision Profile to Profile 8.1. When you set
-- Content mapping to None, content mapping is not applied to the
-- HDR10-compatible signal. Depending on the source peak nit level,
-- clipping might occur on HDR devices without Dolby Vision. When you set
-- Content mapping to HDR10 1000, the transcoder creates a 1,000 nits peak
-- HDR10-compatible signal by applying static content mapping to the
-- source. This mode is speed-optimized for PQ10 sources with metadata that
-- is created from analysis. For graded Dolby Vision content, be aware that
-- creative intent might not be guaranteed with extreme 1,000 nits trims.
newtype DolbyVisionMapping = DolbyVisionMapping'
  { fromDolbyVisionMapping ::
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

pattern DolbyVisionMapping_HDR10_1000 :: DolbyVisionMapping
pattern DolbyVisionMapping_HDR10_1000 = DolbyVisionMapping' "HDR10_1000"

pattern DolbyVisionMapping_HDR10_NOMAP :: DolbyVisionMapping
pattern DolbyVisionMapping_HDR10_NOMAP = DolbyVisionMapping' "HDR10_NOMAP"

{-# COMPLETE
  DolbyVisionMapping_HDR10_1000,
  DolbyVisionMapping_HDR10_NOMAP,
  DolbyVisionMapping'
  #-}

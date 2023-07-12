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
-- Module      : Amazonka.MediaConvert.Types.CmafStreamInfResolution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafStreamInfResolution
  ( CmafStreamInfResolution
      ( ..,
        CmafStreamInfResolution_EXCLUDE,
        CmafStreamInfResolution_INCLUDE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
newtype CmafStreamInfResolution = CmafStreamInfResolution'
  { fromCmafStreamInfResolution ::
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

pattern CmafStreamInfResolution_EXCLUDE :: CmafStreamInfResolution
pattern CmafStreamInfResolution_EXCLUDE = CmafStreamInfResolution' "EXCLUDE"

pattern CmafStreamInfResolution_INCLUDE :: CmafStreamInfResolution
pattern CmafStreamInfResolution_INCLUDE = CmafStreamInfResolution' "INCLUDE"

{-# COMPLETE
  CmafStreamInfResolution_EXCLUDE,
  CmafStreamInfResolution_INCLUDE,
  CmafStreamInfResolution'
  #-}

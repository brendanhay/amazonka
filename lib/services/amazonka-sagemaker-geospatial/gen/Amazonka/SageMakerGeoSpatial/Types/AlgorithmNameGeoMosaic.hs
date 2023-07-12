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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameGeoMosaic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameGeoMosaic
  ( AlgorithmNameGeoMosaic
      ( ..,
        AlgorithmNameGeoMosaic_AVERAGE,
        AlgorithmNameGeoMosaic_BILINEAR,
        AlgorithmNameGeoMosaic_CUBIC,
        AlgorithmNameGeoMosaic_CUBICSPLINE,
        AlgorithmNameGeoMosaic_LANCZOS,
        AlgorithmNameGeoMosaic_MAX,
        AlgorithmNameGeoMosaic_MED,
        AlgorithmNameGeoMosaic_MIN,
        AlgorithmNameGeoMosaic_MODE,
        AlgorithmNameGeoMosaic_NEAR,
        AlgorithmNameGeoMosaic_Q1,
        AlgorithmNameGeoMosaic_Q3,
        AlgorithmNameGeoMosaic_RMS,
        AlgorithmNameGeoMosaic_SUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AlgorithmNameGeoMosaic = AlgorithmNameGeoMosaic'
  { fromAlgorithmNameGeoMosaic ::
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

pattern AlgorithmNameGeoMosaic_AVERAGE :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_AVERAGE = AlgorithmNameGeoMosaic' "AVERAGE"

pattern AlgorithmNameGeoMosaic_BILINEAR :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_BILINEAR = AlgorithmNameGeoMosaic' "BILINEAR"

pattern AlgorithmNameGeoMosaic_CUBIC :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_CUBIC = AlgorithmNameGeoMosaic' "CUBIC"

pattern AlgorithmNameGeoMosaic_CUBICSPLINE :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_CUBICSPLINE = AlgorithmNameGeoMosaic' "CUBICSPLINE"

pattern AlgorithmNameGeoMosaic_LANCZOS :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_LANCZOS = AlgorithmNameGeoMosaic' "LANCZOS"

pattern AlgorithmNameGeoMosaic_MAX :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_MAX = AlgorithmNameGeoMosaic' "MAX"

pattern AlgorithmNameGeoMosaic_MED :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_MED = AlgorithmNameGeoMosaic' "MED"

pattern AlgorithmNameGeoMosaic_MIN :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_MIN = AlgorithmNameGeoMosaic' "MIN"

pattern AlgorithmNameGeoMosaic_MODE :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_MODE = AlgorithmNameGeoMosaic' "MODE"

pattern AlgorithmNameGeoMosaic_NEAR :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_NEAR = AlgorithmNameGeoMosaic' "NEAR"

pattern AlgorithmNameGeoMosaic_Q1 :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_Q1 = AlgorithmNameGeoMosaic' "Q1"

pattern AlgorithmNameGeoMosaic_Q3 :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_Q3 = AlgorithmNameGeoMosaic' "Q3"

pattern AlgorithmNameGeoMosaic_RMS :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_RMS = AlgorithmNameGeoMosaic' "RMS"

pattern AlgorithmNameGeoMosaic_SUM :: AlgorithmNameGeoMosaic
pattern AlgorithmNameGeoMosaic_SUM = AlgorithmNameGeoMosaic' "SUM"

{-# COMPLETE
  AlgorithmNameGeoMosaic_AVERAGE,
  AlgorithmNameGeoMosaic_BILINEAR,
  AlgorithmNameGeoMosaic_CUBIC,
  AlgorithmNameGeoMosaic_CUBICSPLINE,
  AlgorithmNameGeoMosaic_LANCZOS,
  AlgorithmNameGeoMosaic_MAX,
  AlgorithmNameGeoMosaic_MED,
  AlgorithmNameGeoMosaic_MIN,
  AlgorithmNameGeoMosaic_MODE,
  AlgorithmNameGeoMosaic_NEAR,
  AlgorithmNameGeoMosaic_Q1,
  AlgorithmNameGeoMosaic_Q3,
  AlgorithmNameGeoMosaic_RMS,
  AlgorithmNameGeoMosaic_SUM,
  AlgorithmNameGeoMosaic'
  #-}

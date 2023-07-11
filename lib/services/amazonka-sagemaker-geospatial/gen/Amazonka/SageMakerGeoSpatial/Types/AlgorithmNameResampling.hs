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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameResampling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameResampling
  ( AlgorithmNameResampling
      ( ..,
        AlgorithmNameResampling_AVERAGE,
        AlgorithmNameResampling_BILINEAR,
        AlgorithmNameResampling_CUBIC,
        AlgorithmNameResampling_CUBICSPLINE,
        AlgorithmNameResampling_LANCZOS,
        AlgorithmNameResampling_MAX,
        AlgorithmNameResampling_MED,
        AlgorithmNameResampling_MIN,
        AlgorithmNameResampling_MODE,
        AlgorithmNameResampling_NEAR,
        AlgorithmNameResampling_Q1,
        AlgorithmNameResampling_Q3,
        AlgorithmNameResampling_RMS,
        AlgorithmNameResampling_SUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AlgorithmNameResampling = AlgorithmNameResampling'
  { fromAlgorithmNameResampling ::
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

pattern AlgorithmNameResampling_AVERAGE :: AlgorithmNameResampling
pattern AlgorithmNameResampling_AVERAGE = AlgorithmNameResampling' "AVERAGE"

pattern AlgorithmNameResampling_BILINEAR :: AlgorithmNameResampling
pattern AlgorithmNameResampling_BILINEAR = AlgorithmNameResampling' "BILINEAR"

pattern AlgorithmNameResampling_CUBIC :: AlgorithmNameResampling
pattern AlgorithmNameResampling_CUBIC = AlgorithmNameResampling' "CUBIC"

pattern AlgorithmNameResampling_CUBICSPLINE :: AlgorithmNameResampling
pattern AlgorithmNameResampling_CUBICSPLINE = AlgorithmNameResampling' "CUBICSPLINE"

pattern AlgorithmNameResampling_LANCZOS :: AlgorithmNameResampling
pattern AlgorithmNameResampling_LANCZOS = AlgorithmNameResampling' "LANCZOS"

pattern AlgorithmNameResampling_MAX :: AlgorithmNameResampling
pattern AlgorithmNameResampling_MAX = AlgorithmNameResampling' "MAX"

pattern AlgorithmNameResampling_MED :: AlgorithmNameResampling
pattern AlgorithmNameResampling_MED = AlgorithmNameResampling' "MED"

pattern AlgorithmNameResampling_MIN :: AlgorithmNameResampling
pattern AlgorithmNameResampling_MIN = AlgorithmNameResampling' "MIN"

pattern AlgorithmNameResampling_MODE :: AlgorithmNameResampling
pattern AlgorithmNameResampling_MODE = AlgorithmNameResampling' "MODE"

pattern AlgorithmNameResampling_NEAR :: AlgorithmNameResampling
pattern AlgorithmNameResampling_NEAR = AlgorithmNameResampling' "NEAR"

pattern AlgorithmNameResampling_Q1 :: AlgorithmNameResampling
pattern AlgorithmNameResampling_Q1 = AlgorithmNameResampling' "Q1"

pattern AlgorithmNameResampling_Q3 :: AlgorithmNameResampling
pattern AlgorithmNameResampling_Q3 = AlgorithmNameResampling' "Q3"

pattern AlgorithmNameResampling_RMS :: AlgorithmNameResampling
pattern AlgorithmNameResampling_RMS = AlgorithmNameResampling' "RMS"

pattern AlgorithmNameResampling_SUM :: AlgorithmNameResampling
pattern AlgorithmNameResampling_SUM = AlgorithmNameResampling' "SUM"

{-# COMPLETE
  AlgorithmNameResampling_AVERAGE,
  AlgorithmNameResampling_BILINEAR,
  AlgorithmNameResampling_CUBIC,
  AlgorithmNameResampling_CUBICSPLINE,
  AlgorithmNameResampling_LANCZOS,
  AlgorithmNameResampling_MAX,
  AlgorithmNameResampling_MED,
  AlgorithmNameResampling_MIN,
  AlgorithmNameResampling_MODE,
  AlgorithmNameResampling_NEAR,
  AlgorithmNameResampling_Q1,
  AlgorithmNameResampling_Q3,
  AlgorithmNameResampling_RMS,
  AlgorithmNameResampling_SUM,
  AlgorithmNameResampling'
  #-}

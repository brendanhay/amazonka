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
-- Module      : Amazonka.AutoScaling.Types.PredictiveScalingMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.PredictiveScalingMode
  ( PredictiveScalingMode
      ( ..,
        PredictiveScalingMode_ForecastAndScale,
        PredictiveScalingMode_ForecastOnly
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PredictiveScalingMode = PredictiveScalingMode'
  { fromPredictiveScalingMode ::
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

pattern PredictiveScalingMode_ForecastAndScale :: PredictiveScalingMode
pattern PredictiveScalingMode_ForecastAndScale = PredictiveScalingMode' "ForecastAndScale"

pattern PredictiveScalingMode_ForecastOnly :: PredictiveScalingMode
pattern PredictiveScalingMode_ForecastOnly = PredictiveScalingMode' "ForecastOnly"

{-# COMPLETE
  PredictiveScalingMode_ForecastAndScale,
  PredictiveScalingMode_ForecastOnly,
  PredictiveScalingMode'
  #-}

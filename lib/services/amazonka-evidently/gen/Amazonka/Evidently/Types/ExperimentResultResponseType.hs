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
-- Module      : Amazonka.Evidently.Types.ExperimentResultResponseType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ExperimentResultResponseType
  ( ExperimentResultResponseType
      ( ..,
        ExperimentResultResponseType_ConfidenceIntervalLowerBound,
        ExperimentResultResponseType_ConfidenceIntervalUpperBound,
        ExperimentResultResponseType_Mean,
        ExperimentResultResponseType_PValue,
        ExperimentResultResponseType_TreatmentEffect
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExperimentResultResponseType = ExperimentResultResponseType'
  { fromExperimentResultResponseType ::
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

pattern ExperimentResultResponseType_ConfidenceIntervalLowerBound :: ExperimentResultResponseType
pattern ExperimentResultResponseType_ConfidenceIntervalLowerBound = ExperimentResultResponseType' "ConfidenceIntervalLowerBound"

pattern ExperimentResultResponseType_ConfidenceIntervalUpperBound :: ExperimentResultResponseType
pattern ExperimentResultResponseType_ConfidenceIntervalUpperBound = ExperimentResultResponseType' "ConfidenceIntervalUpperBound"

pattern ExperimentResultResponseType_Mean :: ExperimentResultResponseType
pattern ExperimentResultResponseType_Mean = ExperimentResultResponseType' "Mean"

pattern ExperimentResultResponseType_PValue :: ExperimentResultResponseType
pattern ExperimentResultResponseType_PValue = ExperimentResultResponseType' "PValue"

pattern ExperimentResultResponseType_TreatmentEffect :: ExperimentResultResponseType
pattern ExperimentResultResponseType_TreatmentEffect = ExperimentResultResponseType' "TreatmentEffect"

{-# COMPLETE
  ExperimentResultResponseType_ConfidenceIntervalLowerBound,
  ExperimentResultResponseType_ConfidenceIntervalUpperBound,
  ExperimentResultResponseType_Mean,
  ExperimentResultResponseType_PValue,
  ExperimentResultResponseType_TreatmentEffect,
  ExperimentResultResponseType'
  #-}

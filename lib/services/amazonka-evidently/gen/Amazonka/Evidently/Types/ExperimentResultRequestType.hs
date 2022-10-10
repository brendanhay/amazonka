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
-- Module      : Amazonka.Evidently.Types.ExperimentResultRequestType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ExperimentResultRequestType
  ( ExperimentResultRequestType
      ( ..,
        ExperimentResultRequestType_BaseStat,
        ExperimentResultRequestType_ConfidenceInterval,
        ExperimentResultRequestType_PValue,
        ExperimentResultRequestType_TreatmentEffect
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ExperimentResultRequestType = ExperimentResultRequestType'
  { fromExperimentResultRequestType ::
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

pattern ExperimentResultRequestType_BaseStat :: ExperimentResultRequestType
pattern ExperimentResultRequestType_BaseStat = ExperimentResultRequestType' "BaseStat"

pattern ExperimentResultRequestType_ConfidenceInterval :: ExperimentResultRequestType
pattern ExperimentResultRequestType_ConfidenceInterval = ExperimentResultRequestType' "ConfidenceInterval"

pattern ExperimentResultRequestType_PValue :: ExperimentResultRequestType
pattern ExperimentResultRequestType_PValue = ExperimentResultRequestType' "PValue"

pattern ExperimentResultRequestType_TreatmentEffect :: ExperimentResultRequestType
pattern ExperimentResultRequestType_TreatmentEffect = ExperimentResultRequestType' "TreatmentEffect"

{-# COMPLETE
  ExperimentResultRequestType_BaseStat,
  ExperimentResultRequestType_ConfidenceInterval,
  ExperimentResultRequestType_PValue,
  ExperimentResultRequestType_TreatmentEffect,
  ExperimentResultRequestType'
  #-}

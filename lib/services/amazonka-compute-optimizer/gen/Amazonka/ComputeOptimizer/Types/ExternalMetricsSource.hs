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
-- Module      : Amazonka.ComputeOptimizer.Types.ExternalMetricsSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ExternalMetricsSource
  ( ExternalMetricsSource
      ( ..,
        ExternalMetricsSource_Datadog,
        ExternalMetricsSource_Dynatrace,
        ExternalMetricsSource_Instana,
        ExternalMetricsSource_NewRelic
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExternalMetricsSource = ExternalMetricsSource'
  { fromExternalMetricsSource ::
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

pattern ExternalMetricsSource_Datadog :: ExternalMetricsSource
pattern ExternalMetricsSource_Datadog = ExternalMetricsSource' "Datadog"

pattern ExternalMetricsSource_Dynatrace :: ExternalMetricsSource
pattern ExternalMetricsSource_Dynatrace = ExternalMetricsSource' "Dynatrace"

pattern ExternalMetricsSource_Instana :: ExternalMetricsSource
pattern ExternalMetricsSource_Instana = ExternalMetricsSource' "Instana"

pattern ExternalMetricsSource_NewRelic :: ExternalMetricsSource
pattern ExternalMetricsSource_NewRelic = ExternalMetricsSource' "NewRelic"

{-# COMPLETE
  ExternalMetricsSource_Datadog,
  ExternalMetricsSource_Dynatrace,
  ExternalMetricsSource_Instana,
  ExternalMetricsSource_NewRelic,
  ExternalMetricsSource'
  #-}

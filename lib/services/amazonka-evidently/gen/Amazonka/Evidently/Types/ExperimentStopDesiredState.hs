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
-- Module      : Amazonka.Evidently.Types.ExperimentStopDesiredState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ExperimentStopDesiredState
  ( ExperimentStopDesiredState
      ( ..,
        ExperimentStopDesiredState_CANCELLED,
        ExperimentStopDesiredState_COMPLETED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExperimentStopDesiredState = ExperimentStopDesiredState'
  { fromExperimentStopDesiredState ::
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

pattern ExperimentStopDesiredState_CANCELLED :: ExperimentStopDesiredState
pattern ExperimentStopDesiredState_CANCELLED = ExperimentStopDesiredState' "CANCELLED"

pattern ExperimentStopDesiredState_COMPLETED :: ExperimentStopDesiredState
pattern ExperimentStopDesiredState_COMPLETED = ExperimentStopDesiredState' "COMPLETED"

{-# COMPLETE
  ExperimentStopDesiredState_CANCELLED,
  ExperimentStopDesiredState_COMPLETED,
  ExperimentStopDesiredState'
  #-}

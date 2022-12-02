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
-- Module      : Amazonka.SageMaker.Types.TrainingJobSortByOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingJobSortByOptions
  ( TrainingJobSortByOptions
      ( ..,
        TrainingJobSortByOptions_CreationTime,
        TrainingJobSortByOptions_FinalObjectiveMetricValue,
        TrainingJobSortByOptions_Name,
        TrainingJobSortByOptions_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TrainingJobSortByOptions = TrainingJobSortByOptions'
  { fromTrainingJobSortByOptions ::
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

pattern TrainingJobSortByOptions_CreationTime :: TrainingJobSortByOptions
pattern TrainingJobSortByOptions_CreationTime = TrainingJobSortByOptions' "CreationTime"

pattern TrainingJobSortByOptions_FinalObjectiveMetricValue :: TrainingJobSortByOptions
pattern TrainingJobSortByOptions_FinalObjectiveMetricValue = TrainingJobSortByOptions' "FinalObjectiveMetricValue"

pattern TrainingJobSortByOptions_Name :: TrainingJobSortByOptions
pattern TrainingJobSortByOptions_Name = TrainingJobSortByOptions' "Name"

pattern TrainingJobSortByOptions_Status :: TrainingJobSortByOptions
pattern TrainingJobSortByOptions_Status = TrainingJobSortByOptions' "Status"

{-# COMPLETE
  TrainingJobSortByOptions_CreationTime,
  TrainingJobSortByOptions_FinalObjectiveMetricValue,
  TrainingJobSortByOptions_Name,
  TrainingJobSortByOptions_Status,
  TrainingJobSortByOptions'
  #-}

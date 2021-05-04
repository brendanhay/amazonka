{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobSortByOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobSortByOptions
  ( TrainingJobSortByOptions
      ( ..,
        TrainingJobSortByOptions_CreationTime,
        TrainingJobSortByOptions_FinalObjectiveMetricValue,
        TrainingJobSortByOptions_Name,
        TrainingJobSortByOptions_Status
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TrainingJobSortByOptions = TrainingJobSortByOptions'
  { fromTrainingJobSortByOptions ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

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
-- Module      : Amazonka.FraudDetector.Types.TrainingDataSourceEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TrainingDataSourceEnum
  ( TrainingDataSourceEnum
      ( ..,
        TrainingDataSourceEnum_EXTERNAL_EVENTS,
        TrainingDataSourceEnum_INGESTED_EVENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TrainingDataSourceEnum = TrainingDataSourceEnum'
  { fromTrainingDataSourceEnum ::
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

pattern TrainingDataSourceEnum_EXTERNAL_EVENTS :: TrainingDataSourceEnum
pattern TrainingDataSourceEnum_EXTERNAL_EVENTS = TrainingDataSourceEnum' "EXTERNAL_EVENTS"

pattern TrainingDataSourceEnum_INGESTED_EVENTS :: TrainingDataSourceEnum
pattern TrainingDataSourceEnum_INGESTED_EVENTS = TrainingDataSourceEnum' "INGESTED_EVENTS"

{-# COMPLETE
  TrainingDataSourceEnum_EXTERNAL_EVENTS,
  TrainingDataSourceEnum_INGESTED_EVENTS,
  TrainingDataSourceEnum'
  #-}

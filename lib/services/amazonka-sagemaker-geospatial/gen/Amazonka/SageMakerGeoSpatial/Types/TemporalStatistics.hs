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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.TemporalStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.TemporalStatistics
  ( TemporalStatistics
      ( ..,
        TemporalStatistics_MEAN,
        TemporalStatistics_MEDIAN,
        TemporalStatistics_STANDARD_DEVIATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TemporalStatistics = TemporalStatistics'
  { fromTemporalStatistics ::
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

pattern TemporalStatistics_MEAN :: TemporalStatistics
pattern TemporalStatistics_MEAN = TemporalStatistics' "MEAN"

pattern TemporalStatistics_MEDIAN :: TemporalStatistics
pattern TemporalStatistics_MEDIAN = TemporalStatistics' "MEDIAN"

pattern TemporalStatistics_STANDARD_DEVIATION :: TemporalStatistics
pattern TemporalStatistics_STANDARD_DEVIATION = TemporalStatistics' "STANDARD_DEVIATION"

{-# COMPLETE
  TemporalStatistics_MEAN,
  TemporalStatistics_MEDIAN,
  TemporalStatistics_STANDARD_DEVIATION,
  TemporalStatistics'
  #-}

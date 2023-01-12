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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ZonalStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ZonalStatistics
  ( ZonalStatistics
      ( ..,
        ZonalStatistics_MAX,
        ZonalStatistics_MEAN,
        ZonalStatistics_MEDIAN,
        ZonalStatistics_MIN,
        ZonalStatistics_STANDARD_DEVIATION,
        ZonalStatistics_SUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ZonalStatistics = ZonalStatistics'
  { fromZonalStatistics ::
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

pattern ZonalStatistics_MAX :: ZonalStatistics
pattern ZonalStatistics_MAX = ZonalStatistics' "MAX"

pattern ZonalStatistics_MEAN :: ZonalStatistics
pattern ZonalStatistics_MEAN = ZonalStatistics' "MEAN"

pattern ZonalStatistics_MEDIAN :: ZonalStatistics
pattern ZonalStatistics_MEDIAN = ZonalStatistics' "MEDIAN"

pattern ZonalStatistics_MIN :: ZonalStatistics
pattern ZonalStatistics_MIN = ZonalStatistics' "MIN"

pattern ZonalStatistics_STANDARD_DEVIATION :: ZonalStatistics
pattern ZonalStatistics_STANDARD_DEVIATION = ZonalStatistics' "STANDARD_DEVIATION"

pattern ZonalStatistics_SUM :: ZonalStatistics
pattern ZonalStatistics_SUM = ZonalStatistics' "SUM"

{-# COMPLETE
  ZonalStatistics_MAX,
  ZonalStatistics_MEAN,
  ZonalStatistics_MEDIAN,
  ZonalStatistics_MIN,
  ZonalStatistics_STANDARD_DEVIATION,
  ZonalStatistics_SUM,
  ZonalStatistics'
  #-}

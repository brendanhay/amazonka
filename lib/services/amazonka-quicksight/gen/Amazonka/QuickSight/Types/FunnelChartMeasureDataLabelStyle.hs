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
-- Module      : Amazonka.QuickSight.Types.FunnelChartMeasureDataLabelStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FunnelChartMeasureDataLabelStyle
  ( FunnelChartMeasureDataLabelStyle
      ( ..,
        FunnelChartMeasureDataLabelStyle_PERCENTAGE_BY_FIRST_STAGE,
        FunnelChartMeasureDataLabelStyle_PERCENTAGE_BY_PREVIOUS_STAGE,
        FunnelChartMeasureDataLabelStyle_VALUE_AND_PERCENTAGE_BY_FIRST_STAGE,
        FunnelChartMeasureDataLabelStyle_VALUE_AND_PERCENTAGE_BY_PREVIOUS_STAGE,
        FunnelChartMeasureDataLabelStyle_VALUE_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FunnelChartMeasureDataLabelStyle = FunnelChartMeasureDataLabelStyle'
  { fromFunnelChartMeasureDataLabelStyle ::
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

pattern FunnelChartMeasureDataLabelStyle_PERCENTAGE_BY_FIRST_STAGE :: FunnelChartMeasureDataLabelStyle
pattern FunnelChartMeasureDataLabelStyle_PERCENTAGE_BY_FIRST_STAGE = FunnelChartMeasureDataLabelStyle' "PERCENTAGE_BY_FIRST_STAGE"

pattern FunnelChartMeasureDataLabelStyle_PERCENTAGE_BY_PREVIOUS_STAGE :: FunnelChartMeasureDataLabelStyle
pattern FunnelChartMeasureDataLabelStyle_PERCENTAGE_BY_PREVIOUS_STAGE = FunnelChartMeasureDataLabelStyle' "PERCENTAGE_BY_PREVIOUS_STAGE"

pattern FunnelChartMeasureDataLabelStyle_VALUE_AND_PERCENTAGE_BY_FIRST_STAGE :: FunnelChartMeasureDataLabelStyle
pattern FunnelChartMeasureDataLabelStyle_VALUE_AND_PERCENTAGE_BY_FIRST_STAGE = FunnelChartMeasureDataLabelStyle' "VALUE_AND_PERCENTAGE_BY_FIRST_STAGE"

pattern FunnelChartMeasureDataLabelStyle_VALUE_AND_PERCENTAGE_BY_PREVIOUS_STAGE :: FunnelChartMeasureDataLabelStyle
pattern FunnelChartMeasureDataLabelStyle_VALUE_AND_PERCENTAGE_BY_PREVIOUS_STAGE = FunnelChartMeasureDataLabelStyle' "VALUE_AND_PERCENTAGE_BY_PREVIOUS_STAGE"

pattern FunnelChartMeasureDataLabelStyle_VALUE_ONLY :: FunnelChartMeasureDataLabelStyle
pattern FunnelChartMeasureDataLabelStyle_VALUE_ONLY = FunnelChartMeasureDataLabelStyle' "VALUE_ONLY"

{-# COMPLETE
  FunnelChartMeasureDataLabelStyle_PERCENTAGE_BY_FIRST_STAGE,
  FunnelChartMeasureDataLabelStyle_PERCENTAGE_BY_PREVIOUS_STAGE,
  FunnelChartMeasureDataLabelStyle_VALUE_AND_PERCENTAGE_BY_FIRST_STAGE,
  FunnelChartMeasureDataLabelStyle_VALUE_AND_PERCENTAGE_BY_PREVIOUS_STAGE,
  FunnelChartMeasureDataLabelStyle_VALUE_ONLY,
  FunnelChartMeasureDataLabelStyle'
  #-}

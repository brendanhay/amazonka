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
-- Module      : Amazonka.QuickSight.Types.LineChartMarkerShape
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartMarkerShape
  ( LineChartMarkerShape
      ( ..,
        LineChartMarkerShape_CIRCLE,
        LineChartMarkerShape_DIAMOND,
        LineChartMarkerShape_ROUNDED_SQUARE,
        LineChartMarkerShape_SQUARE,
        LineChartMarkerShape_TRIANGLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LineChartMarkerShape = LineChartMarkerShape'
  { fromLineChartMarkerShape ::
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

pattern LineChartMarkerShape_CIRCLE :: LineChartMarkerShape
pattern LineChartMarkerShape_CIRCLE = LineChartMarkerShape' "CIRCLE"

pattern LineChartMarkerShape_DIAMOND :: LineChartMarkerShape
pattern LineChartMarkerShape_DIAMOND = LineChartMarkerShape' "DIAMOND"

pattern LineChartMarkerShape_ROUNDED_SQUARE :: LineChartMarkerShape
pattern LineChartMarkerShape_ROUNDED_SQUARE = LineChartMarkerShape' "ROUNDED_SQUARE"

pattern LineChartMarkerShape_SQUARE :: LineChartMarkerShape
pattern LineChartMarkerShape_SQUARE = LineChartMarkerShape' "SQUARE"

pattern LineChartMarkerShape_TRIANGLE :: LineChartMarkerShape
pattern LineChartMarkerShape_TRIANGLE = LineChartMarkerShape' "TRIANGLE"

{-# COMPLETE
  LineChartMarkerShape_CIRCLE,
  LineChartMarkerShape_DIAMOND,
  LineChartMarkerShape_ROUNDED_SQUARE,
  LineChartMarkerShape_SQUARE,
  LineChartMarkerShape_TRIANGLE,
  LineChartMarkerShape'
  #-}

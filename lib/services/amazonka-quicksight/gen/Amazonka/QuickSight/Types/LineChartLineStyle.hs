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
-- Module      : Amazonka.QuickSight.Types.LineChartLineStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartLineStyle
  ( LineChartLineStyle
      ( ..,
        LineChartLineStyle_DASHED,
        LineChartLineStyle_DOTTED,
        LineChartLineStyle_SOLID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LineChartLineStyle = LineChartLineStyle'
  { fromLineChartLineStyle ::
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

pattern LineChartLineStyle_DASHED :: LineChartLineStyle
pattern LineChartLineStyle_DASHED = LineChartLineStyle' "DASHED"

pattern LineChartLineStyle_DOTTED :: LineChartLineStyle
pattern LineChartLineStyle_DOTTED = LineChartLineStyle' "DOTTED"

pattern LineChartLineStyle_SOLID :: LineChartLineStyle
pattern LineChartLineStyle_SOLID = LineChartLineStyle' "SOLID"

{-# COMPLETE
  LineChartLineStyle_DASHED,
  LineChartLineStyle_DOTTED,
  LineChartLineStyle_SOLID,
  LineChartLineStyle'
  #-}

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
-- Module      : Amazonka.QuickSight.Types.BoxPlotFillStyle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BoxPlotFillStyle
  ( BoxPlotFillStyle
      ( ..,
        BoxPlotFillStyle_SOLID,
        BoxPlotFillStyle_TRANSPARENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BoxPlotFillStyle = BoxPlotFillStyle'
  { fromBoxPlotFillStyle ::
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

pattern BoxPlotFillStyle_SOLID :: BoxPlotFillStyle
pattern BoxPlotFillStyle_SOLID = BoxPlotFillStyle' "SOLID"

pattern BoxPlotFillStyle_TRANSPARENT :: BoxPlotFillStyle
pattern BoxPlotFillStyle_TRANSPARENT = BoxPlotFillStyle' "TRANSPARENT"

{-# COMPLETE
  BoxPlotFillStyle_SOLID,
  BoxPlotFillStyle_TRANSPARENT,
  BoxPlotFillStyle'
  #-}

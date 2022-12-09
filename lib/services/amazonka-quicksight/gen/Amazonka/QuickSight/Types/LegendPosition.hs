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
-- Module      : Amazonka.QuickSight.Types.LegendPosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LegendPosition
  ( LegendPosition
      ( ..,
        LegendPosition_AUTO,
        LegendPosition_BOTTOM,
        LegendPosition_RIGHT,
        LegendPosition_TOP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LegendPosition = LegendPosition'
  { fromLegendPosition ::
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

pattern LegendPosition_AUTO :: LegendPosition
pattern LegendPosition_AUTO = LegendPosition' "AUTO"

pattern LegendPosition_BOTTOM :: LegendPosition
pattern LegendPosition_BOTTOM = LegendPosition' "BOTTOM"

pattern LegendPosition_RIGHT :: LegendPosition
pattern LegendPosition_RIGHT = LegendPosition' "RIGHT"

pattern LegendPosition_TOP :: LegendPosition
pattern LegendPosition_TOP = LegendPosition' "TOP"

{-# COMPLETE
  LegendPosition_AUTO,
  LegendPosition_BOTTOM,
  LegendPosition_RIGHT,
  LegendPosition_TOP,
  LegendPosition'
  #-}

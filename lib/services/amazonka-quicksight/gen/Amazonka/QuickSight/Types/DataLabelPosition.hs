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
-- Module      : Amazonka.QuickSight.Types.DataLabelPosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataLabelPosition
  ( DataLabelPosition
      ( ..,
        DataLabelPosition_BOTTOM,
        DataLabelPosition_INSIDE,
        DataLabelPosition_LEFT,
        DataLabelPosition_OUTSIDE,
        DataLabelPosition_RIGHT,
        DataLabelPosition_TOP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataLabelPosition = DataLabelPosition'
  { fromDataLabelPosition ::
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

pattern DataLabelPosition_BOTTOM :: DataLabelPosition
pattern DataLabelPosition_BOTTOM = DataLabelPosition' "BOTTOM"

pattern DataLabelPosition_INSIDE :: DataLabelPosition
pattern DataLabelPosition_INSIDE = DataLabelPosition' "INSIDE"

pattern DataLabelPosition_LEFT :: DataLabelPosition
pattern DataLabelPosition_LEFT = DataLabelPosition' "LEFT"

pattern DataLabelPosition_OUTSIDE :: DataLabelPosition
pattern DataLabelPosition_OUTSIDE = DataLabelPosition' "OUTSIDE"

pattern DataLabelPosition_RIGHT :: DataLabelPosition
pattern DataLabelPosition_RIGHT = DataLabelPosition' "RIGHT"

pattern DataLabelPosition_TOP :: DataLabelPosition
pattern DataLabelPosition_TOP = DataLabelPosition' "TOP"

{-# COMPLETE
  DataLabelPosition_BOTTOM,
  DataLabelPosition_INSIDE,
  DataLabelPosition_LEFT,
  DataLabelPosition_OUTSIDE,
  DataLabelPosition_RIGHT,
  DataLabelPosition_TOP,
  DataLabelPosition'
  #-}

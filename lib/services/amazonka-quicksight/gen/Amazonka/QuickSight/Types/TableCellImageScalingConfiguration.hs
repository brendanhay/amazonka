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
-- Module      : Amazonka.QuickSight.Types.TableCellImageScalingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableCellImageScalingConfiguration
  ( TableCellImageScalingConfiguration
      ( ..,
        TableCellImageScalingConfiguration_DO_NOT_SCALE,
        TableCellImageScalingConfiguration_FIT_TO_CELL_HEIGHT,
        TableCellImageScalingConfiguration_FIT_TO_CELL_WIDTH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TableCellImageScalingConfiguration = TableCellImageScalingConfiguration'
  { fromTableCellImageScalingConfiguration ::
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

pattern TableCellImageScalingConfiguration_DO_NOT_SCALE :: TableCellImageScalingConfiguration
pattern TableCellImageScalingConfiguration_DO_NOT_SCALE = TableCellImageScalingConfiguration' "DO_NOT_SCALE"

pattern TableCellImageScalingConfiguration_FIT_TO_CELL_HEIGHT :: TableCellImageScalingConfiguration
pattern TableCellImageScalingConfiguration_FIT_TO_CELL_HEIGHT = TableCellImageScalingConfiguration' "FIT_TO_CELL_HEIGHT"

pattern TableCellImageScalingConfiguration_FIT_TO_CELL_WIDTH :: TableCellImageScalingConfiguration
pattern TableCellImageScalingConfiguration_FIT_TO_CELL_WIDTH = TableCellImageScalingConfiguration' "FIT_TO_CELL_WIDTH"

{-# COMPLETE
  TableCellImageScalingConfiguration_DO_NOT_SCALE,
  TableCellImageScalingConfiguration_FIT_TO_CELL_HEIGHT,
  TableCellImageScalingConfiguration_FIT_TO_CELL_WIDTH,
  TableCellImageScalingConfiguration'
  #-}

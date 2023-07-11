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
-- Module      : Amazonka.QuickSight.Types.SheetControlSliderType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetControlSliderType
  ( SheetControlSliderType
      ( ..,
        SheetControlSliderType_RANGE,
        SheetControlSliderType_SINGLE_POINT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SheetControlSliderType = SheetControlSliderType'
  { fromSheetControlSliderType ::
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

pattern SheetControlSliderType_RANGE :: SheetControlSliderType
pattern SheetControlSliderType_RANGE = SheetControlSliderType' "RANGE"

pattern SheetControlSliderType_SINGLE_POINT :: SheetControlSliderType
pattern SheetControlSliderType_SINGLE_POINT = SheetControlSliderType' "SINGLE_POINT"

{-# COMPLETE
  SheetControlSliderType_RANGE,
  SheetControlSliderType_SINGLE_POINT,
  SheetControlSliderType'
  #-}

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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineLabelHorizontalPosition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineLabelHorizontalPosition
  ( ReferenceLineLabelHorizontalPosition
      ( ..,
        ReferenceLineLabelHorizontalPosition_CENTER,
        ReferenceLineLabelHorizontalPosition_LEFT,
        ReferenceLineLabelHorizontalPosition_RIGHT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReferenceLineLabelHorizontalPosition = ReferenceLineLabelHorizontalPosition'
  { fromReferenceLineLabelHorizontalPosition ::
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

pattern ReferenceLineLabelHorizontalPosition_CENTER :: ReferenceLineLabelHorizontalPosition
pattern ReferenceLineLabelHorizontalPosition_CENTER = ReferenceLineLabelHorizontalPosition' "CENTER"

pattern ReferenceLineLabelHorizontalPosition_LEFT :: ReferenceLineLabelHorizontalPosition
pattern ReferenceLineLabelHorizontalPosition_LEFT = ReferenceLineLabelHorizontalPosition' "LEFT"

pattern ReferenceLineLabelHorizontalPosition_RIGHT :: ReferenceLineLabelHorizontalPosition
pattern ReferenceLineLabelHorizontalPosition_RIGHT = ReferenceLineLabelHorizontalPosition' "RIGHT"

{-# COMPLETE
  ReferenceLineLabelHorizontalPosition_CENTER,
  ReferenceLineLabelHorizontalPosition_LEFT,
  ReferenceLineLabelHorizontalPosition_RIGHT,
  ReferenceLineLabelHorizontalPosition'
  #-}

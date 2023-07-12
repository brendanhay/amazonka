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
-- Module      : Amazonka.QuickSight.Types.PivotTableConditionalFormattingScopeRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableConditionalFormattingScopeRole
  ( PivotTableConditionalFormattingScopeRole
      ( ..,
        PivotTableConditionalFormattingScopeRole_FIELD,
        PivotTableConditionalFormattingScopeRole_FIELD_TOTAL,
        PivotTableConditionalFormattingScopeRole_GRAND_TOTAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PivotTableConditionalFormattingScopeRole = PivotTableConditionalFormattingScopeRole'
  { fromPivotTableConditionalFormattingScopeRole ::
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

pattern PivotTableConditionalFormattingScopeRole_FIELD :: PivotTableConditionalFormattingScopeRole
pattern PivotTableConditionalFormattingScopeRole_FIELD = PivotTableConditionalFormattingScopeRole' "FIELD"

pattern PivotTableConditionalFormattingScopeRole_FIELD_TOTAL :: PivotTableConditionalFormattingScopeRole
pattern PivotTableConditionalFormattingScopeRole_FIELD_TOTAL = PivotTableConditionalFormattingScopeRole' "FIELD_TOTAL"

pattern PivotTableConditionalFormattingScopeRole_GRAND_TOTAL :: PivotTableConditionalFormattingScopeRole
pattern PivotTableConditionalFormattingScopeRole_GRAND_TOTAL = PivotTableConditionalFormattingScopeRole' "GRAND_TOTAL"

{-# COMPLETE
  PivotTableConditionalFormattingScopeRole_FIELD,
  PivotTableConditionalFormattingScopeRole_FIELD_TOTAL,
  PivotTableConditionalFormattingScopeRole_GRAND_TOTAL,
  PivotTableConditionalFormattingScopeRole'
  #-}

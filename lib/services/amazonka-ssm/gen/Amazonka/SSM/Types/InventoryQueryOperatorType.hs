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
-- Module      : Amazonka.SSM.Types.InventoryQueryOperatorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InventoryQueryOperatorType
  ( InventoryQueryOperatorType
      ( ..,
        InventoryQueryOperatorType_BeginWith,
        InventoryQueryOperatorType_Equal,
        InventoryQueryOperatorType_Exists,
        InventoryQueryOperatorType_GreaterThan,
        InventoryQueryOperatorType_LessThan,
        InventoryQueryOperatorType_NotEqual
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InventoryQueryOperatorType = InventoryQueryOperatorType'
  { fromInventoryQueryOperatorType ::
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

pattern InventoryQueryOperatorType_BeginWith :: InventoryQueryOperatorType
pattern InventoryQueryOperatorType_BeginWith = InventoryQueryOperatorType' "BeginWith"

pattern InventoryQueryOperatorType_Equal :: InventoryQueryOperatorType
pattern InventoryQueryOperatorType_Equal = InventoryQueryOperatorType' "Equal"

pattern InventoryQueryOperatorType_Exists :: InventoryQueryOperatorType
pattern InventoryQueryOperatorType_Exists = InventoryQueryOperatorType' "Exists"

pattern InventoryQueryOperatorType_GreaterThan :: InventoryQueryOperatorType
pattern InventoryQueryOperatorType_GreaterThan = InventoryQueryOperatorType' "GreaterThan"

pattern InventoryQueryOperatorType_LessThan :: InventoryQueryOperatorType
pattern InventoryQueryOperatorType_LessThan = InventoryQueryOperatorType' "LessThan"

pattern InventoryQueryOperatorType_NotEqual :: InventoryQueryOperatorType
pattern InventoryQueryOperatorType_NotEqual = InventoryQueryOperatorType' "NotEqual"

{-# COMPLETE
  InventoryQueryOperatorType_BeginWith,
  InventoryQueryOperatorType_Equal,
  InventoryQueryOperatorType_Exists,
  InventoryQueryOperatorType_GreaterThan,
  InventoryQueryOperatorType_LessThan,
  InventoryQueryOperatorType_NotEqual,
  InventoryQueryOperatorType'
  #-}

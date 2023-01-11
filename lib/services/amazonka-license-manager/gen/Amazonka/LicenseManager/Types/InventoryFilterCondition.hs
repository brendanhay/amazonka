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
-- Module      : Amazonka.LicenseManager.Types.InventoryFilterCondition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.InventoryFilterCondition
  ( InventoryFilterCondition
      ( ..,
        InventoryFilterCondition_BEGINS_WITH,
        InventoryFilterCondition_CONTAINS,
        InventoryFilterCondition_EQUALS,
        InventoryFilterCondition_NOT_EQUALS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InventoryFilterCondition = InventoryFilterCondition'
  { fromInventoryFilterCondition ::
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

pattern InventoryFilterCondition_BEGINS_WITH :: InventoryFilterCondition
pattern InventoryFilterCondition_BEGINS_WITH = InventoryFilterCondition' "BEGINS_WITH"

pattern InventoryFilterCondition_CONTAINS :: InventoryFilterCondition
pattern InventoryFilterCondition_CONTAINS = InventoryFilterCondition' "CONTAINS"

pattern InventoryFilterCondition_EQUALS :: InventoryFilterCondition
pattern InventoryFilterCondition_EQUALS = InventoryFilterCondition' "EQUALS"

pattern InventoryFilterCondition_NOT_EQUALS :: InventoryFilterCondition
pattern InventoryFilterCondition_NOT_EQUALS = InventoryFilterCondition' "NOT_EQUALS"

{-# COMPLETE
  InventoryFilterCondition_BEGINS_WITH,
  InventoryFilterCondition_CONTAINS,
  InventoryFilterCondition_EQUALS,
  InventoryFilterCondition_NOT_EQUALS,
  InventoryFilterCondition'
  #-}

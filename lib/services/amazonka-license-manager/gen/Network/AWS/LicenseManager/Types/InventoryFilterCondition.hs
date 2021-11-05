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
-- Module      : Network.AWS.LicenseManager.Types.InventoryFilterCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LicenseManager.Types.InventoryFilterCondition
  ( InventoryFilterCondition
      ( ..,
        InventoryFilterCondition_BEGINS_WITH,
        InventoryFilterCondition_CONTAINS,
        InventoryFilterCondition_EQUALS,
        InventoryFilterCondition_NOT_EQUALS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InventoryFilterCondition = InventoryFilterCondition'
  { fromInventoryFilterCondition ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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

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
-- Module      : Network.AWS.SSM.Types.InventoryQueryOperatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryQueryOperatorType
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

import qualified Network.AWS.Core as Core

newtype InventoryQueryOperatorType = InventoryQueryOperatorType'
  { fromInventoryQueryOperatorType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

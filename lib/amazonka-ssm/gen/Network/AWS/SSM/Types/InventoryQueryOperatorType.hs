{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryQueryOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InventoryQueryOperatorType
  ( InventoryQueryOperatorType
    ( InventoryQueryOperatorType'
    , InventoryQueryOperatorTypeEqual
    , InventoryQueryOperatorTypeNotEqual
    , InventoryQueryOperatorTypeBeginWith
    , InventoryQueryOperatorTypeLessThan
    , InventoryQueryOperatorTypeGreaterThan
    , InventoryQueryOperatorTypeExists
    , fromInventoryQueryOperatorType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InventoryQueryOperatorType = InventoryQueryOperatorType'{fromInventoryQueryOperatorType
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern InventoryQueryOperatorTypeEqual :: InventoryQueryOperatorType
pattern InventoryQueryOperatorTypeEqual = InventoryQueryOperatorType' "Equal"

pattern InventoryQueryOperatorTypeNotEqual :: InventoryQueryOperatorType
pattern InventoryQueryOperatorTypeNotEqual = InventoryQueryOperatorType' "NotEqual"

pattern InventoryQueryOperatorTypeBeginWith :: InventoryQueryOperatorType
pattern InventoryQueryOperatorTypeBeginWith = InventoryQueryOperatorType' "BeginWith"

pattern InventoryQueryOperatorTypeLessThan :: InventoryQueryOperatorType
pattern InventoryQueryOperatorTypeLessThan = InventoryQueryOperatorType' "LessThan"

pattern InventoryQueryOperatorTypeGreaterThan :: InventoryQueryOperatorType
pattern InventoryQueryOperatorTypeGreaterThan = InventoryQueryOperatorType' "GreaterThan"

pattern InventoryQueryOperatorTypeExists :: InventoryQueryOperatorType
pattern InventoryQueryOperatorTypeExists = InventoryQueryOperatorType' "Exists"

{-# COMPLETE 
  InventoryQueryOperatorTypeEqual,

  InventoryQueryOperatorTypeNotEqual,

  InventoryQueryOperatorTypeBeginWith,

  InventoryQueryOperatorTypeLessThan,

  InventoryQueryOperatorTypeGreaterThan,

  InventoryQueryOperatorTypeExists,
  InventoryQueryOperatorType'
  #-}

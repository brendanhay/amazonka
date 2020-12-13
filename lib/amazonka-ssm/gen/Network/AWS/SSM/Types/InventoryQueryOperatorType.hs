{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InventoryQueryOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InventoryQueryOperatorType
  ( InventoryQueryOperatorType
      ( InventoryQueryOperatorType',
        IQOTEqual,
        IQOTNotEqual,
        IQOTBeginWith,
        IQOTLessThan,
        IQOTGreaterThan,
        IQOTExists
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InventoryQueryOperatorType = InventoryQueryOperatorType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern IQOTEqual :: InventoryQueryOperatorType
pattern IQOTEqual = InventoryQueryOperatorType' "Equal"

pattern IQOTNotEqual :: InventoryQueryOperatorType
pattern IQOTNotEqual = InventoryQueryOperatorType' "NotEqual"

pattern IQOTBeginWith :: InventoryQueryOperatorType
pattern IQOTBeginWith = InventoryQueryOperatorType' "BeginWith"

pattern IQOTLessThan :: InventoryQueryOperatorType
pattern IQOTLessThan = InventoryQueryOperatorType' "LessThan"

pattern IQOTGreaterThan :: InventoryQueryOperatorType
pattern IQOTGreaterThan = InventoryQueryOperatorType' "GreaterThan"

pattern IQOTExists :: InventoryQueryOperatorType
pattern IQOTExists = InventoryQueryOperatorType' "Exists"

{-# COMPLETE
  IQOTEqual,
  IQOTNotEqual,
  IQOTBeginWith,
  IQOTLessThan,
  IQOTGreaterThan,
  IQOTExists,
  InventoryQueryOperatorType'
  #-}

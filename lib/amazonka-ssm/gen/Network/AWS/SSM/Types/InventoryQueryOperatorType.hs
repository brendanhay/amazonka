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
        IQOTBeginWith,
        IQOTEqual,
        IQOTExists,
        IQOTGreaterThan,
        IQOTLessThan,
        IQOTNotEqual
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

pattern IQOTBeginWith :: InventoryQueryOperatorType
pattern IQOTBeginWith = InventoryQueryOperatorType' "BeginWith"

pattern IQOTEqual :: InventoryQueryOperatorType
pattern IQOTEqual = InventoryQueryOperatorType' "Equal"

pattern IQOTExists :: InventoryQueryOperatorType
pattern IQOTExists = InventoryQueryOperatorType' "Exists"

pattern IQOTGreaterThan :: InventoryQueryOperatorType
pattern IQOTGreaterThan = InventoryQueryOperatorType' "GreaterThan"

pattern IQOTLessThan :: InventoryQueryOperatorType
pattern IQOTLessThan = InventoryQueryOperatorType' "LessThan"

pattern IQOTNotEqual :: InventoryQueryOperatorType
pattern IQOTNotEqual = InventoryQueryOperatorType' "NotEqual"

{-# COMPLETE
  IQOTBeginWith,
  IQOTEqual,
  IQOTExists,
  IQOTGreaterThan,
  IQOTLessThan,
  IQOTNotEqual,
  InventoryQueryOperatorType'
  #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationFilterOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationFilterOperatorType
  ( AssociationFilterOperatorType
      ( AssociationFilterOperatorType',
        AFOTEqual,
        AFOTGreaterThan,
        AFOTLessThan
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AssociationFilterOperatorType = AssociationFilterOperatorType' Lude.Text
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

pattern AFOTEqual :: AssociationFilterOperatorType
pattern AFOTEqual = AssociationFilterOperatorType' "EQUAL"

pattern AFOTGreaterThan :: AssociationFilterOperatorType
pattern AFOTGreaterThan = AssociationFilterOperatorType' "GREATER_THAN"

pattern AFOTLessThan :: AssociationFilterOperatorType
pattern AFOTLessThan = AssociationFilterOperatorType' "LESS_THAN"

{-# COMPLETE
  AFOTEqual,
  AFOTGreaterThan,
  AFOTLessThan,
  AssociationFilterOperatorType'
  #-}

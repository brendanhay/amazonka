{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        AssociationFilterOperatorTypeEqual,
        AssociationFilterOperatorTypeLessThan,
        AssociationFilterOperatorTypeGreaterThan,
        fromAssociationFilterOperatorType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AssociationFilterOperatorType = AssociationFilterOperatorType'
  { fromAssociationFilterOperatorType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AssociationFilterOperatorTypeEqual :: AssociationFilterOperatorType
pattern AssociationFilterOperatorTypeEqual = AssociationFilterOperatorType' "EQUAL"

pattern AssociationFilterOperatorTypeLessThan :: AssociationFilterOperatorType
pattern AssociationFilterOperatorTypeLessThan = AssociationFilterOperatorType' "LESS_THAN"

pattern AssociationFilterOperatorTypeGreaterThan :: AssociationFilterOperatorType
pattern AssociationFilterOperatorTypeGreaterThan = AssociationFilterOperatorType' "GREATER_THAN"

{-# COMPLETE
  AssociationFilterOperatorTypeEqual,
  AssociationFilterOperatorTypeLessThan,
  AssociationFilterOperatorTypeGreaterThan,
  AssociationFilterOperatorType'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryRuleVersion
  ( CostCategoryRuleVersion
      ( CostCategoryRuleVersion',
        CostCategoryExpression_V1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The rule schema version in this particular Cost Category.
newtype CostCategoryRuleVersion = CostCategoryRuleVersion' Lude.Text
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

pattern CostCategoryExpression_V1 :: CostCategoryRuleVersion
pattern CostCategoryExpression_V1 = CostCategoryRuleVersion' "CostCategoryExpression.v1"

{-# COMPLETE
  CostCategoryExpression_V1,
  CostCategoryRuleVersion'
  #-}

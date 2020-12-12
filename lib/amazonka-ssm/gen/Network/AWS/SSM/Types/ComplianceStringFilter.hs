{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceStringFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceStringFilter
  ( ComplianceStringFilter (..),

    -- * Smart constructor
    mkComplianceStringFilter,

    -- * Lenses
    csfValues,
    csfKey,
    csfType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.ComplianceQueryOperatorType

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /See:/ 'mkComplianceStringFilter' smart constructor.
data ComplianceStringFilter = ComplianceStringFilter'
  { values ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    key :: Lude.Maybe Lude.Text,
    type' ::
      Lude.Maybe ComplianceQueryOperatorType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceStringFilter' with the minimum fields required to make a request.
--
-- * 'key' - The name of the filter.
-- * 'type'' - The type of comparison that should be performed for the value: Equal, NotEqual, BeginWith, LessThan, or GreaterThan.
-- * 'values' - The value for which to search.
mkComplianceStringFilter ::
  ComplianceStringFilter
mkComplianceStringFilter =
  ComplianceStringFilter'
    { values = Lude.Nothing,
      key = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The value for which to search.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfValues :: Lens.Lens' ComplianceStringFilter (Lude.Maybe (Lude.NonEmpty Lude.Text))
csfValues = Lens.lens (values :: ComplianceStringFilter -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {values = a} :: ComplianceStringFilter)
{-# DEPRECATED csfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfKey :: Lens.Lens' ComplianceStringFilter (Lude.Maybe Lude.Text)
csfKey = Lens.lens (key :: ComplianceStringFilter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: ComplianceStringFilter)
{-# DEPRECATED csfKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The type of comparison that should be performed for the value: Equal, NotEqual, BeginWith, LessThan, or GreaterThan.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfType :: Lens.Lens' ComplianceStringFilter (Lude.Maybe ComplianceQueryOperatorType)
csfType = Lens.lens (type' :: ComplianceStringFilter -> Lude.Maybe ComplianceQueryOperatorType) (\s a -> s {type' = a} :: ComplianceStringFilter)
{-# DEPRECATED csfType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON ComplianceStringFilter where
  toJSON ComplianceStringFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Values" Lude..=) Lude.<$> values,
            ("Key" Lude..=) Lude.<$> key,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )

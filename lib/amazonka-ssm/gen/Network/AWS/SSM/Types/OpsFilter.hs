{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsFilter
  ( OpsFilter (..),

    -- * Smart constructor
    mkOpsFilter,

    -- * Lenses
    ofValues,
    ofKey,
    ofType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OpsFilterOperatorType

-- | A filter for viewing OpsItem summaries.
--
-- /See:/ 'mkOpsFilter' smart constructor.
data OpsFilter = OpsFilter'
  { -- | The filter value.
    values :: Lude.NonEmpty Lude.Text,
    -- | The name of the filter.
    key :: Lude.Text,
    -- | The type of filter.
    type' :: Lude.Maybe OpsFilterOperatorType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsFilter' with the minimum fields required to make a request.
--
-- * 'values' - The filter value.
-- * 'key' - The name of the filter.
-- * 'type'' - The type of filter.
mkOpsFilter ::
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  -- | 'key'
  Lude.Text ->
  OpsFilter
mkOpsFilter pValues_ pKey_ =
  OpsFilter' {values = pValues_, key = pKey_, type' = Lude.Nothing}

-- | The filter value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofValues :: Lens.Lens' OpsFilter (Lude.NonEmpty Lude.Text)
ofValues = Lens.lens (values :: OpsFilter -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: OpsFilter)
{-# DEPRECATED ofValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofKey :: Lens.Lens' OpsFilter Lude.Text
ofKey = Lens.lens (key :: OpsFilter -> Lude.Text) (\s a -> s {key = a} :: OpsFilter)
{-# DEPRECATED ofKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The type of filter.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofType :: Lens.Lens' OpsFilter (Lude.Maybe OpsFilterOperatorType)
ofType = Lens.lens (type' :: OpsFilter -> Lude.Maybe OpsFilterOperatorType) (\s a -> s {type' = a} :: OpsFilter)
{-# DEPRECATED ofType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON OpsFilter where
  toJSON OpsFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            Lude.Just ("Key" Lude..= key),
            ("Type" Lude..=) Lude.<$> type'
          ]
      )

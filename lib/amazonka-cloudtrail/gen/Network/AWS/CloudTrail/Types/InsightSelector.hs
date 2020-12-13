{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.InsightSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.InsightSelector
  ( InsightSelector (..),

    -- * Smart constructor
    mkInsightSelector,

    -- * Lenses
    isInsightType,
  )
where

import Network.AWS.CloudTrail.Types.InsightType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A JSON string that contains a list of insight types that are logged on a trail.
--
-- /See:/ 'mkInsightSelector' smart constructor.
newtype InsightSelector = InsightSelector'
  { -- | The type of insights to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
    insightType :: Lude.Maybe InsightType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightSelector' with the minimum fields required to make a request.
--
-- * 'insightType' - The type of insights to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
mkInsightSelector ::
  InsightSelector
mkInsightSelector = InsightSelector' {insightType = Lude.Nothing}

-- | The type of insights to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- /Note:/ Consider using 'insightType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInsightType :: Lens.Lens' InsightSelector (Lude.Maybe InsightType)
isInsightType = Lens.lens (insightType :: InsightSelector -> Lude.Maybe InsightType) (\s a -> s {insightType = a} :: InsightSelector)
{-# DEPRECATED isInsightType "Use generic-lens or generic-optics with 'insightType' instead." #-}

instance Lude.FromJSON InsightSelector where
  parseJSON =
    Lude.withObject
      "InsightSelector"
      (\x -> InsightSelector' Lude.<$> (x Lude..:? "InsightType"))

instance Lude.ToJSON InsightSelector where
  toJSON InsightSelector' {..} =
    Lude.object
      (Lude.catMaybes [("InsightType" Lude..=) Lude.<$> insightType])

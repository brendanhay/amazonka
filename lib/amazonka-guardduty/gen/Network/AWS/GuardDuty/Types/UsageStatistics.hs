{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageStatistics
  ( UsageStatistics (..),

    -- * Smart constructor
    mkUsageStatistics,

    -- * Lenses
    usTopResources,
    usSumByResource,
    usSumByDataSource,
    usSumByAccount,
  )
where

import Network.AWS.GuardDuty.Types.UsageAccountResult
import Network.AWS.GuardDuty.Types.UsageDataSourceResult
import Network.AWS.GuardDuty.Types.UsageResourceResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the result of GuardDuty usage. If a UsageStatisticType is provided the result for other types will be null.
--
-- /See:/ 'mkUsageStatistics' smart constructor.
data UsageStatistics = UsageStatistics'
  { topResources ::
      Lude.Maybe [UsageResourceResult],
    sumByResource :: Lude.Maybe [UsageResourceResult],
    sumByDataSource :: Lude.Maybe [UsageDataSourceResult],
    sumByAccount :: Lude.Maybe [UsageAccountResult]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageStatistics' with the minimum fields required to make a request.
--
-- * 'sumByAccount' - The usage statistic sum organized by account ID.
-- * 'sumByDataSource' - The usage statistic sum organized by on data source.
-- * 'sumByResource' - The usage statistic sum organized by resource.
-- * 'topResources' - Lists the top 50 resources that have generated the most GuardDuty usage, in order from most to least expensive.
mkUsageStatistics ::
  UsageStatistics
mkUsageStatistics =
  UsageStatistics'
    { topResources = Lude.Nothing,
      sumByResource = Lude.Nothing,
      sumByDataSource = Lude.Nothing,
      sumByAccount = Lude.Nothing
    }

-- | Lists the top 50 resources that have generated the most GuardDuty usage, in order from most to least expensive.
--
-- /Note:/ Consider using 'topResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usTopResources :: Lens.Lens' UsageStatistics (Lude.Maybe [UsageResourceResult])
usTopResources = Lens.lens (topResources :: UsageStatistics -> Lude.Maybe [UsageResourceResult]) (\s a -> s {topResources = a} :: UsageStatistics)
{-# DEPRECATED usTopResources "Use generic-lens or generic-optics with 'topResources' instead." #-}

-- | The usage statistic sum organized by resource.
--
-- /Note:/ Consider using 'sumByResource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSumByResource :: Lens.Lens' UsageStatistics (Lude.Maybe [UsageResourceResult])
usSumByResource = Lens.lens (sumByResource :: UsageStatistics -> Lude.Maybe [UsageResourceResult]) (\s a -> s {sumByResource = a} :: UsageStatistics)
{-# DEPRECATED usSumByResource "Use generic-lens or generic-optics with 'sumByResource' instead." #-}

-- | The usage statistic sum organized by on data source.
--
-- /Note:/ Consider using 'sumByDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSumByDataSource :: Lens.Lens' UsageStatistics (Lude.Maybe [UsageDataSourceResult])
usSumByDataSource = Lens.lens (sumByDataSource :: UsageStatistics -> Lude.Maybe [UsageDataSourceResult]) (\s a -> s {sumByDataSource = a} :: UsageStatistics)
{-# DEPRECATED usSumByDataSource "Use generic-lens or generic-optics with 'sumByDataSource' instead." #-}

-- | The usage statistic sum organized by account ID.
--
-- /Note:/ Consider using 'sumByAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usSumByAccount :: Lens.Lens' UsageStatistics (Lude.Maybe [UsageAccountResult])
usSumByAccount = Lens.lens (sumByAccount :: UsageStatistics -> Lude.Maybe [UsageAccountResult]) (\s a -> s {sumByAccount = a} :: UsageStatistics)
{-# DEPRECATED usSumByAccount "Use generic-lens or generic-optics with 'sumByAccount' instead." #-}

instance Lude.FromJSON UsageStatistics where
  parseJSON =
    Lude.withObject
      "UsageStatistics"
      ( \x ->
          UsageStatistics'
            Lude.<$> (x Lude..:? "topResources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "sumByResource" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "sumByDataSource" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "sumByAccount" Lude..!= Lude.mempty)
      )

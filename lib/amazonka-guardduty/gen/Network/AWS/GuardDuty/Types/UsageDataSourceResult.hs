{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageDataSourceResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageDataSourceResult
  ( UsageDataSourceResult (..),

    -- * Smart constructor
    mkUsageDataSourceResult,

    -- * Lenses
    udsrTotal,
    udsrDataSource,
  )
where

import Network.AWS.GuardDuty.Types.DataSource
import Network.AWS.GuardDuty.Types.Total
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the result of usage based on data source type.
--
-- /See:/ 'mkUsageDataSourceResult' smart constructor.
data UsageDataSourceResult = UsageDataSourceResult'
  { total ::
      Lude.Maybe Total,
    dataSource :: Lude.Maybe DataSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageDataSourceResult' with the minimum fields required to make a request.
--
-- * 'dataSource' - The data source type that generated usage.
-- * 'total' - Represents the total of usage for the specified data source.
mkUsageDataSourceResult ::
  UsageDataSourceResult
mkUsageDataSourceResult =
  UsageDataSourceResult'
    { total = Lude.Nothing,
      dataSource = Lude.Nothing
    }

-- | Represents the total of usage for the specified data source.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrTotal :: Lens.Lens' UsageDataSourceResult (Lude.Maybe Total)
udsrTotal = Lens.lens (total :: UsageDataSourceResult -> Lude.Maybe Total) (\s a -> s {total = a} :: UsageDataSourceResult)
{-# DEPRECATED udsrTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The data source type that generated usage.
--
-- /Note:/ Consider using 'dataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udsrDataSource :: Lens.Lens' UsageDataSourceResult (Lude.Maybe DataSource)
udsrDataSource = Lens.lens (dataSource :: UsageDataSourceResult -> Lude.Maybe DataSource) (\s a -> s {dataSource = a} :: UsageDataSourceResult)
{-# DEPRECATED udsrDataSource "Use generic-lens or generic-optics with 'dataSource' instead." #-}

instance Lude.FromJSON UsageDataSourceResult where
  parseJSON =
    Lude.withObject
      "UsageDataSourceResult"
      ( \x ->
          UsageDataSourceResult'
            Lude.<$> (x Lude..:? "total") Lude.<*> (x Lude..:? "dataSource")
      )

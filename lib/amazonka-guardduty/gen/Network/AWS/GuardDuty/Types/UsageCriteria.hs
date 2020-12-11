-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageCriteria
  ( UsageCriteria (..),

    -- * Smart constructor
    mkUsageCriteria,

    -- * Lenses
    ucAccountIds,
    ucResources,
    ucDataSources,
  )
where

import Network.AWS.GuardDuty.Types.DataSource
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the criteria used to query usage statistics.
--
-- /See:/ 'mkUsageCriteria' smart constructor.
data UsageCriteria = UsageCriteria'
  { accountIds ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    resources :: Lude.Maybe [Lude.Text],
    dataSources :: [DataSource]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageCriteria' with the minimum fields required to make a request.
--
-- * 'accountIds' - The account IDs to aggregate usage statistics from.
-- * 'dataSources' - The data sources to aggregate usage statistics from.
-- * 'resources' - The resources to aggregate usage statistics from. Only accepts exact resource names.
mkUsageCriteria ::
  UsageCriteria
mkUsageCriteria =
  UsageCriteria'
    { accountIds = Lude.Nothing,
      resources = Lude.Nothing,
      dataSources = Lude.mempty
    }

-- | The account IDs to aggregate usage statistics from.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucAccountIds :: Lens.Lens' UsageCriteria (Lude.Maybe (Lude.NonEmpty Lude.Text))
ucAccountIds = Lens.lens (accountIds :: UsageCriteria -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {accountIds = a} :: UsageCriteria)
{-# DEPRECATED ucAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | The resources to aggregate usage statistics from. Only accepts exact resource names.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucResources :: Lens.Lens' UsageCriteria (Lude.Maybe [Lude.Text])
ucResources = Lens.lens (resources :: UsageCriteria -> Lude.Maybe [Lude.Text]) (\s a -> s {resources = a} :: UsageCriteria)
{-# DEPRECATED ucResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The data sources to aggregate usage statistics from.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDataSources :: Lens.Lens' UsageCriteria [DataSource]
ucDataSources = Lens.lens (dataSources :: UsageCriteria -> [DataSource]) (\s a -> s {dataSources = a} :: UsageCriteria)
{-# DEPRECATED ucDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

instance Lude.ToJSON UsageCriteria where
  toJSON UsageCriteria' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("accountIds" Lude..=) Lude.<$> accountIds,
            ("resources" Lude..=) Lude.<$> resources,
            Lude.Just ("dataSources" Lude..= dataSources)
          ]
      )

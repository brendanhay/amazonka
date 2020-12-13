{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.InstanceHealthSummary
  ( InstanceHealthSummary (..),

    -- * Smart constructor
    mkInstanceHealthSummary,

    -- * Lenses
    ihsOK,
    ihsPending,
    ihsSevere,
    ihsUnknown,
    ihsNoData,
    ihsWarning,
    ihsDegraded,
    ihsInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents summary information about the health of an instance. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- /See:/ 'mkInstanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
  { -- | __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
    ok :: Lude.Maybe Lude.Int,
    -- | __Grey.__ An operation is in progress on an instance within the command timeout.
    pending :: Lude.Maybe Lude.Int,
    -- | __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
    severe :: Lude.Maybe Lude.Int,
    -- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
    unknown :: Lude.Maybe Lude.Int,
    -- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
    noData :: Lude.Maybe Lude.Int,
    -- | __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
    warning :: Lude.Maybe Lude.Int,
    -- | __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
    degraded :: Lude.Maybe Lude.Int,
    -- | __Green.__ An operation is in progress on an instance.
    info :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceHealthSummary' with the minimum fields required to make a request.
--
-- * 'ok' - __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
-- * 'pending' - __Grey.__ An operation is in progress on an instance within the command timeout.
-- * 'severe' - __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
-- * 'unknown' - __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
-- * 'noData' - __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
-- * 'warning' - __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
-- * 'degraded' - __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
-- * 'info' - __Green.__ An operation is in progress on an instance.
mkInstanceHealthSummary ::
  InstanceHealthSummary
mkInstanceHealthSummary =
  InstanceHealthSummary'
    { ok = Lude.Nothing,
      pending = Lude.Nothing,
      severe = Lude.Nothing,
      unknown = Lude.Nothing,
      noData = Lude.Nothing,
      warning = Lude.Nothing,
      degraded = Lude.Nothing,
      info = Lude.Nothing
    }

-- | __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
--
-- /Note:/ Consider using 'ok' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsOK :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Int)
ihsOK = Lens.lens (ok :: InstanceHealthSummary -> Lude.Maybe Lude.Int) (\s a -> s {ok = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsOK "Use generic-lens or generic-optics with 'ok' instead." #-}

-- | __Grey.__ An operation is in progress on an instance within the command timeout.
--
-- /Note:/ Consider using 'pending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsPending :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Int)
ihsPending = Lens.lens (pending :: InstanceHealthSummary -> Lude.Maybe Lude.Int) (\s a -> s {pending = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsPending "Use generic-lens or generic-optics with 'pending' instead." #-}

-- | __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
--
-- /Note:/ Consider using 'severe' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsSevere :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Int)
ihsSevere = Lens.lens (severe :: InstanceHealthSummary -> Lude.Maybe Lude.Int) (\s a -> s {severe = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsSevere "Use generic-lens or generic-optics with 'severe' instead." #-}

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
--
-- /Note:/ Consider using 'unknown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsUnknown :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Int)
ihsUnknown = Lens.lens (unknown :: InstanceHealthSummary -> Lude.Maybe Lude.Int) (\s a -> s {unknown = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsUnknown "Use generic-lens or generic-optics with 'unknown' instead." #-}

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
--
-- /Note:/ Consider using 'noData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsNoData :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Int)
ihsNoData = Lens.lens (noData :: InstanceHealthSummary -> Lude.Maybe Lude.Int) (\s a -> s {noData = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsNoData "Use generic-lens or generic-optics with 'noData' instead." #-}

-- | __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsWarning :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Int)
ihsWarning = Lens.lens (warning :: InstanceHealthSummary -> Lude.Maybe Lude.Int) (\s a -> s {warning = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
--
-- /Note:/ Consider using 'degraded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsDegraded :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Int)
ihsDegraded = Lens.lens (degraded :: InstanceHealthSummary -> Lude.Maybe Lude.Int) (\s a -> s {degraded = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsDegraded "Use generic-lens or generic-optics with 'degraded' instead." #-}

-- | __Green.__ An operation is in progress on an instance.
--
-- /Note:/ Consider using 'info' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihsInfo :: Lens.Lens' InstanceHealthSummary (Lude.Maybe Lude.Int)
ihsInfo = Lens.lens (info :: InstanceHealthSummary -> Lude.Maybe Lude.Int) (\s a -> s {info = a} :: InstanceHealthSummary)
{-# DEPRECATED ihsInfo "Use generic-lens or generic-optics with 'info' instead." #-}

instance Lude.FromXML InstanceHealthSummary where
  parseXML x =
    InstanceHealthSummary'
      Lude.<$> (x Lude..@? "Ok")
      Lude.<*> (x Lude..@? "Pending")
      Lude.<*> (x Lude..@? "Severe")
      Lude.<*> (x Lude..@? "Unknown")
      Lude.<*> (x Lude..@? "NoData")
      Lude.<*> (x Lude..@? "Warning")
      Lude.<*> (x Lude..@? "Degraded")
      Lude.<*> (x Lude..@? "Info")

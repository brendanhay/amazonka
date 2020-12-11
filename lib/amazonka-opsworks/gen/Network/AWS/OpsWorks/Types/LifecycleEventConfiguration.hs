-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.LifecycleEventConfiguration
  ( LifecycleEventConfiguration (..),

    -- * Smart constructor
    mkLifecycleEventConfiguration,

    -- * Lenses
    lecShutdown,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.ShutdownEventConfiguration
import qualified Network.AWS.Prelude as Lude

-- | Specifies the lifecycle event configuration
--
-- /See:/ 'mkLifecycleEventConfiguration' smart constructor.
newtype LifecycleEventConfiguration = LifecycleEventConfiguration'
  { shutdown ::
      Lude.Maybe
        ShutdownEventConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LifecycleEventConfiguration' with the minimum fields required to make a request.
--
-- * 'shutdown' - A @ShutdownEventConfiguration@ object that specifies the Shutdown event configuration.
mkLifecycleEventConfiguration ::
  LifecycleEventConfiguration
mkLifecycleEventConfiguration =
  LifecycleEventConfiguration' {shutdown = Lude.Nothing}

-- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event configuration.
--
-- /Note:/ Consider using 'shutdown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecShutdown :: Lens.Lens' LifecycleEventConfiguration (Lude.Maybe ShutdownEventConfiguration)
lecShutdown = Lens.lens (shutdown :: LifecycleEventConfiguration -> Lude.Maybe ShutdownEventConfiguration) (\s a -> s {shutdown = a} :: LifecycleEventConfiguration)
{-# DEPRECATED lecShutdown "Use generic-lens or generic-optics with 'shutdown' instead." #-}

instance Lude.FromJSON LifecycleEventConfiguration where
  parseJSON =
    Lude.withObject
      "LifecycleEventConfiguration"
      ( \x ->
          LifecycleEventConfiguration' Lude.<$> (x Lude..:? "Shutdown")
      )

instance Lude.ToJSON LifecycleEventConfiguration where
  toJSON LifecycleEventConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [("Shutdown" Lude..=) Lude.<$> shutdown])

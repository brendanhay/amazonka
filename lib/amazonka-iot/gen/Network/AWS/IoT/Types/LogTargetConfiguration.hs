{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogTargetConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogTargetConfiguration
  ( LogTargetConfiguration (..),

    -- * Smart constructor
    mkLogTargetConfiguration,

    -- * Lenses
    ltcLogLevel,
    ltcLogTarget,
  )
where

import Network.AWS.IoT.Types.LogLevel
import Network.AWS.IoT.Types.LogTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The target configuration.
--
-- /See:/ 'mkLogTargetConfiguration' smart constructor.
data LogTargetConfiguration = LogTargetConfiguration'
  { logLevel ::
      Lude.Maybe LogLevel,
    logTarget :: Lude.Maybe LogTarget
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogTargetConfiguration' with the minimum fields required to make a request.
--
-- * 'logLevel' - The logging level.
-- * 'logTarget' - A log target
mkLogTargetConfiguration ::
  LogTargetConfiguration
mkLogTargetConfiguration =
  LogTargetConfiguration'
    { logLevel = Lude.Nothing,
      logTarget = Lude.Nothing
    }

-- | The logging level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcLogLevel :: Lens.Lens' LogTargetConfiguration (Lude.Maybe LogLevel)
ltcLogLevel = Lens.lens (logLevel :: LogTargetConfiguration -> Lude.Maybe LogLevel) (\s a -> s {logLevel = a} :: LogTargetConfiguration)
{-# DEPRECATED ltcLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | A log target
--
-- /Note:/ Consider using 'logTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcLogTarget :: Lens.Lens' LogTargetConfiguration (Lude.Maybe LogTarget)
ltcLogTarget = Lens.lens (logTarget :: LogTargetConfiguration -> Lude.Maybe LogTarget) (\s a -> s {logTarget = a} :: LogTargetConfiguration)
{-# DEPRECATED ltcLogTarget "Use generic-lens or generic-optics with 'logTarget' instead." #-}

instance Lude.FromJSON LogTargetConfiguration where
  parseJSON =
    Lude.withObject
      "LogTargetConfiguration"
      ( \x ->
          LogTargetConfiguration'
            Lude.<$> (x Lude..:? "logLevel") Lude.<*> (x Lude..:? "logTarget")
      )

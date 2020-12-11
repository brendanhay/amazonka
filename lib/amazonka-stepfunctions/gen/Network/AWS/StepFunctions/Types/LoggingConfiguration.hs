-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LoggingConfiguration
  ( LoggingConfiguration (..),

    -- * Smart constructor
    mkLoggingConfiguration,

    -- * Lenses
    lcIncludeExecutionData,
    lcDestinations,
    lcLevel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StepFunctions.Types.LogDestination
import Network.AWS.StepFunctions.Types.LogLevel

-- | The @LoggingConfiguration@ data type is used to set CloudWatch Logs options.
--
-- /See:/ 'mkLoggingConfiguration' smart constructor.
data LoggingConfiguration = LoggingConfiguration'
  { includeExecutionData ::
      Lude.Maybe Lude.Bool,
    destinations :: Lude.Maybe [LogDestination],
    level :: Lude.Maybe LogLevel
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoggingConfiguration' with the minimum fields required to make a request.
--
-- * 'destinations' - An array of objects that describes where your execution history events will be logged. Limited to size 1. Required, if your log level is not set to @OFF@ .
-- * 'includeExecutionData' - Determines whether execution data is included in your log. When set to @false@ , data is excluded.
-- * 'level' - Defines which category of execution history events are logged.
mkLoggingConfiguration ::
  LoggingConfiguration
mkLoggingConfiguration =
  LoggingConfiguration'
    { includeExecutionData = Lude.Nothing,
      destinations = Lude.Nothing,
      level = Lude.Nothing
    }

-- | Determines whether execution data is included in your log. When set to @false@ , data is excluded.
--
-- /Note:/ Consider using 'includeExecutionData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcIncludeExecutionData :: Lens.Lens' LoggingConfiguration (Lude.Maybe Lude.Bool)
lcIncludeExecutionData = Lens.lens (includeExecutionData :: LoggingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {includeExecutionData = a} :: LoggingConfiguration)
{-# DEPRECATED lcIncludeExecutionData "Use generic-lens or generic-optics with 'includeExecutionData' instead." #-}

-- | An array of objects that describes where your execution history events will be logged. Limited to size 1. Required, if your log level is not set to @OFF@ .
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcDestinations :: Lens.Lens' LoggingConfiguration (Lude.Maybe [LogDestination])
lcDestinations = Lens.lens (destinations :: LoggingConfiguration -> Lude.Maybe [LogDestination]) (\s a -> s {destinations = a} :: LoggingConfiguration)
{-# DEPRECATED lcDestinations "Use generic-lens or generic-optics with 'destinations' instead." #-}

-- | Defines which category of execution history events are logged.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcLevel :: Lens.Lens' LoggingConfiguration (Lude.Maybe LogLevel)
lcLevel = Lens.lens (level :: LoggingConfiguration -> Lude.Maybe LogLevel) (\s a -> s {level = a} :: LoggingConfiguration)
{-# DEPRECATED lcLevel "Use generic-lens or generic-optics with 'level' instead." #-}

instance Lude.FromJSON LoggingConfiguration where
  parseJSON =
    Lude.withObject
      "LoggingConfiguration"
      ( \x ->
          LoggingConfiguration'
            Lude.<$> (x Lude..:? "includeExecutionData")
            Lude.<*> (x Lude..:? "destinations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "level")
      )

instance Lude.ToJSON LoggingConfiguration where
  toJSON LoggingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("includeExecutionData" Lude..=) Lude.<$> includeExecutionData,
            ("destinations" Lude..=) Lude.<$> destinations,
            ("level" Lude..=) Lude.<$> level
          ]
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TracingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TracingConfiguration
  ( TracingConfiguration (..),

    -- * Smart constructor
    mkTracingConfiguration,

    -- * Lenses
    tcEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Selects whether or not the state machine's AWS X-Ray tracing is enabled. Default is @false@
--
-- /See:/ 'mkTracingConfiguration' smart constructor.
newtype TracingConfiguration = TracingConfiguration'
  { -- | When set to @true@ , AWS X-Ray tracing is enabled.
    enabled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TracingConfiguration' with the minimum fields required to make a request.
--
-- * 'enabled' - When set to @true@ , AWS X-Ray tracing is enabled.
mkTracingConfiguration ::
  TracingConfiguration
mkTracingConfiguration =
  TracingConfiguration' {enabled = Lude.Nothing}

-- | When set to @true@ , AWS X-Ray tracing is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcEnabled :: Lens.Lens' TracingConfiguration (Lude.Maybe Lude.Bool)
tcEnabled = Lens.lens (enabled :: TracingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: TracingConfiguration)
{-# DEPRECATED tcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromJSON TracingConfiguration where
  parseJSON =
    Lude.withObject
      "TracingConfiguration"
      (\x -> TracingConfiguration' Lude.<$> (x Lude..:? "enabled"))

instance Lude.ToJSON TracingConfiguration where
  toJSON TracingConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [("enabled" Lude..=) Lude.<$> enabled])

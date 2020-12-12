{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionConfiguration
  ( ActionConfiguration (..),

    -- * Smart constructor
    mkActionConfiguration,

    -- * Lenses
    acConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about an action configuration.
--
-- /See:/ 'mkActionConfiguration' smart constructor.
newtype ActionConfiguration = ActionConfiguration'
  { configuration ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionConfiguration' with the minimum fields required to make a request.
--
-- * 'configuration' - The configuration data for the action.
mkActionConfiguration ::
  ActionConfiguration
mkActionConfiguration =
  ActionConfiguration' {configuration = Lude.Nothing}

-- | The configuration data for the action.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acConfiguration :: Lens.Lens' ActionConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
acConfiguration = Lens.lens (configuration :: ActionConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {configuration = a} :: ActionConfiguration)
{-# DEPRECATED acConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

instance Lude.FromJSON ActionConfiguration where
  parseJSON =
    Lude.withObject
      "ActionConfiguration"
      ( \x ->
          ActionConfiguration'
            Lude.<$> (x Lude..:? "configuration" Lude..!= Lude.mempty)
      )

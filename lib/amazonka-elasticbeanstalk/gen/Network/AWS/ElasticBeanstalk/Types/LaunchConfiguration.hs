{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
  ( LaunchConfiguration (..),

    -- * Smart constructor
    mkLaunchConfiguration,

    -- * Lenses
    lcName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'mkLaunchConfiguration' smart constructor.
newtype LaunchConfiguration = LaunchConfiguration'
  { -- | The name of the launch configuration.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchConfiguration' with the minimum fields required to make a request.
--
-- * 'name' - The name of the launch configuration.
mkLaunchConfiguration ::
  LaunchConfiguration
mkLaunchConfiguration = LaunchConfiguration' {name = Lude.Nothing}

-- | The name of the launch configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcName :: Lens.Lens' LaunchConfiguration (Lude.Maybe Lude.Text)
lcName = Lens.lens (name :: LaunchConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LaunchConfiguration)
{-# DEPRECATED lcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML LaunchConfiguration where
  parseXML x = LaunchConfiguration' Lude.<$> (x Lude..@? "Name")

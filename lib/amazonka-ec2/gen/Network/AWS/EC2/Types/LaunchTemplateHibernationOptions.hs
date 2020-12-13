{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateHibernationOptions
  ( LaunchTemplateHibernationOptions (..),

    -- * Smart constructor
    mkLaunchTemplateHibernationOptions,

    -- * Lenses
    lthoConfigured,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether an instance is configured for hibernation.
--
-- /See:/ 'mkLaunchTemplateHibernationOptions' smart constructor.
newtype LaunchTemplateHibernationOptions = LaunchTemplateHibernationOptions'
  { -- | If this parameter is set to @true@ , the instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
    configured :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateHibernationOptions' with the minimum fields required to make a request.
--
-- * 'configured' - If this parameter is set to @true@ , the instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
mkLaunchTemplateHibernationOptions ::
  LaunchTemplateHibernationOptions
mkLaunchTemplateHibernationOptions =
  LaunchTemplateHibernationOptions' {configured = Lude.Nothing}

-- | If this parameter is set to @true@ , the instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
--
-- /Note:/ Consider using 'configured' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lthoConfigured :: Lens.Lens' LaunchTemplateHibernationOptions (Lude.Maybe Lude.Bool)
lthoConfigured = Lens.lens (configured :: LaunchTemplateHibernationOptions -> Lude.Maybe Lude.Bool) (\s a -> s {configured = a} :: LaunchTemplateHibernationOptions)
{-# DEPRECATED lthoConfigured "Use generic-lens or generic-optics with 'configured' instead." #-}

instance Lude.FromXML LaunchTemplateHibernationOptions where
  parseXML x =
    LaunchTemplateHibernationOptions'
      Lude.<$> (x Lude..@? "configured")

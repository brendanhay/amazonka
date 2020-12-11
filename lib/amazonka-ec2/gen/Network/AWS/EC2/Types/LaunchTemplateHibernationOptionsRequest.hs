-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateHibernationOptionsRequest
  ( LaunchTemplateHibernationOptionsRequest (..),

    -- * Smart constructor
    mkLaunchTemplateHibernationOptionsRequest,

    -- * Lenses
    lthorConfigured,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the instance is configured for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> .
--
-- /See:/ 'mkLaunchTemplateHibernationOptionsRequest' smart constructor.
newtype LaunchTemplateHibernationOptionsRequest = LaunchTemplateHibernationOptionsRequest'
  { configured ::
      Lude.Maybe
        Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateHibernationOptionsRequest' with the minimum fields required to make a request.
--
-- * 'configured' - If you set this parameter to @true@ , the instance is enabled for hibernation.
--
-- Default: @false@
mkLaunchTemplateHibernationOptionsRequest ::
  LaunchTemplateHibernationOptionsRequest
mkLaunchTemplateHibernationOptionsRequest =
  LaunchTemplateHibernationOptionsRequest'
    { configured =
        Lude.Nothing
    }

-- | If you set this parameter to @true@ , the instance is enabled for hibernation.
--
-- Default: @false@
--
-- /Note:/ Consider using 'configured' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lthorConfigured :: Lens.Lens' LaunchTemplateHibernationOptionsRequest (Lude.Maybe Lude.Bool)
lthorConfigured = Lens.lens (configured :: LaunchTemplateHibernationOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {configured = a} :: LaunchTemplateHibernationOptionsRequest)
{-# DEPRECATED lthorConfigured "Use generic-lens or generic-optics with 'configured' instead." #-}

instance Lude.ToQuery LaunchTemplateHibernationOptionsRequest where
  toQuery LaunchTemplateHibernationOptionsRequest' {..} =
    Lude.mconcat ["Configured" Lude.=: configured]

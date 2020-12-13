{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HibernationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HibernationOptions
  ( HibernationOptions (..),

    -- * Smart constructor
    mkHibernationOptions,

    -- * Lenses
    hoConfigured,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether your instance is configured for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkHibernationOptions' smart constructor.
newtype HibernationOptions = HibernationOptions'
  { -- | If this parameter is set to @true@ , your instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
    configured :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HibernationOptions' with the minimum fields required to make a request.
--
-- * 'configured' - If this parameter is set to @true@ , your instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
mkHibernationOptions ::
  HibernationOptions
mkHibernationOptions =
  HibernationOptions' {configured = Lude.Nothing}

-- | If this parameter is set to @true@ , your instance is enabled for hibernation; otherwise, it is not enabled for hibernation.
--
-- /Note:/ Consider using 'configured' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hoConfigured :: Lens.Lens' HibernationOptions (Lude.Maybe Lude.Bool)
hoConfigured = Lens.lens (configured :: HibernationOptions -> Lude.Maybe Lude.Bool) (\s a -> s {configured = a} :: HibernationOptions)
{-# DEPRECATED hoConfigured "Use generic-lens or generic-optics with 'configured' instead." #-}

instance Lude.FromXML HibernationOptions where
  parseXML x = HibernationOptions' Lude.<$> (x Lude..@? "configured")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HibernationOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HibernationOptionsRequest
  ( HibernationOptionsRequest (..),

    -- * Smart constructor
    mkHibernationOptionsRequest,

    -- * Lenses
    horConfigured,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether your instance is configured for hibernation. This parameter is valid only if the instance meets the <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites> . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkHibernationOptionsRequest' smart constructor.
newtype HibernationOptionsRequest = HibernationOptionsRequest'
  { configured ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HibernationOptionsRequest' with the minimum fields required to make a request.
--
-- * 'configured' - If you set this parameter to @true@ , your instance is enabled for hibernation.
--
-- Default: @false@
mkHibernationOptionsRequest ::
  HibernationOptionsRequest
mkHibernationOptionsRequest =
  HibernationOptionsRequest' {configured = Lude.Nothing}

-- | If you set this parameter to @true@ , your instance is enabled for hibernation.
--
-- Default: @false@
--
-- /Note:/ Consider using 'configured' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
horConfigured :: Lens.Lens' HibernationOptionsRequest (Lude.Maybe Lude.Bool)
horConfigured = Lens.lens (configured :: HibernationOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {configured = a} :: HibernationOptionsRequest)
{-# DEPRECATED horConfigured "Use generic-lens or generic-optics with 'configured' instead." #-}

instance Lude.ToQuery HibernationOptionsRequest where
  toQuery HibernationOptionsRequest' {..} =
    Lude.mconcat ["Configured" Lude.=: configured]

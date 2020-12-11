-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ResourceQuota
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ResourceQuota
  ( ResourceQuota (..),

    -- * Smart constructor
    mkResourceQuota,

    -- * Lenses
    rqMaximum,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The AWS Elastic Beanstalk quota information for a single resource type in an AWS account. It reflects the resource's limits for this account.
--
-- /See:/ 'mkResourceQuota' smart constructor.
newtype ResourceQuota = ResourceQuota'
  { maximum ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceQuota' with the minimum fields required to make a request.
--
-- * 'maximum' - The maximum number of instances of this Elastic Beanstalk resource type that an AWS account can use.
mkResourceQuota ::
  ResourceQuota
mkResourceQuota = ResourceQuota' {maximum = Lude.Nothing}

-- | The maximum number of instances of this Elastic Beanstalk resource type that an AWS account can use.
--
-- /Note:/ Consider using 'maximum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rqMaximum :: Lens.Lens' ResourceQuota (Lude.Maybe Lude.Int)
rqMaximum = Lens.lens (maximum :: ResourceQuota -> Lude.Maybe Lude.Int) (\s a -> s {maximum = a} :: ResourceQuota)
{-# DEPRECATED rqMaximum "Use generic-lens or generic-optics with 'maximum' instead." #-}

instance Lude.FromXML ResourceQuota where
  parseXML x = ResourceQuota' Lude.<$> (x Lude..@? "Maximum")

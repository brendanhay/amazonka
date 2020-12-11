-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.TagDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.TagDescription
  ( TagDescription (..),

    -- * Smart constructor
    mkTagDescription,

    -- * Lenses
    tdLoadBalancerName,
    tdTags,
  )
where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The tags associated with a load balancer.
--
-- /See:/ 'mkTagDescription' smart constructor.
data TagDescription = TagDescription'
  { loadBalancerName ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagDescription' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'tags' - The tags.
mkTagDescription ::
  TagDescription
mkTagDescription =
  TagDescription'
    { loadBalancerName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdLoadBalancerName :: Lens.Lens' TagDescription (Lude.Maybe Lude.Text)
tdLoadBalancerName = Lens.lens (loadBalancerName :: TagDescription -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: TagDescription)
{-# DEPRECATED tdLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTags :: Lens.Lens' TagDescription (Lude.Maybe (Lude.NonEmpty Tag))
tdTags = Lens.lens (tags :: TagDescription -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: TagDescription)
{-# DEPRECATED tdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TagDescription where
  parseXML x =
    TagDescription'
      Lude.<$> (x Lude..@? "LoadBalancerName")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLNonEmpty "member")
               )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.LoadBalancer
  ( LoadBalancer (..),

    -- * Smart constructor
    mkLoadBalancer,

    -- * Lenses
    lbName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a LoadBalancer.
--
-- /See:/ 'mkLoadBalancer' smart constructor.
newtype LoadBalancer = LoadBalancer'
  { -- | The name of the LoadBalancer.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- * 'name' - The name of the LoadBalancer.
mkLoadBalancer ::
  LoadBalancer
mkLoadBalancer = LoadBalancer' {name = Lude.Nothing}

-- | The name of the LoadBalancer.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbName :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbName = Lens.lens (name :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LoadBalancer)
{-# DEPRECATED lbName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML LoadBalancer where
  parseXML x = LoadBalancer' Lude.<$> (x Lude..@? "Name")

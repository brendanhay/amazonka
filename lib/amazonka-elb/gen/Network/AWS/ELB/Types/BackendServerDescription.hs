-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.BackendServerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.BackendServerDescription
  ( BackendServerDescription (..),

    -- * Smart constructor
    mkBackendServerDescription,

    -- * Lenses
    bsdPolicyNames,
    bsdInstancePort,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the configuration of an EC2 instance.
--
-- /See:/ 'mkBackendServerDescription' smart constructor.
data BackendServerDescription = BackendServerDescription'
  { policyNames ::
      Lude.Maybe [Lude.Text],
    instancePort :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackendServerDescription' with the minimum fields required to make a request.
--
-- * 'instancePort' - The port on which the EC2 instance is listening.
-- * 'policyNames' - The names of the policies enabled for the EC2 instance.
mkBackendServerDescription ::
  BackendServerDescription
mkBackendServerDescription =
  BackendServerDescription'
    { policyNames = Lude.Nothing,
      instancePort = Lude.Nothing
    }

-- | The names of the policies enabled for the EC2 instance.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsdPolicyNames :: Lens.Lens' BackendServerDescription (Lude.Maybe [Lude.Text])
bsdPolicyNames = Lens.lens (policyNames :: BackendServerDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {policyNames = a} :: BackendServerDescription)
{-# DEPRECATED bsdPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | The port on which the EC2 instance is listening.
--
-- /Note:/ Consider using 'instancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsdInstancePort :: Lens.Lens' BackendServerDescription (Lude.Maybe Lude.Natural)
bsdInstancePort = Lens.lens (instancePort :: BackendServerDescription -> Lude.Maybe Lude.Natural) (\s a -> s {instancePort = a} :: BackendServerDescription)
{-# DEPRECATED bsdInstancePort "Use generic-lens or generic-optics with 'instancePort' instead." #-}

instance Lude.FromXML BackendServerDescription where
  parseXML x =
    BackendServerDescription'
      Lude.<$> ( x Lude..@? "PolicyNames" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "InstancePort")

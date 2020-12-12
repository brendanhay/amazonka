{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ListenerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ListenerDescription
  ( ListenerDescription (..),

    -- * Smart constructor
    mkListenerDescription,

    -- * Lenses
    ldPolicyNames,
    ldListener,
  )
where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.Listener
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The policies enabled for a listener.
--
-- /See:/ 'mkListenerDescription' smart constructor.
data ListenerDescription = ListenerDescription'
  { policyNames ::
      Lude.Maybe [Lude.Text],
    listener :: Lude.Maybe Listener
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListenerDescription' with the minimum fields required to make a request.
--
-- * 'listener' - The listener.
-- * 'policyNames' - The policies. If there are no policies enabled, the list is empty.
mkListenerDescription ::
  ListenerDescription
mkListenerDescription =
  ListenerDescription'
    { policyNames = Lude.Nothing,
      listener = Lude.Nothing
    }

-- | The policies. If there are no policies enabled, the list is empty.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldPolicyNames :: Lens.Lens' ListenerDescription (Lude.Maybe [Lude.Text])
ldPolicyNames = Lens.lens (policyNames :: ListenerDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {policyNames = a} :: ListenerDescription)
{-# DEPRECATED ldPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | The listener.
--
-- /Note:/ Consider using 'listener' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldListener :: Lens.Lens' ListenerDescription (Lude.Maybe Listener)
ldListener = Lens.lens (listener :: ListenerDescription -> Lude.Maybe Listener) (\s a -> s {listener = a} :: ListenerDescription)
{-# DEPRECATED ldListener "Use generic-lens or generic-optics with 'listener' instead." #-}

instance Lude.FromXML ListenerDescription where
  parseXML x =
    ListenerDescription'
      Lude.<$> ( x Lude..@? "PolicyNames" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Listener")

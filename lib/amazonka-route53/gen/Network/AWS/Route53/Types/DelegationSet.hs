{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.DelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.DelegationSet
  ( DelegationSet (..),

    -- * Smart constructor
    mkDelegationSet,

    -- * Lenses
    dsId,
    dsCallerReference,
    dsNameServers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | A complex type that lists the name servers in a delegation set, as well as the @CallerReference@ and the @ID@ for the delegation set.
--
-- /See:/ 'mkDelegationSet' smart constructor.
data DelegationSet = DelegationSet'
  { id :: Lude.Maybe ResourceId,
    callerReference :: Lude.Maybe Lude.Text,
    nameServers :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DelegationSet' with the minimum fields required to make a request.
--
-- * 'callerReference' - The value that you specified for @CallerReference@ when you created the reusable delegation set.
-- * 'id' - The ID that Amazon Route 53 assigns to a reusable delegation set.
-- * 'nameServers' - A complex type that contains a list of the authoritative name servers for a hosted zone or for a reusable delegation set.
mkDelegationSet ::
  -- | 'nameServers'
  Lude.NonEmpty Lude.Text ->
  DelegationSet
mkDelegationSet pNameServers_ =
  DelegationSet'
    { id = Lude.Nothing,
      callerReference = Lude.Nothing,
      nameServers = pNameServers_
    }

-- | The ID that Amazon Route 53 assigns to a reusable delegation set.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsId :: Lens.Lens' DelegationSet (Lude.Maybe ResourceId)
dsId = Lens.lens (id :: DelegationSet -> Lude.Maybe ResourceId) (\s a -> s {id = a} :: DelegationSet)
{-# DEPRECATED dsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The value that you specified for @CallerReference@ when you created the reusable delegation set.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCallerReference :: Lens.Lens' DelegationSet (Lude.Maybe Lude.Text)
dsCallerReference = Lens.lens (callerReference :: DelegationSet -> Lude.Maybe Lude.Text) (\s a -> s {callerReference = a} :: DelegationSet)
{-# DEPRECATED dsCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | A complex type that contains a list of the authoritative name servers for a hosted zone or for a reusable delegation set.
--
-- /Note:/ Consider using 'nameServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNameServers :: Lens.Lens' DelegationSet (Lude.NonEmpty Lude.Text)
dsNameServers = Lens.lens (nameServers :: DelegationSet -> Lude.NonEmpty Lude.Text) (\s a -> s {nameServers = a} :: DelegationSet)
{-# DEPRECATED dsNameServers "Use generic-lens or generic-optics with 'nameServers' instead." #-}

instance Lude.FromXML DelegationSet where
  parseXML x =
    DelegationSet'
      Lude.<$> (x Lude..@? "Id")
      Lude.<*> (x Lude..@? "CallerReference")
      Lude.<*> ( x Lude..@? "NameServers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.parseXMLNonEmpty "NameServer"
               )

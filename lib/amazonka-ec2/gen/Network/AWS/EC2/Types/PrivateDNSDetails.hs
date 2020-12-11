-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateDNSDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrivateDNSDetails
  ( PrivateDNSDetails (..),

    -- * Smart constructor
    mkPrivateDNSDetails,

    -- * Lenses
    pddPrivateDNSName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the Private DNS name for interface endpoints.
--
-- /See:/ 'mkPrivateDNSDetails' smart constructor.
newtype PrivateDNSDetails = PrivateDNSDetails'
  { privateDNSName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrivateDNSDetails' with the minimum fields required to make a request.
--
-- * 'privateDNSName' - The private DNS name assigned to the VPC endpoint service.
mkPrivateDNSDetails ::
  PrivateDNSDetails
mkPrivateDNSDetails =
  PrivateDNSDetails' {privateDNSName = Lude.Nothing}

-- | The private DNS name assigned to the VPC endpoint service.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pddPrivateDNSName :: Lens.Lens' PrivateDNSDetails (Lude.Maybe Lude.Text)
pddPrivateDNSName = Lens.lens (privateDNSName :: PrivateDNSDetails -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: PrivateDNSDetails)
{-# DEPRECATED pddPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

instance Lude.FromXML PrivateDNSDetails where
  parseXML x =
    PrivateDNSDetails' Lude.<$> (x Lude..@? "privateDnsName")

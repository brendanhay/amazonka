-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClassicLinkDNSSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClassicLinkDNSSupport
  ( ClassicLinkDNSSupport (..),

    -- * Smart constructor
    mkClassicLinkDNSSupport,

    -- * Lenses
    cldsVPCId,
    cldsClassicLinkDNSSupported,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the ClassicLink DNS support status of a VPC.
--
-- /See:/ 'mkClassicLinkDNSSupport' smart constructor.
data ClassicLinkDNSSupport = ClassicLinkDNSSupport'
  { vpcId ::
      Lude.Maybe Lude.Text,
    classicLinkDNSSupported :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClassicLinkDNSSupport' with the minimum fields required to make a request.
--
-- * 'classicLinkDNSSupported' - Indicates whether ClassicLink DNS support is enabled for the VPC.
-- * 'vpcId' - The ID of the VPC.
mkClassicLinkDNSSupport ::
  ClassicLinkDNSSupport
mkClassicLinkDNSSupport =
  ClassicLinkDNSSupport'
    { vpcId = Lude.Nothing,
      classicLinkDNSSupported = Lude.Nothing
    }

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldsVPCId :: Lens.Lens' ClassicLinkDNSSupport (Lude.Maybe Lude.Text)
cldsVPCId = Lens.lens (vpcId :: ClassicLinkDNSSupport -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: ClassicLinkDNSSupport)
{-# DEPRECATED cldsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Indicates whether ClassicLink DNS support is enabled for the VPC.
--
-- /Note:/ Consider using 'classicLinkDNSSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldsClassicLinkDNSSupported :: Lens.Lens' ClassicLinkDNSSupport (Lude.Maybe Lude.Bool)
cldsClassicLinkDNSSupported = Lens.lens (classicLinkDNSSupported :: ClassicLinkDNSSupport -> Lude.Maybe Lude.Bool) (\s a -> s {classicLinkDNSSupported = a} :: ClassicLinkDNSSupport)
{-# DEPRECATED cldsClassicLinkDNSSupported "Use generic-lens or generic-optics with 'classicLinkDNSSupported' instead." #-}

instance Lude.FromXML ClassicLinkDNSSupport where
  parseXML x =
    ClassicLinkDNSSupport'
      Lude.<$> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "classicLinkDnsSupported")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StaleIPPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StaleIPPermission
  ( StaleIPPermission (..),

    -- * Smart constructor
    mkStaleIPPermission,

    -- * Lenses
    sipFromPort,
    sipUserIdGroupPairs,
    sipPrefixListIds,
    sipIPProtocol,
    sipToPort,
    sipIPRanges,
  )
where

import Network.AWS.EC2.Types.UserIdGroupPair
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a stale rule in a security group.
--
-- /See:/ 'mkStaleIPPermission' smart constructor.
data StaleIPPermission = StaleIPPermission'
  { fromPort ::
      Lude.Maybe Lude.Int,
    userIdGroupPairs :: Lude.Maybe [UserIdGroupPair],
    prefixListIds :: Lude.Maybe [Lude.Text],
    ipProtocol :: Lude.Maybe Lude.Text,
    toPort :: Lude.Maybe Lude.Int,
    ipRanges :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaleIPPermission' with the minimum fields required to make a request.
--
-- * 'fromPort' - The start of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types.
-- * 'ipProtocol' - The IP protocol name (for @tcp@ , @udp@ , and @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)> .
-- * 'ipRanges' - The IP ranges. Not applicable for stale security group rules.
-- * 'prefixListIds' - The prefix list IDs. Not applicable for stale security group rules.
-- * 'toPort' - The end of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types.
-- * 'userIdGroupPairs' - The security group pairs. Returns the ID of the referenced security group and VPC, and the ID and status of the VPC peering connection.
mkStaleIPPermission ::
  StaleIPPermission
mkStaleIPPermission =
  StaleIPPermission'
    { fromPort = Lude.Nothing,
      userIdGroupPairs = Lude.Nothing,
      prefixListIds = Lude.Nothing,
      ipProtocol = Lude.Nothing,
      toPort = Lude.Nothing,
      ipRanges = Lude.Nothing
    }

-- | The start of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types.
--
-- /Note:/ Consider using 'fromPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipFromPort :: Lens.Lens' StaleIPPermission (Lude.Maybe Lude.Int)
sipFromPort = Lens.lens (fromPort :: StaleIPPermission -> Lude.Maybe Lude.Int) (\s a -> s {fromPort = a} :: StaleIPPermission)
{-# DEPRECATED sipFromPort "Use generic-lens or generic-optics with 'fromPort' instead." #-}

-- | The security group pairs. Returns the ID of the referenced security group and VPC, and the ID and status of the VPC peering connection.
--
-- /Note:/ Consider using 'userIdGroupPairs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipUserIdGroupPairs :: Lens.Lens' StaleIPPermission (Lude.Maybe [UserIdGroupPair])
sipUserIdGroupPairs = Lens.lens (userIdGroupPairs :: StaleIPPermission -> Lude.Maybe [UserIdGroupPair]) (\s a -> s {userIdGroupPairs = a} :: StaleIPPermission)
{-# DEPRECATED sipUserIdGroupPairs "Use generic-lens or generic-optics with 'userIdGroupPairs' instead." #-}

-- | The prefix list IDs. Not applicable for stale security group rules.
--
-- /Note:/ Consider using 'prefixListIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipPrefixListIds :: Lens.Lens' StaleIPPermission (Lude.Maybe [Lude.Text])
sipPrefixListIds = Lens.lens (prefixListIds :: StaleIPPermission -> Lude.Maybe [Lude.Text]) (\s a -> s {prefixListIds = a} :: StaleIPPermission)
{-# DEPRECATED sipPrefixListIds "Use generic-lens or generic-optics with 'prefixListIds' instead." #-}

-- | The IP protocol name (for @tcp@ , @udp@ , and @icmp@ ) or number (see <http://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml Protocol Numbers)> .
--
-- /Note:/ Consider using 'ipProtocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipIPProtocol :: Lens.Lens' StaleIPPermission (Lude.Maybe Lude.Text)
sipIPProtocol = Lens.lens (ipProtocol :: StaleIPPermission -> Lude.Maybe Lude.Text) (\s a -> s {ipProtocol = a} :: StaleIPPermission)
{-# DEPRECATED sipIPProtocol "Use generic-lens or generic-optics with 'ipProtocol' instead." #-}

-- | The end of the port range for the TCP and UDP protocols, or an ICMP type number. A value of @-1@ indicates all ICMP types.
--
-- /Note:/ Consider using 'toPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipToPort :: Lens.Lens' StaleIPPermission (Lude.Maybe Lude.Int)
sipToPort = Lens.lens (toPort :: StaleIPPermission -> Lude.Maybe Lude.Int) (\s a -> s {toPort = a} :: StaleIPPermission)
{-# DEPRECATED sipToPort "Use generic-lens or generic-optics with 'toPort' instead." #-}

-- | The IP ranges. Not applicable for stale security group rules.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipIPRanges :: Lens.Lens' StaleIPPermission (Lude.Maybe [Lude.Text])
sipIPRanges = Lens.lens (ipRanges :: StaleIPPermission -> Lude.Maybe [Lude.Text]) (\s a -> s {ipRanges = a} :: StaleIPPermission)
{-# DEPRECATED sipIPRanges "Use generic-lens or generic-optics with 'ipRanges' instead." #-}

instance Lude.FromXML StaleIPPermission where
  parseXML x =
    StaleIPPermission'
      Lude.<$> (x Lude..@? "fromPort")
      Lude.<*> ( x Lude..@? "groups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "prefixListIds" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "ipProtocol")
      Lude.<*> (x Lude..@? "toPort")
      Lude.<*> ( x Lude..@? "ipRanges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )

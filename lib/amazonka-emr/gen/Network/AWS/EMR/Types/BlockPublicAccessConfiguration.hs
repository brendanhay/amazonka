{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.BlockPublicAccessConfiguration
  ( BlockPublicAccessConfiguration (..),

    -- * Smart constructor
    mkBlockPublicAccessConfiguration,

    -- * Lenses
    bpacPermittedPublicSecurityGroupRuleRanges,
    bpacBlockPublicSecurityGroupRules,
  )
where

import Network.AWS.EMR.Types.PortRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A configuration for Amazon EMR block public access. When @BlockPublicSecurityGroupRules@ is set to @true@ , Amazon EMR prevents cluster creation if one of the cluster's security groups has a rule that allows inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ .
--
-- /See:/ 'mkBlockPublicAccessConfiguration' smart constructor.
data BlockPublicAccessConfiguration = BlockPublicAccessConfiguration'
  { permittedPublicSecurityGroupRuleRanges ::
      Lude.Maybe [PortRange],
    blockPublicSecurityGroupRules ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlockPublicAccessConfiguration' with the minimum fields required to make a request.
--
-- * 'blockPublicSecurityGroupRules' - Indicates whether Amazon EMR block public access is enabled (@true@ ) or disabled (@false@ ). By default, the value is @false@ for accounts that have created EMR clusters before July 2019. For accounts created after this, the default is @true@ .
-- * 'permittedPublicSecurityGroupRuleRanges' - Specifies ports and port ranges that are permitted to have security group rules that allow inbound traffic from all public sources. For example, if Port 23 (Telnet) is specified for @PermittedPublicSecurityGroupRuleRanges@ , Amazon EMR allows cluster creation if a security group associated with the cluster has a rule that allows inbound traffic on Port 23 from IPv4 0.0.0.0/0 or IPv6 port ::/0 as the source.
--
-- By default, Port 22, which is used for SSH access to the cluster EC2 instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@ .
mkBlockPublicAccessConfiguration ::
  -- | 'blockPublicSecurityGroupRules'
  Lude.Bool ->
  BlockPublicAccessConfiguration
mkBlockPublicAccessConfiguration pBlockPublicSecurityGroupRules_ =
  BlockPublicAccessConfiguration'
    { permittedPublicSecurityGroupRuleRanges =
        Lude.Nothing,
      blockPublicSecurityGroupRules = pBlockPublicSecurityGroupRules_
    }

-- | Specifies ports and port ranges that are permitted to have security group rules that allow inbound traffic from all public sources. For example, if Port 23 (Telnet) is specified for @PermittedPublicSecurityGroupRuleRanges@ , Amazon EMR allows cluster creation if a security group associated with the cluster has a rule that allows inbound traffic on Port 23 from IPv4 0.0.0.0/0 or IPv6 port ::/0 as the source.
--
-- By default, Port 22, which is used for SSH access to the cluster EC2 instances, is in the list of @PermittedPublicSecurityGroupRuleRanges@ .
--
-- /Note:/ Consider using 'permittedPublicSecurityGroupRuleRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpacPermittedPublicSecurityGroupRuleRanges :: Lens.Lens' BlockPublicAccessConfiguration (Lude.Maybe [PortRange])
bpacPermittedPublicSecurityGroupRuleRanges = Lens.lens (permittedPublicSecurityGroupRuleRanges :: BlockPublicAccessConfiguration -> Lude.Maybe [PortRange]) (\s a -> s {permittedPublicSecurityGroupRuleRanges = a} :: BlockPublicAccessConfiguration)
{-# DEPRECATED bpacPermittedPublicSecurityGroupRuleRanges "Use generic-lens or generic-optics with 'permittedPublicSecurityGroupRuleRanges' instead." #-}

-- | Indicates whether Amazon EMR block public access is enabled (@true@ ) or disabled (@false@ ). By default, the value is @false@ for accounts that have created EMR clusters before July 2019. For accounts created after this, the default is @true@ .
--
-- /Note:/ Consider using 'blockPublicSecurityGroupRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpacBlockPublicSecurityGroupRules :: Lens.Lens' BlockPublicAccessConfiguration Lude.Bool
bpacBlockPublicSecurityGroupRules = Lens.lens (blockPublicSecurityGroupRules :: BlockPublicAccessConfiguration -> Lude.Bool) (\s a -> s {blockPublicSecurityGroupRules = a} :: BlockPublicAccessConfiguration)
{-# DEPRECATED bpacBlockPublicSecurityGroupRules "Use generic-lens or generic-optics with 'blockPublicSecurityGroupRules' instead." #-}

instance Lude.FromJSON BlockPublicAccessConfiguration where
  parseJSON =
    Lude.withObject
      "BlockPublicAccessConfiguration"
      ( \x ->
          BlockPublicAccessConfiguration'
            Lude.<$> ( x Lude..:? "PermittedPublicSecurityGroupRuleRanges"
                         Lude..!= Lude.mempty
                     )
            Lude.<*> (x Lude..: "BlockPublicSecurityGroupRules")
      )

instance Lude.ToJSON BlockPublicAccessConfiguration where
  toJSON BlockPublicAccessConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PermittedPublicSecurityGroupRuleRanges" Lude..=)
              Lude.<$> permittedPublicSecurityGroupRuleRanges,
            Lude.Just
              ( "BlockPublicSecurityGroupRules"
                  Lude..= blockPublicSecurityGroupRules
              )
          ]
      )

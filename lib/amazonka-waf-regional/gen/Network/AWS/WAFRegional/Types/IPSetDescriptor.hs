{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.IPSetDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.IPSetDescriptor
  ( IPSetDescriptor (..),

    -- * Smart constructor
    mkIPSetDescriptor,

    -- * Lenses
    ipsdType,
    ipsdValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.IPSetDescriptorType as Types
import qualified Network.AWS.WAFRegional.Types.Value as Types

-- | Specifies the IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR format) that web requests originate from.
--
-- /See:/ 'mkIPSetDescriptor' smart constructor.
data IPSetDescriptor = IPSetDescriptor'
  { -- | Specify @IPV4@ or @IPV6@ .
    type' :: Types.IPSetDescriptorType,
    -- | Specify an IPv4 address by using CIDR notation. For example:
    --
    --
    --     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 192.0.2.44, specify @192.0.2.44/32@ .
    --
    --
    --     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify @192.0.2.0/24@ .
    --
    --
    -- For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
    -- Specify an IPv6 address by using CIDR notation. For example:
    --
    --     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify @1111:0000:0000:0000:0000:0000:0000:0111/128@ .
    --
    --
    --     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify @1111:0000:0000:0000:0000:0000:0000:0000/64@ .
    value :: Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IPSetDescriptor' value with any optional fields omitted.
mkIPSetDescriptor ::
  -- | 'type\''
  Types.IPSetDescriptorType ->
  -- | 'value'
  Types.Value ->
  IPSetDescriptor
mkIPSetDescriptor type' value = IPSetDescriptor' {type', value}

-- | Specify @IPV4@ or @IPV6@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsdType :: Lens.Lens' IPSetDescriptor Types.IPSetDescriptorType
ipsdType = Lens.field @"type'"
{-# DEPRECATED ipsdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Specify an IPv4 address by using CIDR notation. For example:
--
--
--     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 192.0.2.44, specify @192.0.2.44/32@ .
--
--
--     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses from 192.0.2.0 to 192.0.2.255, specify @192.0.2.0/24@ .
--
--
-- For more information about CIDR notation, see the Wikipedia entry <https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing Classless Inter-Domain Routing> .
-- Specify an IPv6 address by using CIDR notation. For example:
--
--     * To configure AWS WAF to allow, block, or count requests that originated from the IP address 1111:0000:0000:0000:0000:0000:0000:0111, specify @1111:0000:0000:0000:0000:0000:0000:0111/128@ .
--
--
--     * To configure AWS WAF to allow, block, or count requests that originated from IP addresses 1111:0000:0000:0000:0000:0000:0000:0000 to 1111:0000:0000:0000:ffff:ffff:ffff:ffff, specify @1111:0000:0000:0000:0000:0000:0000:0000/64@ .
--
--
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsdValue :: Lens.Lens' IPSetDescriptor Types.Value
ipsdValue = Lens.field @"value"
{-# DEPRECATED ipsdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON IPSetDescriptor where
  toJSON IPSetDescriptor {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            Core.Just ("Value" Core..= value)
          ]
      )

instance Core.FromJSON IPSetDescriptor where
  parseJSON =
    Core.withObject "IPSetDescriptor" Core.$
      \x ->
        IPSetDescriptor'
          Core.<$> (x Core..: "Type") Core.<*> (x Core..: "Value")

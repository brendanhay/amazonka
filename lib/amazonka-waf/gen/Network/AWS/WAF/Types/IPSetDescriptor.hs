-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.IPSetDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.IPSetDescriptor
  ( IPSetDescriptor (..),

    -- * Smart constructor
    mkIPSetDescriptor,

    -- * Lenses
    isdType,
    isdValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.IPSetDescriptorType

-- | Specifies the IP address type (@IPV4@ or @IPV6@ ) and the IP address range (in CIDR format) that web requests originate from.
--
-- /See:/ 'mkIPSetDescriptor' smart constructor.
data IPSetDescriptor = IPSetDescriptor'
  { type' ::
      IPSetDescriptorType,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPSetDescriptor' with the minimum fields required to make a request.
--
-- * 'type'' - Specify @IPV4@ or @IPV6@ .
-- * 'value' - Specify an IPv4 address by using CIDR notation. For example:
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
mkIPSetDescriptor ::
  -- | 'type''
  IPSetDescriptorType ->
  -- | 'value'
  Lude.Text ->
  IPSetDescriptor
mkIPSetDescriptor pType_ pValue_ =
  IPSetDescriptor' {type' = pType_, value = pValue_}

-- | Specify @IPV4@ or @IPV6@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isdType :: Lens.Lens' IPSetDescriptor IPSetDescriptorType
isdType = Lens.lens (type' :: IPSetDescriptor -> IPSetDescriptorType) (\s a -> s {type' = a} :: IPSetDescriptor)
{-# DEPRECATED isdType "Use generic-lens or generic-optics with 'type'' instead." #-}

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
isdValue :: Lens.Lens' IPSetDescriptor Lude.Text
isdValue = Lens.lens (value :: IPSetDescriptor -> Lude.Text) (\s a -> s {value = a} :: IPSetDescriptor)
{-# DEPRECATED isdValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON IPSetDescriptor where
  parseJSON =
    Lude.withObject
      "IPSetDescriptor"
      ( \x ->
          IPSetDescriptor'
            Lude.<$> (x Lude..: "Type") Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON IPSetDescriptor where
  toJSON IPSetDescriptor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Value" Lude..= value)
          ]
      )

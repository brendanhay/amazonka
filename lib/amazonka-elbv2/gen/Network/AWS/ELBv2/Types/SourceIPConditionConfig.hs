{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SourceIPConditionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SourceIPConditionConfig
  ( SourceIPConditionConfig (..),

    -- * Smart constructor
    mkSourceIPConditionConfig,

    -- * Lenses
    siccValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a source IP condition.
--
-- You can use this condition to route based on the IP address of the source that connects to the load balancer. If a client is behind a proxy, this is the IP address of the proxy not the IP address of the client.
--
-- /See:/ 'mkSourceIPConditionConfig' smart constructor.
newtype SourceIPConditionConfig = SourceIPConditionConfig'
  { values ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SourceIPConditionConfig' with the minimum fields required to make a request.
--
-- * 'values' - One or more source IP addresses, in CIDR format. You can use both IPv4 and IPv6 addresses. Wildcards are not supported.
--
-- If you specify multiple addresses, the condition is satisfied if the source IP address of the request matches one of the CIDR blocks. This condition is not satisfied by the addresses in the X-Forwarded-For header. To search for addresses in the X-Forwarded-For header, use 'HttpHeaderConditionConfig' .
mkSourceIPConditionConfig ::
  SourceIPConditionConfig
mkSourceIPConditionConfig =
  SourceIPConditionConfig' {values = Lude.Nothing}

-- | One or more source IP addresses, in CIDR format. You can use both IPv4 and IPv6 addresses. Wildcards are not supported.
--
-- If you specify multiple addresses, the condition is satisfied if the source IP address of the request matches one of the CIDR blocks. This condition is not satisfied by the addresses in the X-Forwarded-For header. To search for addresses in the X-Forwarded-For header, use 'HttpHeaderConditionConfig' .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siccValues :: Lens.Lens' SourceIPConditionConfig (Lude.Maybe [Lude.Text])
siccValues = Lens.lens (values :: SourceIPConditionConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: SourceIPConditionConfig)
{-# DEPRECATED siccValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromXML SourceIPConditionConfig where
  parseXML x =
    SourceIPConditionConfig'
      Lude.<$> ( x Lude..@? "Values" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

instance Lude.ToQuery SourceIPConditionConfig where
  toQuery SourceIPConditionConfig' {..} =
    Lude.mconcat
      [ "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values)
      ]

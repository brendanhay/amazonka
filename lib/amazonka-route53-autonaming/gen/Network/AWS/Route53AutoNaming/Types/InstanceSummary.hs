{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.InstanceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.InstanceSummary
  ( InstanceSummary (..),

    -- * Smart constructor
    mkInstanceSummary,

    -- * Lenses
    isAttributes,
    isId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains information about the instances that you registered by using a specified service.
--
-- /See:/ 'mkInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { attributes ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceSummary' with the minimum fields required to make a request.
--
-- * 'attributes' - A string map that contains the following information:
--
--
--     * The attributes that are associate with the instance.
--
--
--     * For each attribute, the applicable value.
--
--
-- Supported attribute keys include the following:
--
--     * @AWS_ALIAS_DNS_NAME@ : For an alias record that routes traffic to an Elastic Load Balancing load balancer, the DNS name that is associated with the load balancer.
--
--
--     * @AWS_EC2_INSTANCE_ID@ : (HTTP namespaces only) The Amazon EC2 instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4 address.
--
--
--     * @AWS_INSTANCE_CNAME@ : For a @CNAME@ record, the domain name that Route 53 returns in response to DNS queries, for example, @example.com@ .
--
--
--     * @AWS_INSTANCE_IPV4@ : For an @A@ record, the IPv4 address that Route 53 returns in response to DNS queries, for example, @192.0.2.44@ .
--
--
--     * @AWS_INSTANCE_IPV6@ : For an @AAAA@ record, the IPv6 address that Route 53 returns in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .
--
--
--     * @AWS_INSTANCE_PORT@ : For an @SRV@ record, the value that Route 53 returns for the port. In addition, if the service includes @HealthCheckConfig@ , the port on the endpoint that Route 53 sends requests to.
--
--
-- * 'id' - The ID for an instance that you created by using a specified service.
mkInstanceSummary ::
  InstanceSummary
mkInstanceSummary =
  InstanceSummary' {attributes = Lude.Nothing, id = Lude.Nothing}

-- | A string map that contains the following information:
--
--
--     * The attributes that are associate with the instance.
--
--
--     * For each attribute, the applicable value.
--
--
-- Supported attribute keys include the following:
--
--     * @AWS_ALIAS_DNS_NAME@ : For an alias record that routes traffic to an Elastic Load Balancing load balancer, the DNS name that is associated with the load balancer.
--
--
--     * @AWS_EC2_INSTANCE_ID@ : (HTTP namespaces only) The Amazon EC2 instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@ attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute contains the primary private IPv4 address.
--
--
--     * @AWS_INSTANCE_CNAME@ : For a @CNAME@ record, the domain name that Route 53 returns in response to DNS queries, for example, @example.com@ .
--
--
--     * @AWS_INSTANCE_IPV4@ : For an @A@ record, the IPv4 address that Route 53 returns in response to DNS queries, for example, @192.0.2.44@ .
--
--
--     * @AWS_INSTANCE_IPV6@ : For an @AAAA@ record, the IPv6 address that Route 53 returns in response to DNS queries, for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ .
--
--
--     * @AWS_INSTANCE_PORT@ : For an @SRV@ record, the value that Route 53 returns for the port. In addition, if the service includes @HealthCheckConfig@ , the port on the endpoint that Route 53 sends requests to.
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isAttributes :: Lens.Lens' InstanceSummary (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
isAttributes = Lens.lens (attributes :: InstanceSummary -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: InstanceSummary)
{-# DEPRECATED isAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The ID for an instance that you created by using a specified service.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isId :: Lens.Lens' InstanceSummary (Lude.Maybe Lude.Text)
isId = Lens.lens (id :: InstanceSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InstanceSummary)
{-# DEPRECATED isId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON InstanceSummary where
  parseJSON =
    Lude.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Lude.<$> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
      )

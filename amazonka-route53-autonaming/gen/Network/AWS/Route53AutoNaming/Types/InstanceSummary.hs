{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.InstanceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.InstanceSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that contains information about the instances that you
-- registered by using a specified service.
--
-- /See:/ 'newInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { -- | The ID for an instance that you created by using a specified service.
    id :: Prelude.Maybe Prelude.Text,
    -- | A string map that contains the following information:
    --
    -- -   The attributes that are associate with the instance.
    --
    -- -   For each attribute, the applicable value.
    --
    -- Supported attribute keys include the following:
    --
    -- -   @AWS_ALIAS_DNS_NAME@: For an alias record that routes traffic to an
    --     Elastic Load Balancing load balancer, the DNS name that is
    --     associated with the load balancer.
    --
    -- -   @AWS_EC2_INSTANCE_ID@: (HTTP namespaces only) The Amazon EC2
    --     instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@
    --     attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute
    --     contains the primary private IPv4 address.
    --
    -- -   @AWS_INSTANCE_CNAME@: For a @CNAME@ record, the domain name that
    --     Route 53 returns in response to DNS queries, for example,
    --     @example.com@.
    --
    -- -   @AWS_INSTANCE_IPV4@: For an @A@ record, the IPv4 address that
    --     Route 53 returns in response to DNS queries, for example,
    --     @192.0.2.44@.
    --
    -- -   @AWS_INSTANCE_IPV6@: For an @AAAA@ record, the IPv6 address that
    --     Route 53 returns in response to DNS queries, for example,
    --     @2001:0db8:85a3:0000:0000:abcd:0001:2345@.
    --
    -- -   @AWS_INSTANCE_PORT@: For an @SRV@ record, the value that Route 53
    --     returns for the port. In addition, if the service includes
    --     @HealthCheckConfig@, the port on the endpoint that Route 53 sends
    --     requests to.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'instanceSummary_id' - The ID for an instance that you created by using a specified service.
--
-- 'attributes', 'instanceSummary_attributes' - A string map that contains the following information:
--
-- -   The attributes that are associate with the instance.
--
-- -   For each attribute, the applicable value.
--
-- Supported attribute keys include the following:
--
-- -   @AWS_ALIAS_DNS_NAME@: For an alias record that routes traffic to an
--     Elastic Load Balancing load balancer, the DNS name that is
--     associated with the load balancer.
--
-- -   @AWS_EC2_INSTANCE_ID@: (HTTP namespaces only) The Amazon EC2
--     instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@
--     attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute
--     contains the primary private IPv4 address.
--
-- -   @AWS_INSTANCE_CNAME@: For a @CNAME@ record, the domain name that
--     Route 53 returns in response to DNS queries, for example,
--     @example.com@.
--
-- -   @AWS_INSTANCE_IPV4@: For an @A@ record, the IPv4 address that
--     Route 53 returns in response to DNS queries, for example,
--     @192.0.2.44@.
--
-- -   @AWS_INSTANCE_IPV6@: For an @AAAA@ record, the IPv6 address that
--     Route 53 returns in response to DNS queries, for example,
--     @2001:0db8:85a3:0000:0000:abcd:0001:2345@.
--
-- -   @AWS_INSTANCE_PORT@: For an @SRV@ record, the value that Route 53
--     returns for the port. In addition, if the service includes
--     @HealthCheckConfig@, the port on the endpoint that Route 53 sends
--     requests to.
newInstanceSummary ::
  InstanceSummary
newInstanceSummary =
  InstanceSummary'
    { id = Prelude.Nothing,
      attributes = Prelude.Nothing
    }

-- | The ID for an instance that you created by using a specified service.
instanceSummary_id :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_id = Lens.lens (\InstanceSummary' {id} -> id) (\s@InstanceSummary' {} a -> s {id = a} :: InstanceSummary)

-- | A string map that contains the following information:
--
-- -   The attributes that are associate with the instance.
--
-- -   For each attribute, the applicable value.
--
-- Supported attribute keys include the following:
--
-- -   @AWS_ALIAS_DNS_NAME@: For an alias record that routes traffic to an
--     Elastic Load Balancing load balancer, the DNS name that is
--     associated with the load balancer.
--
-- -   @AWS_EC2_INSTANCE_ID@: (HTTP namespaces only) The Amazon EC2
--     instance ID for the instance. When the @AWS_EC2_INSTANCE_ID@
--     attribute is specified, then the @AWS_INSTANCE_IPV4@ attribute
--     contains the primary private IPv4 address.
--
-- -   @AWS_INSTANCE_CNAME@: For a @CNAME@ record, the domain name that
--     Route 53 returns in response to DNS queries, for example,
--     @example.com@.
--
-- -   @AWS_INSTANCE_IPV4@: For an @A@ record, the IPv4 address that
--     Route 53 returns in response to DNS queries, for example,
--     @192.0.2.44@.
--
-- -   @AWS_INSTANCE_IPV6@: For an @AAAA@ record, the IPv6 address that
--     Route 53 returns in response to DNS queries, for example,
--     @2001:0db8:85a3:0000:0000:abcd:0001:2345@.
--
-- -   @AWS_INSTANCE_PORT@: For an @SRV@ record, the value that Route 53
--     returns for the port. In addition, if the service includes
--     @HealthCheckConfig@, the port on the endpoint that Route 53 sends
--     requests to.
instanceSummary_attributes :: Lens.Lens' InstanceSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
instanceSummary_attributes = Lens.lens (\InstanceSummary' {attributes} -> attributes) (\s@InstanceSummary' {} a -> s {attributes = a} :: InstanceSummary) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON InstanceSummary where
  parseJSON =
    Core.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Prelude.<$> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable InstanceSummary

instance Prelude.NFData InstanceSummary

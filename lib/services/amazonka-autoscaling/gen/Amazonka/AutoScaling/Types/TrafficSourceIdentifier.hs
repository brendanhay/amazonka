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
-- Module      : Amazonka.AutoScaling.Types.TrafficSourceIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.TrafficSourceIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifying information for a traffic source.
--
-- /See:/ 'newTrafficSourceIdentifier' smart constructor.
data TrafficSourceIdentifier = TrafficSourceIdentifier'
  { -- | Provides additional context for the value of @Identifier@.
    --
    -- The following lists the valid values:
    --
    -- -   @elb@ if @Identifier@ is the name of a Classic Load Balancer.
    --
    -- -   @elbv2@ if @Identifier@ is the ARN of an Application Load Balancer,
    --     Gateway Load Balancer, or Network Load Balancer target group.
    --
    -- -   @vpc-lattice@ if @Identifier@ is the ARN of a VPC Lattice target
    --     group.
    --
    -- Required if the identifier is the name of a Classic Load Balancer.
    type' :: Prelude.Maybe Prelude.Text,
    -- | Identifies the traffic source.
    --
    -- For Application Load Balancers, Gateway Load Balancers, Network Load
    -- Balancers, and VPC Lattice, this will be the Amazon Resource Name (ARN)
    -- for a target group in this account and Region. For Classic Load
    -- Balancers, this will be the name of the Classic Load Balancer in this
    -- account and Region.
    --
    -- For example:
    --
    -- -   Application Load Balancer ARN:
    --     @arn:aws:elasticloadbalancing:us-west-2:123456789012:targetgroup\/my-targets\/1234567890123456@
    --
    -- -   Classic Load Balancer name: @my-classic-load-balancer@
    --
    -- -   VPC Lattice ARN:
    --     @arn:aws:vpc-lattice:us-west-2:123456789012:targetgroup\/tg-1234567890123456@
    --
    -- To get the ARN of a target group for a Application Load Balancer,
    -- Gateway Load Balancer, or Network Load Balancer, or the name of a
    -- Classic Load Balancer, use the Elastic Load Balancing
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
    -- and
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
    -- API operations.
    --
    -- To get the ARN of a target group for VPC Lattice, use the VPC Lattice
    -- <https://docs.aws.amazon.com/vpc-lattice/latest/APIReference/API_GetTargetGroup.html GetTargetGroup>
    -- API operation.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficSourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'trafficSourceIdentifier_type' - Provides additional context for the value of @Identifier@.
--
-- The following lists the valid values:
--
-- -   @elb@ if @Identifier@ is the name of a Classic Load Balancer.
--
-- -   @elbv2@ if @Identifier@ is the ARN of an Application Load Balancer,
--     Gateway Load Balancer, or Network Load Balancer target group.
--
-- -   @vpc-lattice@ if @Identifier@ is the ARN of a VPC Lattice target
--     group.
--
-- Required if the identifier is the name of a Classic Load Balancer.
--
-- 'identifier', 'trafficSourceIdentifier_identifier' - Identifies the traffic source.
--
-- For Application Load Balancers, Gateway Load Balancers, Network Load
-- Balancers, and VPC Lattice, this will be the Amazon Resource Name (ARN)
-- for a target group in this account and Region. For Classic Load
-- Balancers, this will be the name of the Classic Load Balancer in this
-- account and Region.
--
-- For example:
--
-- -   Application Load Balancer ARN:
--     @arn:aws:elasticloadbalancing:us-west-2:123456789012:targetgroup\/my-targets\/1234567890123456@
--
-- -   Classic Load Balancer name: @my-classic-load-balancer@
--
-- -   VPC Lattice ARN:
--     @arn:aws:vpc-lattice:us-west-2:123456789012:targetgroup\/tg-1234567890123456@
--
-- To get the ARN of a target group for a Application Load Balancer,
-- Gateway Load Balancer, or Network Load Balancer, or the name of a
-- Classic Load Balancer, use the Elastic Load Balancing
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- and
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
-- API operations.
--
-- To get the ARN of a target group for VPC Lattice, use the VPC Lattice
-- <https://docs.aws.amazon.com/vpc-lattice/latest/APIReference/API_GetTargetGroup.html GetTargetGroup>
-- API operation.
newTrafficSourceIdentifier ::
  -- | 'identifier'
  Prelude.Text ->
  TrafficSourceIdentifier
newTrafficSourceIdentifier pIdentifier_ =
  TrafficSourceIdentifier'
    { type' = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | Provides additional context for the value of @Identifier@.
--
-- The following lists the valid values:
--
-- -   @elb@ if @Identifier@ is the name of a Classic Load Balancer.
--
-- -   @elbv2@ if @Identifier@ is the ARN of an Application Load Balancer,
--     Gateway Load Balancer, or Network Load Balancer target group.
--
-- -   @vpc-lattice@ if @Identifier@ is the ARN of a VPC Lattice target
--     group.
--
-- Required if the identifier is the name of a Classic Load Balancer.
trafficSourceIdentifier_type :: Lens.Lens' TrafficSourceIdentifier (Prelude.Maybe Prelude.Text)
trafficSourceIdentifier_type = Lens.lens (\TrafficSourceIdentifier' {type'} -> type') (\s@TrafficSourceIdentifier' {} a -> s {type' = a} :: TrafficSourceIdentifier)

-- | Identifies the traffic source.
--
-- For Application Load Balancers, Gateway Load Balancers, Network Load
-- Balancers, and VPC Lattice, this will be the Amazon Resource Name (ARN)
-- for a target group in this account and Region. For Classic Load
-- Balancers, this will be the name of the Classic Load Balancer in this
-- account and Region.
--
-- For example:
--
-- -   Application Load Balancer ARN:
--     @arn:aws:elasticloadbalancing:us-west-2:123456789012:targetgroup\/my-targets\/1234567890123456@
--
-- -   Classic Load Balancer name: @my-classic-load-balancer@
--
-- -   VPC Lattice ARN:
--     @arn:aws:vpc-lattice:us-west-2:123456789012:targetgroup\/tg-1234567890123456@
--
-- To get the ARN of a target group for a Application Load Balancer,
-- Gateway Load Balancer, or Network Load Balancer, or the name of a
-- Classic Load Balancer, use the Elastic Load Balancing
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeTargetGroups.html DescribeTargetGroups>
-- and
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>
-- API operations.
--
-- To get the ARN of a target group for VPC Lattice, use the VPC Lattice
-- <https://docs.aws.amazon.com/vpc-lattice/latest/APIReference/API_GetTargetGroup.html GetTargetGroup>
-- API operation.
trafficSourceIdentifier_identifier :: Lens.Lens' TrafficSourceIdentifier Prelude.Text
trafficSourceIdentifier_identifier = Lens.lens (\TrafficSourceIdentifier' {identifier} -> identifier) (\s@TrafficSourceIdentifier' {} a -> s {identifier = a} :: TrafficSourceIdentifier)

instance Data.FromXML TrafficSourceIdentifier where
  parseXML x =
    TrafficSourceIdentifier'
      Prelude.<$> (x Data..@? "Type")
      Prelude.<*> (x Data..@ "Identifier")

instance Prelude.Hashable TrafficSourceIdentifier where
  hashWithSalt _salt TrafficSourceIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData TrafficSourceIdentifier where
  rnf TrafficSourceIdentifier' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToQuery TrafficSourceIdentifier where
  toQuery TrafficSourceIdentifier' {..} =
    Prelude.mconcat
      [ "Type" Data.=: type',
        "Identifier" Data.=: identifier
      ]

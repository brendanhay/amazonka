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
-- Module      : Amazonka.AutoScaling.Types.TrafficSourceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.TrafficSourceState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a traffic source.
--
-- /See:/ 'newTrafficSourceState' smart constructor.
data TrafficSourceState = TrafficSourceState'
  { -- | The unique identifier of the traffic source.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | Describes the current state of a traffic source.
    --
    -- The state values are as follows:
    --
    -- -   @Adding@ - The Auto Scaling instances are being registered with the
    --     load balancer or target group.
    --
    -- -   @Added@ - All Auto Scaling instances are registered with the load
    --     balancer or target group.
    --
    -- -   @InService@ - For an Elastic Load Balancing load balancer or target
    --     group, at least one Auto Scaling instance passed an @ELB@ health
    --     check. For VPC Lattice, at least one Auto Scaling instance passed an
    --     @VPC_LATTICE@ health check.
    --
    -- -   @Removing@ - The Auto Scaling instances are being deregistered from
    --     the load balancer or target group. If connection draining
    --     (deregistration delay) is enabled, Elastic Load Balancing or VPC
    --     Lattice waits for in-flight requests to complete before
    --     deregistering the instances.
    --
    -- -   @Removed@ - All Auto Scaling instances are deregistered from the
    --     load balancer or target group.
    state :: Prelude.Maybe Prelude.Text,
    -- | This is replaced by @Identifier@.
    trafficSource :: Prelude.Maybe Prelude.Text,
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
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficSourceState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'trafficSourceState_identifier' - The unique identifier of the traffic source.
--
-- 'state', 'trafficSourceState_state' - Describes the current state of a traffic source.
--
-- The state values are as follows:
--
-- -   @Adding@ - The Auto Scaling instances are being registered with the
--     load balancer or target group.
--
-- -   @Added@ - All Auto Scaling instances are registered with the load
--     balancer or target group.
--
-- -   @InService@ - For an Elastic Load Balancing load balancer or target
--     group, at least one Auto Scaling instance passed an @ELB@ health
--     check. For VPC Lattice, at least one Auto Scaling instance passed an
--     @VPC_LATTICE@ health check.
--
-- -   @Removing@ - The Auto Scaling instances are being deregistered from
--     the load balancer or target group. If connection draining
--     (deregistration delay) is enabled, Elastic Load Balancing or VPC
--     Lattice waits for in-flight requests to complete before
--     deregistering the instances.
--
-- -   @Removed@ - All Auto Scaling instances are deregistered from the
--     load balancer or target group.
--
-- 'trafficSource', 'trafficSourceState_trafficSource' - This is replaced by @Identifier@.
--
-- 'type'', 'trafficSourceState_type' - Provides additional context for the value of @Identifier@.
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
newTrafficSourceState ::
  TrafficSourceState
newTrafficSourceState =
  TrafficSourceState'
    { identifier = Prelude.Nothing,
      state = Prelude.Nothing,
      trafficSource = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The unique identifier of the traffic source.
trafficSourceState_identifier :: Lens.Lens' TrafficSourceState (Prelude.Maybe Prelude.Text)
trafficSourceState_identifier = Lens.lens (\TrafficSourceState' {identifier} -> identifier) (\s@TrafficSourceState' {} a -> s {identifier = a} :: TrafficSourceState)

-- | Describes the current state of a traffic source.
--
-- The state values are as follows:
--
-- -   @Adding@ - The Auto Scaling instances are being registered with the
--     load balancer or target group.
--
-- -   @Added@ - All Auto Scaling instances are registered with the load
--     balancer or target group.
--
-- -   @InService@ - For an Elastic Load Balancing load balancer or target
--     group, at least one Auto Scaling instance passed an @ELB@ health
--     check. For VPC Lattice, at least one Auto Scaling instance passed an
--     @VPC_LATTICE@ health check.
--
-- -   @Removing@ - The Auto Scaling instances are being deregistered from
--     the load balancer or target group. If connection draining
--     (deregistration delay) is enabled, Elastic Load Balancing or VPC
--     Lattice waits for in-flight requests to complete before
--     deregistering the instances.
--
-- -   @Removed@ - All Auto Scaling instances are deregistered from the
--     load balancer or target group.
trafficSourceState_state :: Lens.Lens' TrafficSourceState (Prelude.Maybe Prelude.Text)
trafficSourceState_state = Lens.lens (\TrafficSourceState' {state} -> state) (\s@TrafficSourceState' {} a -> s {state = a} :: TrafficSourceState)

-- | This is replaced by @Identifier@.
trafficSourceState_trafficSource :: Lens.Lens' TrafficSourceState (Prelude.Maybe Prelude.Text)
trafficSourceState_trafficSource = Lens.lens (\TrafficSourceState' {trafficSource} -> trafficSource) (\s@TrafficSourceState' {} a -> s {trafficSource = a} :: TrafficSourceState)

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
trafficSourceState_type :: Lens.Lens' TrafficSourceState (Prelude.Maybe Prelude.Text)
trafficSourceState_type = Lens.lens (\TrafficSourceState' {type'} -> type') (\s@TrafficSourceState' {} a -> s {type' = a} :: TrafficSourceState)

instance Data.FromXML TrafficSourceState where
  parseXML x =
    TrafficSourceState'
      Prelude.<$> (x Data..@? "Identifier")
      Prelude.<*> (x Data..@? "State")
      Prelude.<*> (x Data..@? "TrafficSource")
      Prelude.<*> (x Data..@? "Type")

instance Prelude.Hashable TrafficSourceState where
  hashWithSalt _salt TrafficSourceState' {..} =
    _salt
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` trafficSource
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TrafficSourceState where
  rnf TrafficSourceState' {..} =
    Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf trafficSource
      `Prelude.seq` Prelude.rnf type'

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
-- Module      : Amazonka.Route53.Types.TrafficPolicyInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.TrafficPolicyInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.RRType

-- | A complex type that contains settings for the new traffic policy
-- instance.
--
-- /See:/ 'newTrafficPolicyInstance' smart constructor.
data TrafficPolicyInstance = TrafficPolicyInstance'
  { -- | The ID that Amazon Route 53 assigned to the new traffic policy instance.
    id :: Prelude.Text,
    -- | The ID of the hosted zone that Amazon Route 53 created resource record
    -- sets in.
    hostedZoneId :: ResourceId,
    -- | The DNS name, such as www.example.com, for which Amazon Route 53
    -- responds to queries by using the resource record sets that are
    -- associated with this traffic policy instance.
    name :: Prelude.Text,
    -- | The TTL that Amazon Route 53 assigned to all of the resource record sets
    -- that it created in the specified hosted zone.
    ttl :: Prelude.Natural,
    -- | The value of @State@ is one of the following values:
    --
    -- [Applied]
    --     Amazon Route 53 has finished creating resource record sets, and
    --     changes have propagated to all Route 53 edge locations.
    --
    -- [Creating]
    --     Route 53 is creating the resource record sets. Use
    --     @GetTrafficPolicyInstance@ to confirm that the
    --     @CreateTrafficPolicyInstance@ request completed successfully.
    --
    -- [Failed]
    --     Route 53 wasn\'t able to create or update the resource record sets.
    --     When the value of @State@ is @Failed@, see @Message@ for an
    --     explanation of what caused the request to fail.
    state :: Prelude.Text,
    -- | If @State@ is @Failed@, an explanation of the reason for the failure. If
    -- @State@ is another value, @Message@ is empty.
    message :: Prelude.Text,
    -- | The ID of the traffic policy that Amazon Route 53 used to create
    -- resource record sets in the specified hosted zone.
    trafficPolicyId :: Prelude.Text,
    -- | The version of the traffic policy that Amazon Route 53 used to create
    -- resource record sets in the specified hosted zone.
    trafficPolicyVersion :: Prelude.Natural,
    -- | The DNS type that Amazon Route 53 assigned to all of the resource record
    -- sets that it created for this traffic policy instance.
    trafficPolicyType :: RRType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficPolicyInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'trafficPolicyInstance_id' - The ID that Amazon Route 53 assigned to the new traffic policy instance.
--
-- 'hostedZoneId', 'trafficPolicyInstance_hostedZoneId' - The ID of the hosted zone that Amazon Route 53 created resource record
-- sets in.
--
-- 'name', 'trafficPolicyInstance_name' - The DNS name, such as www.example.com, for which Amazon Route 53
-- responds to queries by using the resource record sets that are
-- associated with this traffic policy instance.
--
-- 'ttl', 'trafficPolicyInstance_ttl' - The TTL that Amazon Route 53 assigned to all of the resource record sets
-- that it created in the specified hosted zone.
--
-- 'state', 'trafficPolicyInstance_state' - The value of @State@ is one of the following values:
--
-- [Applied]
--     Amazon Route 53 has finished creating resource record sets, and
--     changes have propagated to all Route 53 edge locations.
--
-- [Creating]
--     Route 53 is creating the resource record sets. Use
--     @GetTrafficPolicyInstance@ to confirm that the
--     @CreateTrafficPolicyInstance@ request completed successfully.
--
-- [Failed]
--     Route 53 wasn\'t able to create or update the resource record sets.
--     When the value of @State@ is @Failed@, see @Message@ for an
--     explanation of what caused the request to fail.
--
-- 'message', 'trafficPolicyInstance_message' - If @State@ is @Failed@, an explanation of the reason for the failure. If
-- @State@ is another value, @Message@ is empty.
--
-- 'trafficPolicyId', 'trafficPolicyInstance_trafficPolicyId' - The ID of the traffic policy that Amazon Route 53 used to create
-- resource record sets in the specified hosted zone.
--
-- 'trafficPolicyVersion', 'trafficPolicyInstance_trafficPolicyVersion' - The version of the traffic policy that Amazon Route 53 used to create
-- resource record sets in the specified hosted zone.
--
-- 'trafficPolicyType', 'trafficPolicyInstance_trafficPolicyType' - The DNS type that Amazon Route 53 assigned to all of the resource record
-- sets that it created for this traffic policy instance.
newTrafficPolicyInstance ::
  -- | 'id'
  Prelude.Text ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'name'
  Prelude.Text ->
  -- | 'ttl'
  Prelude.Natural ->
  -- | 'state'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  -- | 'trafficPolicyId'
  Prelude.Text ->
  -- | 'trafficPolicyVersion'
  Prelude.Natural ->
  -- | 'trafficPolicyType'
  RRType ->
  TrafficPolicyInstance
newTrafficPolicyInstance
  pId_
  pHostedZoneId_
  pName_
  pTTL_
  pState_
  pMessage_
  pTrafficPolicyId_
  pTrafficPolicyVersion_
  pTrafficPolicyType_ =
    TrafficPolicyInstance'
      { id = pId_,
        hostedZoneId = pHostedZoneId_,
        name = pName_,
        ttl = pTTL_,
        state = pState_,
        message = pMessage_,
        trafficPolicyId = pTrafficPolicyId_,
        trafficPolicyVersion = pTrafficPolicyVersion_,
        trafficPolicyType = pTrafficPolicyType_
      }

-- | The ID that Amazon Route 53 assigned to the new traffic policy instance.
trafficPolicyInstance_id :: Lens.Lens' TrafficPolicyInstance Prelude.Text
trafficPolicyInstance_id = Lens.lens (\TrafficPolicyInstance' {id} -> id) (\s@TrafficPolicyInstance' {} a -> s {id = a} :: TrafficPolicyInstance)

-- | The ID of the hosted zone that Amazon Route 53 created resource record
-- sets in.
trafficPolicyInstance_hostedZoneId :: Lens.Lens' TrafficPolicyInstance ResourceId
trafficPolicyInstance_hostedZoneId = Lens.lens (\TrafficPolicyInstance' {hostedZoneId} -> hostedZoneId) (\s@TrafficPolicyInstance' {} a -> s {hostedZoneId = a} :: TrafficPolicyInstance)

-- | The DNS name, such as www.example.com, for which Amazon Route 53
-- responds to queries by using the resource record sets that are
-- associated with this traffic policy instance.
trafficPolicyInstance_name :: Lens.Lens' TrafficPolicyInstance Prelude.Text
trafficPolicyInstance_name = Lens.lens (\TrafficPolicyInstance' {name} -> name) (\s@TrafficPolicyInstance' {} a -> s {name = a} :: TrafficPolicyInstance)

-- | The TTL that Amazon Route 53 assigned to all of the resource record sets
-- that it created in the specified hosted zone.
trafficPolicyInstance_ttl :: Lens.Lens' TrafficPolicyInstance Prelude.Natural
trafficPolicyInstance_ttl = Lens.lens (\TrafficPolicyInstance' {ttl} -> ttl) (\s@TrafficPolicyInstance' {} a -> s {ttl = a} :: TrafficPolicyInstance)

-- | The value of @State@ is one of the following values:
--
-- [Applied]
--     Amazon Route 53 has finished creating resource record sets, and
--     changes have propagated to all Route 53 edge locations.
--
-- [Creating]
--     Route 53 is creating the resource record sets. Use
--     @GetTrafficPolicyInstance@ to confirm that the
--     @CreateTrafficPolicyInstance@ request completed successfully.
--
-- [Failed]
--     Route 53 wasn\'t able to create or update the resource record sets.
--     When the value of @State@ is @Failed@, see @Message@ for an
--     explanation of what caused the request to fail.
trafficPolicyInstance_state :: Lens.Lens' TrafficPolicyInstance Prelude.Text
trafficPolicyInstance_state = Lens.lens (\TrafficPolicyInstance' {state} -> state) (\s@TrafficPolicyInstance' {} a -> s {state = a} :: TrafficPolicyInstance)

-- | If @State@ is @Failed@, an explanation of the reason for the failure. If
-- @State@ is another value, @Message@ is empty.
trafficPolicyInstance_message :: Lens.Lens' TrafficPolicyInstance Prelude.Text
trafficPolicyInstance_message = Lens.lens (\TrafficPolicyInstance' {message} -> message) (\s@TrafficPolicyInstance' {} a -> s {message = a} :: TrafficPolicyInstance)

-- | The ID of the traffic policy that Amazon Route 53 used to create
-- resource record sets in the specified hosted zone.
trafficPolicyInstance_trafficPolicyId :: Lens.Lens' TrafficPolicyInstance Prelude.Text
trafficPolicyInstance_trafficPolicyId = Lens.lens (\TrafficPolicyInstance' {trafficPolicyId} -> trafficPolicyId) (\s@TrafficPolicyInstance' {} a -> s {trafficPolicyId = a} :: TrafficPolicyInstance)

-- | The version of the traffic policy that Amazon Route 53 used to create
-- resource record sets in the specified hosted zone.
trafficPolicyInstance_trafficPolicyVersion :: Lens.Lens' TrafficPolicyInstance Prelude.Natural
trafficPolicyInstance_trafficPolicyVersion = Lens.lens (\TrafficPolicyInstance' {trafficPolicyVersion} -> trafficPolicyVersion) (\s@TrafficPolicyInstance' {} a -> s {trafficPolicyVersion = a} :: TrafficPolicyInstance)

-- | The DNS type that Amazon Route 53 assigned to all of the resource record
-- sets that it created for this traffic policy instance.
trafficPolicyInstance_trafficPolicyType :: Lens.Lens' TrafficPolicyInstance RRType
trafficPolicyInstance_trafficPolicyType = Lens.lens (\TrafficPolicyInstance' {trafficPolicyType} -> trafficPolicyType) (\s@TrafficPolicyInstance' {} a -> s {trafficPolicyType = a} :: TrafficPolicyInstance)

instance Data.FromXML TrafficPolicyInstance where
  parseXML x =
    TrafficPolicyInstance'
      Prelude.<$> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "HostedZoneId")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "TTL")
      Prelude.<*> (x Data..@ "State")
      Prelude.<*> (x Data..@ "Message")
      Prelude.<*> (x Data..@ "TrafficPolicyId")
      Prelude.<*> (x Data..@ "TrafficPolicyVersion")
      Prelude.<*> (x Data..@ "TrafficPolicyType")

instance Prelude.Hashable TrafficPolicyInstance where
  hashWithSalt _salt TrafficPolicyInstance' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` trafficPolicyId
      `Prelude.hashWithSalt` trafficPolicyVersion
      `Prelude.hashWithSalt` trafficPolicyType

instance Prelude.NFData TrafficPolicyInstance where
  rnf TrafficPolicyInstance' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf trafficPolicyId
      `Prelude.seq` Prelude.rnf trafficPolicyVersion
      `Prelude.seq` Prelude.rnf trafficPolicyType

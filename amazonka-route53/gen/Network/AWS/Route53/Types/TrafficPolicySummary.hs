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
-- Module      : Network.AWS.Route53.Types.TrafficPolicySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.TrafficPolicySummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.RRType

-- | A complex type that contains information about the latest version of one
-- traffic policy that is associated with the current AWS account.
--
-- /See:/ 'newTrafficPolicySummary' smart constructor.
data TrafficPolicySummary = TrafficPolicySummary'
  { -- | The ID that Amazon Route 53 assigned to the traffic policy when you
    -- created it.
    id :: Prelude.Text,
    -- | The name that you specified for the traffic policy when you created it.
    name :: Prelude.Text,
    -- | The DNS type of the resource record sets that Amazon Route 53 creates
    -- when you use a traffic policy to create a traffic policy instance.
    type' :: RRType,
    -- | The version number of the latest version of the traffic policy.
    latestVersion :: Prelude.Natural,
    -- | The number of traffic policies that are associated with the current AWS
    -- account.
    trafficPolicyCount :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficPolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'trafficPolicySummary_id' - The ID that Amazon Route 53 assigned to the traffic policy when you
-- created it.
--
-- 'name', 'trafficPolicySummary_name' - The name that you specified for the traffic policy when you created it.
--
-- 'type'', 'trafficPolicySummary_type' - The DNS type of the resource record sets that Amazon Route 53 creates
-- when you use a traffic policy to create a traffic policy instance.
--
-- 'latestVersion', 'trafficPolicySummary_latestVersion' - The version number of the latest version of the traffic policy.
--
-- 'trafficPolicyCount', 'trafficPolicySummary_trafficPolicyCount' - The number of traffic policies that are associated with the current AWS
-- account.
newTrafficPolicySummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  RRType ->
  -- | 'latestVersion'
  Prelude.Natural ->
  -- | 'trafficPolicyCount'
  Prelude.Natural ->
  TrafficPolicySummary
newTrafficPolicySummary
  pId_
  pName_
  pType_
  pLatestVersion_
  pTrafficPolicyCount_ =
    TrafficPolicySummary'
      { id = pId_,
        name = pName_,
        type' = pType_,
        latestVersion = pLatestVersion_,
        trafficPolicyCount = pTrafficPolicyCount_
      }

-- | The ID that Amazon Route 53 assigned to the traffic policy when you
-- created it.
trafficPolicySummary_id :: Lens.Lens' TrafficPolicySummary Prelude.Text
trafficPolicySummary_id = Lens.lens (\TrafficPolicySummary' {id} -> id) (\s@TrafficPolicySummary' {} a -> s {id = a} :: TrafficPolicySummary)

-- | The name that you specified for the traffic policy when you created it.
trafficPolicySummary_name :: Lens.Lens' TrafficPolicySummary Prelude.Text
trafficPolicySummary_name = Lens.lens (\TrafficPolicySummary' {name} -> name) (\s@TrafficPolicySummary' {} a -> s {name = a} :: TrafficPolicySummary)

-- | The DNS type of the resource record sets that Amazon Route 53 creates
-- when you use a traffic policy to create a traffic policy instance.
trafficPolicySummary_type :: Lens.Lens' TrafficPolicySummary RRType
trafficPolicySummary_type = Lens.lens (\TrafficPolicySummary' {type'} -> type') (\s@TrafficPolicySummary' {} a -> s {type' = a} :: TrafficPolicySummary)

-- | The version number of the latest version of the traffic policy.
trafficPolicySummary_latestVersion :: Lens.Lens' TrafficPolicySummary Prelude.Natural
trafficPolicySummary_latestVersion = Lens.lens (\TrafficPolicySummary' {latestVersion} -> latestVersion) (\s@TrafficPolicySummary' {} a -> s {latestVersion = a} :: TrafficPolicySummary)

-- | The number of traffic policies that are associated with the current AWS
-- account.
trafficPolicySummary_trafficPolicyCount :: Lens.Lens' TrafficPolicySummary Prelude.Natural
trafficPolicySummary_trafficPolicyCount = Lens.lens (\TrafficPolicySummary' {trafficPolicyCount} -> trafficPolicyCount) (\s@TrafficPolicySummary' {} a -> s {trafficPolicyCount = a} :: TrafficPolicySummary)

instance Core.FromXML TrafficPolicySummary where
  parseXML x =
    TrafficPolicySummary'
      Prelude.<$> (x Core..@ "Id")
      Prelude.<*> (x Core..@ "Name")
      Prelude.<*> (x Core..@ "Type")
      Prelude.<*> (x Core..@ "LatestVersion")
      Prelude.<*> (x Core..@ "TrafficPolicyCount")

instance Prelude.Hashable TrafficPolicySummary

instance Prelude.NFData TrafficPolicySummary

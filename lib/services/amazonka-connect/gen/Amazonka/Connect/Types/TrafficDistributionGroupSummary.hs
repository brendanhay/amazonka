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
-- Module      : Amazonka.Connect.Types.TrafficDistributionGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TrafficDistributionGroupSummary where

import Amazonka.Connect.Types.TrafficDistributionGroupStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about traffic distribution groups.
--
-- /See:/ 'newTrafficDistributionGroupSummary' smart constructor.
data TrafficDistributionGroupSummary = TrafficDistributionGroupSummary'
  { -- | The Amazon Resource Name (ARN) of the traffic distribution group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the traffic distribution group. This can be the ID or
    -- the ARN if the API is being called in the Region where the traffic
    -- distribution group was created. The ARN must be provided if the call is
    -- from the replicated Region.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the traffic distribution group.
    instanceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the traffic distribution group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the traffic distribution group.
    --
    -- -   @CREATION_IN_PROGRESS@ means the previous
    --     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
    --     operation is still in progress and has not yet completed.
    --
    -- -   @ACTIVE@ means the previous
    --     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
    --     operation has succeeded.
    --
    -- -   @CREATION_FAILED@ indicates that the previous
    --     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
    --     operation has failed.
    --
    -- -   @PENDING_DELETION@ means the previous
    --     <https://docs.aws.amazon.com/connect/latest/APIReference/API_DeleteTrafficDistributionGroup.html DeleteTrafficDistributionGroup>
    --     operation is still in progress and has not yet completed.
    --
    -- -   @DELETION_FAILED@ means the previous
    --     <https://docs.aws.amazon.com/connect/latest/APIReference/API_DeleteTrafficDistributionGroup.html DeleteTrafficDistributionGroup>
    --     operation has failed.
    --
    -- -   @UPDATE_IN_PROGRESS@ means the previous
    --     <https://docs.aws.amazon.com/connect/latest/APIReference/API_UpdateTrafficDistributionGroup.html UpdateTrafficDistributionGroup>
    --     operation is still in progress and has not yet completed.
    status :: Prelude.Maybe TrafficDistributionGroupStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficDistributionGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'trafficDistributionGroupSummary_arn' - The Amazon Resource Name (ARN) of the traffic distribution group.
--
-- 'id', 'trafficDistributionGroupSummary_id' - The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
--
-- 'instanceArn', 'trafficDistributionGroupSummary_instanceArn' - The Amazon Resource Name (ARN) of the traffic distribution group.
--
-- 'name', 'trafficDistributionGroupSummary_name' - The name of the traffic distribution group.
--
-- 'status', 'trafficDistributionGroupSummary_status' - The status of the traffic distribution group.
--
-- -   @CREATION_IN_PROGRESS@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
--     operation is still in progress and has not yet completed.
--
-- -   @ACTIVE@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
--     operation has succeeded.
--
-- -   @CREATION_FAILED@ indicates that the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
--     operation has failed.
--
-- -   @PENDING_DELETION@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_DeleteTrafficDistributionGroup.html DeleteTrafficDistributionGroup>
--     operation is still in progress and has not yet completed.
--
-- -   @DELETION_FAILED@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_DeleteTrafficDistributionGroup.html DeleteTrafficDistributionGroup>
--     operation has failed.
--
-- -   @UPDATE_IN_PROGRESS@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_UpdateTrafficDistributionGroup.html UpdateTrafficDistributionGroup>
--     operation is still in progress and has not yet completed.
newTrafficDistributionGroupSummary ::
  TrafficDistributionGroupSummary
newTrafficDistributionGroupSummary =
  TrafficDistributionGroupSummary'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      instanceArn = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the traffic distribution group.
trafficDistributionGroupSummary_arn :: Lens.Lens' TrafficDistributionGroupSummary (Prelude.Maybe Prelude.Text)
trafficDistributionGroupSummary_arn = Lens.lens (\TrafficDistributionGroupSummary' {arn} -> arn) (\s@TrafficDistributionGroupSummary' {} a -> s {arn = a} :: TrafficDistributionGroupSummary)

-- | The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
trafficDistributionGroupSummary_id :: Lens.Lens' TrafficDistributionGroupSummary (Prelude.Maybe Prelude.Text)
trafficDistributionGroupSummary_id = Lens.lens (\TrafficDistributionGroupSummary' {id} -> id) (\s@TrafficDistributionGroupSummary' {} a -> s {id = a} :: TrafficDistributionGroupSummary)

-- | The Amazon Resource Name (ARN) of the traffic distribution group.
trafficDistributionGroupSummary_instanceArn :: Lens.Lens' TrafficDistributionGroupSummary (Prelude.Maybe Prelude.Text)
trafficDistributionGroupSummary_instanceArn = Lens.lens (\TrafficDistributionGroupSummary' {instanceArn} -> instanceArn) (\s@TrafficDistributionGroupSummary' {} a -> s {instanceArn = a} :: TrafficDistributionGroupSummary)

-- | The name of the traffic distribution group.
trafficDistributionGroupSummary_name :: Lens.Lens' TrafficDistributionGroupSummary (Prelude.Maybe Prelude.Text)
trafficDistributionGroupSummary_name = Lens.lens (\TrafficDistributionGroupSummary' {name} -> name) (\s@TrafficDistributionGroupSummary' {} a -> s {name = a} :: TrafficDistributionGroupSummary)

-- | The status of the traffic distribution group.
--
-- -   @CREATION_IN_PROGRESS@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
--     operation is still in progress and has not yet completed.
--
-- -   @ACTIVE@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
--     operation has succeeded.
--
-- -   @CREATION_FAILED@ indicates that the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_CreateTrafficDistributionGroup.html CreateTrafficDistributionGroup>
--     operation has failed.
--
-- -   @PENDING_DELETION@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_DeleteTrafficDistributionGroup.html DeleteTrafficDistributionGroup>
--     operation is still in progress and has not yet completed.
--
-- -   @DELETION_FAILED@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_DeleteTrafficDistributionGroup.html DeleteTrafficDistributionGroup>
--     operation has failed.
--
-- -   @UPDATE_IN_PROGRESS@ means the previous
--     <https://docs.aws.amazon.com/connect/latest/APIReference/API_UpdateTrafficDistributionGroup.html UpdateTrafficDistributionGroup>
--     operation is still in progress and has not yet completed.
trafficDistributionGroupSummary_status :: Lens.Lens' TrafficDistributionGroupSummary (Prelude.Maybe TrafficDistributionGroupStatus)
trafficDistributionGroupSummary_status = Lens.lens (\TrafficDistributionGroupSummary' {status} -> status) (\s@TrafficDistributionGroupSummary' {} a -> s {status = a} :: TrafficDistributionGroupSummary)

instance
  Data.FromJSON
    TrafficDistributionGroupSummary
  where
  parseJSON =
    Data.withObject
      "TrafficDistributionGroupSummary"
      ( \x ->
          TrafficDistributionGroupSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InstanceArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    TrafficDistributionGroupSummary
  where
  hashWithSalt
    _salt
    TrafficDistributionGroupSummary' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` instanceArn
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    TrafficDistributionGroupSummary
  where
  rnf TrafficDistributionGroupSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status

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
-- Module      : Amazonka.Connect.Types.TrafficDistributionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TrafficDistributionGroup where

import Amazonka.Connect.Types.TrafficDistributionGroupStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a traffic distribution group.
--
-- /See:/ 'newTrafficDistributionGroup' smart constructor.
data TrafficDistributionGroup = TrafficDistributionGroup'
  { -- | The Amazon Resource Name (ARN) of the traffic distribution group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the traffic distribution group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the traffic distribution group. This can be the ID or
    -- the ARN if the API is being called in the Region where the traffic
    -- distribution group was created. The ARN must be provided if the call is
    -- from the replicated Region.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN).
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
    status :: Prelude.Maybe TrafficDistributionGroupStatus,
    -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficDistributionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'trafficDistributionGroup_arn' - The Amazon Resource Name (ARN) of the traffic distribution group.
--
-- 'description', 'trafficDistributionGroup_description' - The description of the traffic distribution group.
--
-- 'id', 'trafficDistributionGroup_id' - The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
--
-- 'instanceArn', 'trafficDistributionGroup_instanceArn' - The Amazon Resource Name (ARN).
--
-- 'name', 'trafficDistributionGroup_name' - The name of the traffic distribution group.
--
-- 'status', 'trafficDistributionGroup_status' - The status of the traffic distribution group.
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
--
-- 'tags', 'trafficDistributionGroup_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
newTrafficDistributionGroup ::
  TrafficDistributionGroup
newTrafficDistributionGroup =
  TrafficDistributionGroup'
    { arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      instanceArn = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the traffic distribution group.
trafficDistributionGroup_arn :: Lens.Lens' TrafficDistributionGroup (Prelude.Maybe Prelude.Text)
trafficDistributionGroup_arn = Lens.lens (\TrafficDistributionGroup' {arn} -> arn) (\s@TrafficDistributionGroup' {} a -> s {arn = a} :: TrafficDistributionGroup)

-- | The description of the traffic distribution group.
trafficDistributionGroup_description :: Lens.Lens' TrafficDistributionGroup (Prelude.Maybe Prelude.Text)
trafficDistributionGroup_description = Lens.lens (\TrafficDistributionGroup' {description} -> description) (\s@TrafficDistributionGroup' {} a -> s {description = a} :: TrafficDistributionGroup)

-- | The identifier of the traffic distribution group. This can be the ID or
-- the ARN if the API is being called in the Region where the traffic
-- distribution group was created. The ARN must be provided if the call is
-- from the replicated Region.
trafficDistributionGroup_id :: Lens.Lens' TrafficDistributionGroup (Prelude.Maybe Prelude.Text)
trafficDistributionGroup_id = Lens.lens (\TrafficDistributionGroup' {id} -> id) (\s@TrafficDistributionGroup' {} a -> s {id = a} :: TrafficDistributionGroup)

-- | The Amazon Resource Name (ARN).
trafficDistributionGroup_instanceArn :: Lens.Lens' TrafficDistributionGroup (Prelude.Maybe Prelude.Text)
trafficDistributionGroup_instanceArn = Lens.lens (\TrafficDistributionGroup' {instanceArn} -> instanceArn) (\s@TrafficDistributionGroup' {} a -> s {instanceArn = a} :: TrafficDistributionGroup)

-- | The name of the traffic distribution group.
trafficDistributionGroup_name :: Lens.Lens' TrafficDistributionGroup (Prelude.Maybe Prelude.Text)
trafficDistributionGroup_name = Lens.lens (\TrafficDistributionGroup' {name} -> name) (\s@TrafficDistributionGroup' {} a -> s {name = a} :: TrafficDistributionGroup)

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
trafficDistributionGroup_status :: Lens.Lens' TrafficDistributionGroup (Prelude.Maybe TrafficDistributionGroupStatus)
trafficDistributionGroup_status = Lens.lens (\TrafficDistributionGroup' {status} -> status) (\s@TrafficDistributionGroup' {} a -> s {status = a} :: TrafficDistributionGroup)

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
trafficDistributionGroup_tags :: Lens.Lens' TrafficDistributionGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
trafficDistributionGroup_tags = Lens.lens (\TrafficDistributionGroup' {tags} -> tags) (\s@TrafficDistributionGroup' {} a -> s {tags = a} :: TrafficDistributionGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TrafficDistributionGroup where
  parseJSON =
    Data.withObject
      "TrafficDistributionGroup"
      ( \x ->
          TrafficDistributionGroup'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InstanceArn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TrafficDistributionGroup where
  hashWithSalt _salt TrafficDistributionGroup' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` instanceArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags

instance Prelude.NFData TrafficDistributionGroup where
  rnf TrafficDistributionGroup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf instanceArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags

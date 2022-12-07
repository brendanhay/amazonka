{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.GetSpotPlacementScores
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Calculates the Spot placement score for a Region or Availability Zone
-- based on the specified target capacity and compute requirements.
--
-- You can specify your compute requirements either by using
-- @InstanceRequirementsWithMetadata@ and letting Amazon EC2 choose the
-- optimal instance types to fulfill your Spot request, or you can specify
-- the instance types by using @InstanceTypes@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-placement-score.html Spot placement score>
-- in the Amazon EC2 User Guide.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetSpotPlacementScores
  ( -- * Creating a Request
    GetSpotPlacementScores (..),
    newGetSpotPlacementScores,

    -- * Request Lenses
    getSpotPlacementScores_regionNames,
    getSpotPlacementScores_nextToken,
    getSpotPlacementScores_instanceTypes,
    getSpotPlacementScores_singleAvailabilityZone,
    getSpotPlacementScores_dryRun,
    getSpotPlacementScores_targetCapacityUnitType,
    getSpotPlacementScores_maxResults,
    getSpotPlacementScores_instanceRequirementsWithMetadata,
    getSpotPlacementScores_targetCapacity,

    -- * Destructuring the Response
    GetSpotPlacementScoresResponse (..),
    newGetSpotPlacementScoresResponse,

    -- * Response Lenses
    getSpotPlacementScoresResponse_nextToken,
    getSpotPlacementScoresResponse_spotPlacementScores,
    getSpotPlacementScoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSpotPlacementScores' smart constructor.
data GetSpotPlacementScores = GetSpotPlacementScores'
  { -- | The Regions used to narrow down the list of Regions to be scored. Enter
    -- the Region code, for example, @us-east-1@.
    regionNames :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The instance types. We recommend that you specify at least three
    -- instance types. If you specify one or two instance types, or specify
    -- variations of a single instance type (for example, an @m3.xlarge@ with
    -- and without instance storage), the returned placement score will always
    -- be low.
    --
    -- If you specify @InstanceTypes@, you can\'t specify
    -- @InstanceRequirementsWithMetadata@.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | Specify @true@ so that the response returns a list of scored
    -- Availability Zones. Otherwise, the response returns a list of scored
    -- Regions.
    --
    -- A list of scored Availability Zones is useful if you want to launch all
    -- of your Spot capacity into a single Availability Zone.
    singleAvailabilityZone :: Prelude.Maybe Prelude.Bool,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The unit for the target capacity.
    --
    -- Default: @units@ (translates to number of instances)
    targetCapacityUnitType :: Prelude.Maybe TargetCapacityUnitType,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and  1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with  the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The attributes for the instance types. When you specify instance
    -- attributes, Amazon EC2 will identify instance types with those
    -- attributes.
    --
    -- If you specify @InstanceRequirementsWithMetadata@, you can\'t specify
    -- @InstanceTypes@.
    instanceRequirementsWithMetadata :: Prelude.Maybe InstanceRequirementsWithMetadataRequest,
    -- | The target capacity.
    targetCapacity :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSpotPlacementScores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionNames', 'getSpotPlacementScores_regionNames' - The Regions used to narrow down the list of Regions to be scored. Enter
-- the Region code, for example, @us-east-1@.
--
-- 'nextToken', 'getSpotPlacementScores_nextToken' - The token for the next set of results.
--
-- 'instanceTypes', 'getSpotPlacementScores_instanceTypes' - The instance types. We recommend that you specify at least three
-- instance types. If you specify one or two instance types, or specify
-- variations of a single instance type (for example, an @m3.xlarge@ with
-- and without instance storage), the returned placement score will always
-- be low.
--
-- If you specify @InstanceTypes@, you can\'t specify
-- @InstanceRequirementsWithMetadata@.
--
-- 'singleAvailabilityZone', 'getSpotPlacementScores_singleAvailabilityZone' - Specify @true@ so that the response returns a list of scored
-- Availability Zones. Otherwise, the response returns a list of scored
-- Regions.
--
-- A list of scored Availability Zones is useful if you want to launch all
-- of your Spot capacity into a single Availability Zone.
--
-- 'dryRun', 'getSpotPlacementScores_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'targetCapacityUnitType', 'getSpotPlacementScores_targetCapacityUnitType' - The unit for the target capacity.
--
-- Default: @units@ (translates to number of instances)
--
-- 'maxResults', 'getSpotPlacementScores_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and  1000. The default value is 1000. To retrieve the
-- remaining results, make another call with  the returned @NextToken@
-- value.
--
-- 'instanceRequirementsWithMetadata', 'getSpotPlacementScores_instanceRequirementsWithMetadata' - The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- If you specify @InstanceRequirementsWithMetadata@, you can\'t specify
-- @InstanceTypes@.
--
-- 'targetCapacity', 'getSpotPlacementScores_targetCapacity' - The target capacity.
newGetSpotPlacementScores ::
  -- | 'targetCapacity'
  Prelude.Natural ->
  GetSpotPlacementScores
newGetSpotPlacementScores pTargetCapacity_ =
  GetSpotPlacementScores'
    { regionNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      singleAvailabilityZone = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      targetCapacityUnitType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceRequirementsWithMetadata = Prelude.Nothing,
      targetCapacity = pTargetCapacity_
    }

-- | The Regions used to narrow down the list of Regions to be scored. Enter
-- the Region code, for example, @us-east-1@.
getSpotPlacementScores_regionNames :: Lens.Lens' GetSpotPlacementScores (Prelude.Maybe [Prelude.Text])
getSpotPlacementScores_regionNames = Lens.lens (\GetSpotPlacementScores' {regionNames} -> regionNames) (\s@GetSpotPlacementScores' {} a -> s {regionNames = a} :: GetSpotPlacementScores) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results.
getSpotPlacementScores_nextToken :: Lens.Lens' GetSpotPlacementScores (Prelude.Maybe Prelude.Text)
getSpotPlacementScores_nextToken = Lens.lens (\GetSpotPlacementScores' {nextToken} -> nextToken) (\s@GetSpotPlacementScores' {} a -> s {nextToken = a} :: GetSpotPlacementScores)

-- | The instance types. We recommend that you specify at least three
-- instance types. If you specify one or two instance types, or specify
-- variations of a single instance type (for example, an @m3.xlarge@ with
-- and without instance storage), the returned placement score will always
-- be low.
--
-- If you specify @InstanceTypes@, you can\'t specify
-- @InstanceRequirementsWithMetadata@.
getSpotPlacementScores_instanceTypes :: Lens.Lens' GetSpotPlacementScores (Prelude.Maybe [Prelude.Text])
getSpotPlacementScores_instanceTypes = Lens.lens (\GetSpotPlacementScores' {instanceTypes} -> instanceTypes) (\s@GetSpotPlacementScores' {} a -> s {instanceTypes = a} :: GetSpotPlacementScores) Prelude.. Lens.mapping Lens.coerced

-- | Specify @true@ so that the response returns a list of scored
-- Availability Zones. Otherwise, the response returns a list of scored
-- Regions.
--
-- A list of scored Availability Zones is useful if you want to launch all
-- of your Spot capacity into a single Availability Zone.
getSpotPlacementScores_singleAvailabilityZone :: Lens.Lens' GetSpotPlacementScores (Prelude.Maybe Prelude.Bool)
getSpotPlacementScores_singleAvailabilityZone = Lens.lens (\GetSpotPlacementScores' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@GetSpotPlacementScores' {} a -> s {singleAvailabilityZone = a} :: GetSpotPlacementScores)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getSpotPlacementScores_dryRun :: Lens.Lens' GetSpotPlacementScores (Prelude.Maybe Prelude.Bool)
getSpotPlacementScores_dryRun = Lens.lens (\GetSpotPlacementScores' {dryRun} -> dryRun) (\s@GetSpotPlacementScores' {} a -> s {dryRun = a} :: GetSpotPlacementScores)

-- | The unit for the target capacity.
--
-- Default: @units@ (translates to number of instances)
getSpotPlacementScores_targetCapacityUnitType :: Lens.Lens' GetSpotPlacementScores (Prelude.Maybe TargetCapacityUnitType)
getSpotPlacementScores_targetCapacityUnitType = Lens.lens (\GetSpotPlacementScores' {targetCapacityUnitType} -> targetCapacityUnitType) (\s@GetSpotPlacementScores' {} a -> s {targetCapacityUnitType = a} :: GetSpotPlacementScores)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and  1000. The default value is 1000. To retrieve the
-- remaining results, make another call with  the returned @NextToken@
-- value.
getSpotPlacementScores_maxResults :: Lens.Lens' GetSpotPlacementScores (Prelude.Maybe Prelude.Natural)
getSpotPlacementScores_maxResults = Lens.lens (\GetSpotPlacementScores' {maxResults} -> maxResults) (\s@GetSpotPlacementScores' {} a -> s {maxResults = a} :: GetSpotPlacementScores)

-- | The attributes for the instance types. When you specify instance
-- attributes, Amazon EC2 will identify instance types with those
-- attributes.
--
-- If you specify @InstanceRequirementsWithMetadata@, you can\'t specify
-- @InstanceTypes@.
getSpotPlacementScores_instanceRequirementsWithMetadata :: Lens.Lens' GetSpotPlacementScores (Prelude.Maybe InstanceRequirementsWithMetadataRequest)
getSpotPlacementScores_instanceRequirementsWithMetadata = Lens.lens (\GetSpotPlacementScores' {instanceRequirementsWithMetadata} -> instanceRequirementsWithMetadata) (\s@GetSpotPlacementScores' {} a -> s {instanceRequirementsWithMetadata = a} :: GetSpotPlacementScores)

-- | The target capacity.
getSpotPlacementScores_targetCapacity :: Lens.Lens' GetSpotPlacementScores Prelude.Natural
getSpotPlacementScores_targetCapacity = Lens.lens (\GetSpotPlacementScores' {targetCapacity} -> targetCapacity) (\s@GetSpotPlacementScores' {} a -> s {targetCapacity = a} :: GetSpotPlacementScores)

instance Core.AWSPager GetSpotPlacementScores where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getSpotPlacementScoresResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getSpotPlacementScoresResponse_spotPlacementScores
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getSpotPlacementScores_nextToken
          Lens..~ rs
          Lens.^? getSpotPlacementScoresResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetSpotPlacementScores where
  type
    AWSResponse GetSpotPlacementScores =
      GetSpotPlacementScoresResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetSpotPlacementScoresResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "spotPlacementScoreSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSpotPlacementScores where
  hashWithSalt _salt GetSpotPlacementScores' {..} =
    _salt `Prelude.hashWithSalt` regionNames
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceTypes
      `Prelude.hashWithSalt` singleAvailabilityZone
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` targetCapacityUnitType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceRequirementsWithMetadata
      `Prelude.hashWithSalt` targetCapacity

instance Prelude.NFData GetSpotPlacementScores where
  rnf GetSpotPlacementScores' {..} =
    Prelude.rnf regionNames
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf singleAvailabilityZone
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf targetCapacityUnitType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceRequirementsWithMetadata
      `Prelude.seq` Prelude.rnf targetCapacity

instance Data.ToHeaders GetSpotPlacementScores where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSpotPlacementScores where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSpotPlacementScores where
  toQuery GetSpotPlacementScores' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetSpotPlacementScores" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "RegionName"
              Prelude.<$> regionNames
          ),
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "InstanceType"
              Prelude.<$> instanceTypes
          ),
        "SingleAvailabilityZone"
          Data.=: singleAvailabilityZone,
        "DryRun" Data.=: dryRun,
        "TargetCapacityUnitType"
          Data.=: targetCapacityUnitType,
        "MaxResults" Data.=: maxResults,
        "InstanceRequirementsWithMetadata"
          Data.=: instanceRequirementsWithMetadata,
        "TargetCapacity" Data.=: targetCapacity
      ]

-- | /See:/ 'newGetSpotPlacementScoresResponse' smart constructor.
data GetSpotPlacementScoresResponse = GetSpotPlacementScoresResponse'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Spot placement score for the top 10 Regions or Availability Zones,
    -- scored on a scale from 1 to 10. Each score  reflects how likely it is
    -- that each Region or Availability Zone will succeed at fulfilling the
    -- specified target capacity  /at the time of the Spot placement score
    -- request/. A score of @10@ means that your Spot capacity request is
    -- highly likely to succeed in that Region or Availability Zone.
    --
    -- If you request a Spot placement score for Regions, a high score assumes
    -- that your fleet request will be configured to use all Availability Zones
    -- and the @capacity-optimized@ allocation strategy. If you request a Spot
    -- placement score for Availability Zones, a high score assumes that your
    -- fleet request will be configured to use a single Availability Zone and
    -- the @capacity-optimized@ allocation strategy.
    --
    -- Different  Regions or Availability Zones might return the same score.
    --
    -- The Spot placement score serves as a recommendation only. No score
    -- guarantees that your Spot request will be fully or partially fulfilled.
    spotPlacementScores :: Prelude.Maybe [SpotPlacementScore],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSpotPlacementScoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getSpotPlacementScoresResponse_nextToken' - The token for the next set of results.
--
-- 'spotPlacementScores', 'getSpotPlacementScoresResponse_spotPlacementScores' - The Spot placement score for the top 10 Regions or Availability Zones,
-- scored on a scale from 1 to 10. Each score  reflects how likely it is
-- that each Region or Availability Zone will succeed at fulfilling the
-- specified target capacity  /at the time of the Spot placement score
-- request/. A score of @10@ means that your Spot capacity request is
-- highly likely to succeed in that Region or Availability Zone.
--
-- If you request a Spot placement score for Regions, a high score assumes
-- that your fleet request will be configured to use all Availability Zones
-- and the @capacity-optimized@ allocation strategy. If you request a Spot
-- placement score for Availability Zones, a high score assumes that your
-- fleet request will be configured to use a single Availability Zone and
-- the @capacity-optimized@ allocation strategy.
--
-- Different  Regions or Availability Zones might return the same score.
--
-- The Spot placement score serves as a recommendation only. No score
-- guarantees that your Spot request will be fully or partially fulfilled.
--
-- 'httpStatus', 'getSpotPlacementScoresResponse_httpStatus' - The response's http status code.
newGetSpotPlacementScoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSpotPlacementScoresResponse
newGetSpotPlacementScoresResponse pHttpStatus_ =
  GetSpotPlacementScoresResponse'
    { nextToken =
        Prelude.Nothing,
      spotPlacementScores = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results.
getSpotPlacementScoresResponse_nextToken :: Lens.Lens' GetSpotPlacementScoresResponse (Prelude.Maybe Prelude.Text)
getSpotPlacementScoresResponse_nextToken = Lens.lens (\GetSpotPlacementScoresResponse' {nextToken} -> nextToken) (\s@GetSpotPlacementScoresResponse' {} a -> s {nextToken = a} :: GetSpotPlacementScoresResponse)

-- | The Spot placement score for the top 10 Regions or Availability Zones,
-- scored on a scale from 1 to 10. Each score  reflects how likely it is
-- that each Region or Availability Zone will succeed at fulfilling the
-- specified target capacity  /at the time of the Spot placement score
-- request/. A score of @10@ means that your Spot capacity request is
-- highly likely to succeed in that Region or Availability Zone.
--
-- If you request a Spot placement score for Regions, a high score assumes
-- that your fleet request will be configured to use all Availability Zones
-- and the @capacity-optimized@ allocation strategy. If you request a Spot
-- placement score for Availability Zones, a high score assumes that your
-- fleet request will be configured to use a single Availability Zone and
-- the @capacity-optimized@ allocation strategy.
--
-- Different  Regions or Availability Zones might return the same score.
--
-- The Spot placement score serves as a recommendation only. No score
-- guarantees that your Spot request will be fully or partially fulfilled.
getSpotPlacementScoresResponse_spotPlacementScores :: Lens.Lens' GetSpotPlacementScoresResponse (Prelude.Maybe [SpotPlacementScore])
getSpotPlacementScoresResponse_spotPlacementScores = Lens.lens (\GetSpotPlacementScoresResponse' {spotPlacementScores} -> spotPlacementScores) (\s@GetSpotPlacementScoresResponse' {} a -> s {spotPlacementScores = a} :: GetSpotPlacementScoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSpotPlacementScoresResponse_httpStatus :: Lens.Lens' GetSpotPlacementScoresResponse Prelude.Int
getSpotPlacementScoresResponse_httpStatus = Lens.lens (\GetSpotPlacementScoresResponse' {httpStatus} -> httpStatus) (\s@GetSpotPlacementScoresResponse' {} a -> s {httpStatus = a} :: GetSpotPlacementScoresResponse)

instance
  Prelude.NFData
    GetSpotPlacementScoresResponse
  where
  rnf GetSpotPlacementScoresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf spotPlacementScores
      `Prelude.seq` Prelude.rnf httpStatus

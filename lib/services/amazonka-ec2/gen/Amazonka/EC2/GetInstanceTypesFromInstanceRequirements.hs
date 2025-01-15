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
-- Module      : Amazonka.EC2.GetInstanceTypesFromInstanceRequirements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of instance types with the specified instance attributes.
-- You can use the response to preview the instance types without launching
-- instances. Note that the response does not consider capacity.
--
-- When you specify multiple parameters, you get instance types that
-- satisfy all of the specified parameters. If you specify multiple values
-- for a parameter, you get instance types that satisfy any of the
-- specified values.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-attribute-based-instance-type-selection.html#spotfleet-get-instance-types-from-instance-requirements Preview instance types with specified attributes>,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-attribute-based-instance-type-selection.html Attribute-based instance type selection for EC2 Fleet>,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-attribute-based-instance-type-selection.html Attribute-based instance type selection for Spot Fleet>,
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-placement-score.html Spot placement score>
-- in the /Amazon EC2 User Guide/, and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/create-asg-instance-type-requirements.html Creating an Auto Scaling group using attribute-based instance type selection>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.GetInstanceTypesFromInstanceRequirements
  ( -- * Creating a Request
    GetInstanceTypesFromInstanceRequirements (..),
    newGetInstanceTypesFromInstanceRequirements,

    -- * Request Lenses
    getInstanceTypesFromInstanceRequirements_dryRun,
    getInstanceTypesFromInstanceRequirements_maxResults,
    getInstanceTypesFromInstanceRequirements_nextToken,
    getInstanceTypesFromInstanceRequirements_architectureTypes,
    getInstanceTypesFromInstanceRequirements_virtualizationTypes,
    getInstanceTypesFromInstanceRequirements_instanceRequirements,

    -- * Destructuring the Response
    GetInstanceTypesFromInstanceRequirementsResponse (..),
    newGetInstanceTypesFromInstanceRequirementsResponse,

    -- * Response Lenses
    getInstanceTypesFromInstanceRequirementsResponse_instanceTypes,
    getInstanceTypesFromInstanceRequirementsResponse_nextToken,
    getInstanceTypesFromInstanceRequirementsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInstanceTypesFromInstanceRequirements' smart constructor.
data GetInstanceTypesFromInstanceRequirements = GetInstanceTypesFromInstanceRequirements'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return in a single call. Specify a
    -- value between 1 and  1000. The default value is 1000. To retrieve the
    -- remaining results, make another call with  the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The processor architecture type.
    architectureTypes :: [ArchitectureType],
    -- | The virtualization type.
    virtualizationTypes :: [VirtualizationType],
    -- | The attributes required for the instance types.
    instanceRequirements :: InstanceRequirementsRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceTypesFromInstanceRequirements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getInstanceTypesFromInstanceRequirements_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getInstanceTypesFromInstanceRequirements_maxResults' - The maximum number of results to return in a single call. Specify a
-- value between 1 and  1000. The default value is 1000. To retrieve the
-- remaining results, make another call with  the returned @NextToken@
-- value.
--
-- 'nextToken', 'getInstanceTypesFromInstanceRequirements_nextToken' - The token for the next set of results.
--
-- 'architectureTypes', 'getInstanceTypesFromInstanceRequirements_architectureTypes' - The processor architecture type.
--
-- 'virtualizationTypes', 'getInstanceTypesFromInstanceRequirements_virtualizationTypes' - The virtualization type.
--
-- 'instanceRequirements', 'getInstanceTypesFromInstanceRequirements_instanceRequirements' - The attributes required for the instance types.
newGetInstanceTypesFromInstanceRequirements ::
  -- | 'instanceRequirements'
  InstanceRequirementsRequest ->
  GetInstanceTypesFromInstanceRequirements
newGetInstanceTypesFromInstanceRequirements
  pInstanceRequirements_ =
    GetInstanceTypesFromInstanceRequirements'
      { dryRun =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        architectureTypes =
          Prelude.mempty,
        virtualizationTypes =
          Prelude.mempty,
        instanceRequirements =
          pInstanceRequirements_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getInstanceTypesFromInstanceRequirements_dryRun :: Lens.Lens' GetInstanceTypesFromInstanceRequirements (Prelude.Maybe Prelude.Bool)
getInstanceTypesFromInstanceRequirements_dryRun = Lens.lens (\GetInstanceTypesFromInstanceRequirements' {dryRun} -> dryRun) (\s@GetInstanceTypesFromInstanceRequirements' {} a -> s {dryRun = a} :: GetInstanceTypesFromInstanceRequirements)

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and  1000. The default value is 1000. To retrieve the
-- remaining results, make another call with  the returned @NextToken@
-- value.
getInstanceTypesFromInstanceRequirements_maxResults :: Lens.Lens' GetInstanceTypesFromInstanceRequirements (Prelude.Maybe Prelude.Int)
getInstanceTypesFromInstanceRequirements_maxResults = Lens.lens (\GetInstanceTypesFromInstanceRequirements' {maxResults} -> maxResults) (\s@GetInstanceTypesFromInstanceRequirements' {} a -> s {maxResults = a} :: GetInstanceTypesFromInstanceRequirements)

-- | The token for the next set of results.
getInstanceTypesFromInstanceRequirements_nextToken :: Lens.Lens' GetInstanceTypesFromInstanceRequirements (Prelude.Maybe Prelude.Text)
getInstanceTypesFromInstanceRequirements_nextToken = Lens.lens (\GetInstanceTypesFromInstanceRequirements' {nextToken} -> nextToken) (\s@GetInstanceTypesFromInstanceRequirements' {} a -> s {nextToken = a} :: GetInstanceTypesFromInstanceRequirements)

-- | The processor architecture type.
getInstanceTypesFromInstanceRequirements_architectureTypes :: Lens.Lens' GetInstanceTypesFromInstanceRequirements [ArchitectureType]
getInstanceTypesFromInstanceRequirements_architectureTypes = Lens.lens (\GetInstanceTypesFromInstanceRequirements' {architectureTypes} -> architectureTypes) (\s@GetInstanceTypesFromInstanceRequirements' {} a -> s {architectureTypes = a} :: GetInstanceTypesFromInstanceRequirements) Prelude.. Lens.coerced

-- | The virtualization type.
getInstanceTypesFromInstanceRequirements_virtualizationTypes :: Lens.Lens' GetInstanceTypesFromInstanceRequirements [VirtualizationType]
getInstanceTypesFromInstanceRequirements_virtualizationTypes = Lens.lens (\GetInstanceTypesFromInstanceRequirements' {virtualizationTypes} -> virtualizationTypes) (\s@GetInstanceTypesFromInstanceRequirements' {} a -> s {virtualizationTypes = a} :: GetInstanceTypesFromInstanceRequirements) Prelude.. Lens.coerced

-- | The attributes required for the instance types.
getInstanceTypesFromInstanceRequirements_instanceRequirements :: Lens.Lens' GetInstanceTypesFromInstanceRequirements InstanceRequirementsRequest
getInstanceTypesFromInstanceRequirements_instanceRequirements = Lens.lens (\GetInstanceTypesFromInstanceRequirements' {instanceRequirements} -> instanceRequirements) (\s@GetInstanceTypesFromInstanceRequirements' {} a -> s {instanceRequirements = a} :: GetInstanceTypesFromInstanceRequirements)

instance
  Core.AWSPager
    GetInstanceTypesFromInstanceRequirements
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInstanceTypesFromInstanceRequirementsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getInstanceTypesFromInstanceRequirementsResponse_instanceTypes
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getInstanceTypesFromInstanceRequirements_nextToken
              Lens..~ rs
              Lens.^? getInstanceTypesFromInstanceRequirementsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetInstanceTypesFromInstanceRequirements
  where
  type
    AWSResponse
      GetInstanceTypesFromInstanceRequirements =
      GetInstanceTypesFromInstanceRequirementsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetInstanceTypesFromInstanceRequirementsResponse'
            Prelude.<$> ( x Data..@? "instanceTypeSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetInstanceTypesFromInstanceRequirements
  where
  hashWithSalt
    _salt
    GetInstanceTypesFromInstanceRequirements' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` architectureTypes
        `Prelude.hashWithSalt` virtualizationTypes
        `Prelude.hashWithSalt` instanceRequirements

instance
  Prelude.NFData
    GetInstanceTypesFromInstanceRequirements
  where
  rnf GetInstanceTypesFromInstanceRequirements' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf architectureTypes `Prelude.seq`
            Prelude.rnf virtualizationTypes `Prelude.seq`
              Prelude.rnf instanceRequirements

instance
  Data.ToHeaders
    GetInstanceTypesFromInstanceRequirements
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetInstanceTypesFromInstanceRequirements
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetInstanceTypesFromInstanceRequirements
  where
  toQuery GetInstanceTypesFromInstanceRequirements' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetInstanceTypesFromInstanceRequirements" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQueryList
          "ArchitectureType"
          architectureTypes,
        Data.toQueryList
          "VirtualizationType"
          virtualizationTypes,
        "InstanceRequirements" Data.=: instanceRequirements
      ]

-- | /See:/ 'newGetInstanceTypesFromInstanceRequirementsResponse' smart constructor.
data GetInstanceTypesFromInstanceRequirementsResponse = GetInstanceTypesFromInstanceRequirementsResponse'
  { -- | The instance types with the specified instance attributes.
    instanceTypes :: Prelude.Maybe [InstanceTypeInfoFromInstanceRequirements],
    -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceTypesFromInstanceRequirementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTypes', 'getInstanceTypesFromInstanceRequirementsResponse_instanceTypes' - The instance types with the specified instance attributes.
--
-- 'nextToken', 'getInstanceTypesFromInstanceRequirementsResponse_nextToken' - The token for the next set of results.
--
-- 'httpStatus', 'getInstanceTypesFromInstanceRequirementsResponse_httpStatus' - The response's http status code.
newGetInstanceTypesFromInstanceRequirementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstanceTypesFromInstanceRequirementsResponse
newGetInstanceTypesFromInstanceRequirementsResponse
  pHttpStatus_ =
    GetInstanceTypesFromInstanceRequirementsResponse'
      { instanceTypes =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The instance types with the specified instance attributes.
getInstanceTypesFromInstanceRequirementsResponse_instanceTypes :: Lens.Lens' GetInstanceTypesFromInstanceRequirementsResponse (Prelude.Maybe [InstanceTypeInfoFromInstanceRequirements])
getInstanceTypesFromInstanceRequirementsResponse_instanceTypes = Lens.lens (\GetInstanceTypesFromInstanceRequirementsResponse' {instanceTypes} -> instanceTypes) (\s@GetInstanceTypesFromInstanceRequirementsResponse' {} a -> s {instanceTypes = a} :: GetInstanceTypesFromInstanceRequirementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results.
getInstanceTypesFromInstanceRequirementsResponse_nextToken :: Lens.Lens' GetInstanceTypesFromInstanceRequirementsResponse (Prelude.Maybe Prelude.Text)
getInstanceTypesFromInstanceRequirementsResponse_nextToken = Lens.lens (\GetInstanceTypesFromInstanceRequirementsResponse' {nextToken} -> nextToken) (\s@GetInstanceTypesFromInstanceRequirementsResponse' {} a -> s {nextToken = a} :: GetInstanceTypesFromInstanceRequirementsResponse)

-- | The response's http status code.
getInstanceTypesFromInstanceRequirementsResponse_httpStatus :: Lens.Lens' GetInstanceTypesFromInstanceRequirementsResponse Prelude.Int
getInstanceTypesFromInstanceRequirementsResponse_httpStatus = Lens.lens (\GetInstanceTypesFromInstanceRequirementsResponse' {httpStatus} -> httpStatus) (\s@GetInstanceTypesFromInstanceRequirementsResponse' {} a -> s {httpStatus = a} :: GetInstanceTypesFromInstanceRequirementsResponse)

instance
  Prelude.NFData
    GetInstanceTypesFromInstanceRequirementsResponse
  where
  rnf
    GetInstanceTypesFromInstanceRequirementsResponse' {..} =
      Prelude.rnf instanceTypes `Prelude.seq`
        Prelude.rnf nextToken `Prelude.seq`
          Prelude.rnf httpStatus

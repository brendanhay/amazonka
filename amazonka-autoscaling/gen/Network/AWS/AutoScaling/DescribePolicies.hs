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
-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the policies for the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribePolicies
  ( -- * Creating a Request
    DescribePolicies (..),
    newDescribePolicies,

    -- * Request Lenses
    describePolicies_nextToken,
    describePolicies_policyTypes,
    describePolicies_policyNames,
    describePolicies_autoScalingGroupName,
    describePolicies_maxRecords,

    -- * Destructuring the Response
    DescribePoliciesResponse (..),
    newDescribePoliciesResponse,

    -- * Response Lenses
    describePoliciesResponse_nextToken,
    describePoliciesResponse_scalingPolicies,
    describePoliciesResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePolicies' smart constructor.
data DescribePolicies = DescribePolicies'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more policy types. The valid values are @SimpleScaling@,
    -- @StepScaling@, and @TargetTrackingScaling@.
    policyTypes :: Prelude.Maybe [Prelude.Text],
    -- | The names of one or more policies. If you omit this parameter, all
    -- policies are described. If a group name is provided, the results are
    -- limited to that group. This list is limited to 50 items. If you specify
    -- an unknown policy name, it is ignored with no error.
    policyNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to be returned with each call. The default
    -- value is @50@ and the maximum value is @100@.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePolicies_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'policyTypes', 'describePolicies_policyTypes' - One or more policy types. The valid values are @SimpleScaling@,
-- @StepScaling@, and @TargetTrackingScaling@.
--
-- 'policyNames', 'describePolicies_policyNames' - The names of one or more policies. If you omit this parameter, all
-- policies are described. If a group name is provided, the results are
-- limited to that group. This list is limited to 50 items. If you specify
-- an unknown policy name, it is ignored with no error.
--
-- 'autoScalingGroupName', 'describePolicies_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'maxRecords', 'describePolicies_maxRecords' - The maximum number of items to be returned with each call. The default
-- value is @50@ and the maximum value is @100@.
newDescribePolicies ::
  DescribePolicies
newDescribePolicies =
  DescribePolicies'
    { nextToken = Prelude.Nothing,
      policyTypes = Prelude.Nothing,
      policyNames = Prelude.Nothing,
      autoScalingGroupName = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describePolicies_nextToken :: Lens.Lens' DescribePolicies (Prelude.Maybe Prelude.Text)
describePolicies_nextToken = Lens.lens (\DescribePolicies' {nextToken} -> nextToken) (\s@DescribePolicies' {} a -> s {nextToken = a} :: DescribePolicies)

-- | One or more policy types. The valid values are @SimpleScaling@,
-- @StepScaling@, and @TargetTrackingScaling@.
describePolicies_policyTypes :: Lens.Lens' DescribePolicies (Prelude.Maybe [Prelude.Text])
describePolicies_policyTypes = Lens.lens (\DescribePolicies' {policyTypes} -> policyTypes) (\s@DescribePolicies' {} a -> s {policyTypes = a} :: DescribePolicies) Prelude.. Lens.mapping Lens._Coerce

-- | The names of one or more policies. If you omit this parameter, all
-- policies are described. If a group name is provided, the results are
-- limited to that group. This list is limited to 50 items. If you specify
-- an unknown policy name, it is ignored with no error.
describePolicies_policyNames :: Lens.Lens' DescribePolicies (Prelude.Maybe [Prelude.Text])
describePolicies_policyNames = Lens.lens (\DescribePolicies' {policyNames} -> policyNames) (\s@DescribePolicies' {} a -> s {policyNames = a} :: DescribePolicies) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
describePolicies_autoScalingGroupName :: Lens.Lens' DescribePolicies (Prelude.Maybe Prelude.Text)
describePolicies_autoScalingGroupName = Lens.lens (\DescribePolicies' {autoScalingGroupName} -> autoScalingGroupName) (\s@DescribePolicies' {} a -> s {autoScalingGroupName = a} :: DescribePolicies)

-- | The maximum number of items to be returned with each call. The default
-- value is @50@ and the maximum value is @100@.
describePolicies_maxRecords :: Lens.Lens' DescribePolicies (Prelude.Maybe Prelude.Int)
describePolicies_maxRecords = Lens.lens (\DescribePolicies' {maxRecords} -> maxRecords) (\s@DescribePolicies' {} a -> s {maxRecords = a} :: DescribePolicies)

instance Core.AWSPager DescribePolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describePoliciesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describePoliciesResponse_scalingPolicies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describePolicies_nextToken
          Lens..~ rs
          Lens.^? describePoliciesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribePolicies where
  type
    AWSResponse DescribePolicies =
      DescribePoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribePoliciesResult"
      ( \s h x ->
          DescribePoliciesResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "ScalingPolicies" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePolicies

instance Prelude.NFData DescribePolicies

instance Core.ToHeaders DescribePolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribePolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePolicies where
  toQuery DescribePolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribePolicies" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "PolicyTypes"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> policyTypes),
        "PolicyNames"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> policyNames),
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribePoliciesResponse' smart constructor.
data DescribePoliciesResponse = DescribePoliciesResponse'
  { -- | A string that indicates that the response contains more items than can
    -- be returned in a single response. To receive additional items, specify
    -- this string for the @NextToken@ value when requesting the next set of
    -- items. This value is null when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The scaling policies.
    scalingPolicies :: Prelude.Maybe [ScalingPolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describePoliciesResponse_nextToken' - A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
--
-- 'scalingPolicies', 'describePoliciesResponse_scalingPolicies' - The scaling policies.
--
-- 'httpStatus', 'describePoliciesResponse_httpStatus' - The response's http status code.
newDescribePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePoliciesResponse
newDescribePoliciesResponse pHttpStatus_ =
  DescribePoliciesResponse'
    { nextToken =
        Prelude.Nothing,
      scalingPolicies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that indicates that the response contains more items than can
-- be returned in a single response. To receive additional items, specify
-- this string for the @NextToken@ value when requesting the next set of
-- items. This value is null when there are no more items to return.
describePoliciesResponse_nextToken :: Lens.Lens' DescribePoliciesResponse (Prelude.Maybe Prelude.Text)
describePoliciesResponse_nextToken = Lens.lens (\DescribePoliciesResponse' {nextToken} -> nextToken) (\s@DescribePoliciesResponse' {} a -> s {nextToken = a} :: DescribePoliciesResponse)

-- | The scaling policies.
describePoliciesResponse_scalingPolicies :: Lens.Lens' DescribePoliciesResponse (Prelude.Maybe [ScalingPolicy])
describePoliciesResponse_scalingPolicies = Lens.lens (\DescribePoliciesResponse' {scalingPolicies} -> scalingPolicies) (\s@DescribePoliciesResponse' {} a -> s {scalingPolicies = a} :: DescribePoliciesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describePoliciesResponse_httpStatus :: Lens.Lens' DescribePoliciesResponse Prelude.Int
describePoliciesResponse_httpStatus = Lens.lens (\DescribePoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribePoliciesResponse' {} a -> s {httpStatus = a} :: DescribePoliciesResponse)

instance Prelude.NFData DescribePoliciesResponse

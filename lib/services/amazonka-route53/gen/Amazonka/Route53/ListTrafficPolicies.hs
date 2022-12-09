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
-- Module      : Amazonka.Route53.ListTrafficPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the latest version for every traffic policy that
-- is associated with the current Amazon Web Services account. Policies are
-- listed in the order that they were created in.
--
-- For information about how of deleting a traffic policy affects the
-- response from @ListTrafficPolicies@, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicy.html DeleteTrafficPolicy>.
module Amazonka.Route53.ListTrafficPolicies
  ( -- * Creating a Request
    ListTrafficPolicies (..),
    newListTrafficPolicies,

    -- * Request Lenses
    listTrafficPolicies_maxItems,
    listTrafficPolicies_trafficPolicyIdMarker,

    -- * Destructuring the Response
    ListTrafficPoliciesResponse (..),
    newListTrafficPoliciesResponse,

    -- * Response Lenses
    listTrafficPoliciesResponse_httpStatus,
    listTrafficPoliciesResponse_trafficPolicySummaries,
    listTrafficPoliciesResponse_isTruncated,
    listTrafficPoliciesResponse_trafficPolicyIdMarker,
    listTrafficPoliciesResponse_maxItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains the information about the request to list
-- the traffic policies that are associated with the current Amazon Web
-- Services account.
--
-- /See:/ 'newListTrafficPolicies' smart constructor.
data ListTrafficPolicies = ListTrafficPolicies'
  { -- | (Optional) The maximum number of traffic policies that you want Amazon
    -- Route 53 to return in response to this request. If you have more than
    -- @MaxItems@ traffic policies, the value of @IsTruncated@ in the response
    -- is @true@, and the value of @TrafficPolicyIdMarker@ is the ID of the
    -- first traffic policy that Route 53 will return if you submit another
    -- request.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | (Conditional) For your first request to @ListTrafficPolicies@, don\'t
    -- include the @TrafficPolicyIdMarker@ parameter.
    --
    -- If you have more traffic policies than the value of @MaxItems@,
    -- @ListTrafficPolicies@ returns only the first @MaxItems@ traffic
    -- policies. To get the next group of policies, submit another request to
    -- @ListTrafficPolicies@. For the value of @TrafficPolicyIdMarker@, specify
    -- the value of @TrafficPolicyIdMarker@ that was returned in the previous
    -- response.
    trafficPolicyIdMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listTrafficPolicies_maxItems' - (Optional) The maximum number of traffic policies that you want Amazon
-- Route 53 to return in response to this request. If you have more than
-- @MaxItems@ traffic policies, the value of @IsTruncated@ in the response
-- is @true@, and the value of @TrafficPolicyIdMarker@ is the ID of the
-- first traffic policy that Route 53 will return if you submit another
-- request.
--
-- 'trafficPolicyIdMarker', 'listTrafficPolicies_trafficPolicyIdMarker' - (Conditional) For your first request to @ListTrafficPolicies@, don\'t
-- include the @TrafficPolicyIdMarker@ parameter.
--
-- If you have more traffic policies than the value of @MaxItems@,
-- @ListTrafficPolicies@ returns only the first @MaxItems@ traffic
-- policies. To get the next group of policies, submit another request to
-- @ListTrafficPolicies@. For the value of @TrafficPolicyIdMarker@, specify
-- the value of @TrafficPolicyIdMarker@ that was returned in the previous
-- response.
newListTrafficPolicies ::
  ListTrafficPolicies
newListTrafficPolicies =
  ListTrafficPolicies'
    { maxItems = Prelude.Nothing,
      trafficPolicyIdMarker = Prelude.Nothing
    }

-- | (Optional) The maximum number of traffic policies that you want Amazon
-- Route 53 to return in response to this request. If you have more than
-- @MaxItems@ traffic policies, the value of @IsTruncated@ in the response
-- is @true@, and the value of @TrafficPolicyIdMarker@ is the ID of the
-- first traffic policy that Route 53 will return if you submit another
-- request.
listTrafficPolicies_maxItems :: Lens.Lens' ListTrafficPolicies (Prelude.Maybe Prelude.Text)
listTrafficPolicies_maxItems = Lens.lens (\ListTrafficPolicies' {maxItems} -> maxItems) (\s@ListTrafficPolicies' {} a -> s {maxItems = a} :: ListTrafficPolicies)

-- | (Conditional) For your first request to @ListTrafficPolicies@, don\'t
-- include the @TrafficPolicyIdMarker@ parameter.
--
-- If you have more traffic policies than the value of @MaxItems@,
-- @ListTrafficPolicies@ returns only the first @MaxItems@ traffic
-- policies. To get the next group of policies, submit another request to
-- @ListTrafficPolicies@. For the value of @TrafficPolicyIdMarker@, specify
-- the value of @TrafficPolicyIdMarker@ that was returned in the previous
-- response.
listTrafficPolicies_trafficPolicyIdMarker :: Lens.Lens' ListTrafficPolicies (Prelude.Maybe Prelude.Text)
listTrafficPolicies_trafficPolicyIdMarker = Lens.lens (\ListTrafficPolicies' {trafficPolicyIdMarker} -> trafficPolicyIdMarker) (\s@ListTrafficPolicies' {} a -> s {trafficPolicyIdMarker = a} :: ListTrafficPolicies)

instance Core.AWSRequest ListTrafficPolicies where
  type
    AWSResponse ListTrafficPolicies =
      ListTrafficPoliciesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListTrafficPoliciesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "TrafficPolicySummaries"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "TrafficPolicySummary"
                        )
            Prelude.<*> (x Data..@ "IsTruncated")
            Prelude.<*> (x Data..@ "TrafficPolicyIdMarker")
            Prelude.<*> (x Data..@ "MaxItems")
      )

instance Prelude.Hashable ListTrafficPolicies where
  hashWithSalt _salt ListTrafficPolicies' {..} =
    _salt `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` trafficPolicyIdMarker

instance Prelude.NFData ListTrafficPolicies where
  rnf ListTrafficPolicies' {..} =
    Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf trafficPolicyIdMarker

instance Data.ToHeaders ListTrafficPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTrafficPolicies where
  toPath = Prelude.const "/2013-04-01/trafficpolicies"

instance Data.ToQuery ListTrafficPolicies where
  toQuery ListTrafficPolicies' {..} =
    Prelude.mconcat
      [ "maxitems" Data.=: maxItems,
        "trafficpolicyid" Data.=: trafficPolicyIdMarker
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'newListTrafficPoliciesResponse' smart constructor.
data ListTrafficPoliciesResponse = ListTrafficPoliciesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains one @TrafficPolicySummary@ element for each traffic
    -- policy that was created by the current Amazon Web Services account.
    trafficPolicySummaries :: [TrafficPolicySummary],
    -- | A flag that indicates whether there are more traffic policies to be
    -- listed. If the response was truncated, you can get the next group of
    -- traffic policies by submitting another @ListTrafficPolicies@ request and
    -- specifying the value of @TrafficPolicyIdMarker@ in the
    -- @TrafficPolicyIdMarker@ request parameter.
    isTruncated :: Prelude.Bool,
    -- | If the value of @IsTruncated@ is @true@, @TrafficPolicyIdMarker@ is the
    -- ID of the first traffic policy in the next group of @MaxItems@ traffic
    -- policies.
    trafficPolicyIdMarker :: Prelude.Text,
    -- | The value that you specified for the @MaxItems@ parameter in the
    -- @ListTrafficPolicies@ request that produced the current response.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTrafficPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicySummaries', 'listTrafficPoliciesResponse_trafficPolicySummaries' - A list that contains one @TrafficPolicySummary@ element for each traffic
-- policy that was created by the current Amazon Web Services account.
--
-- 'isTruncated', 'listTrafficPoliciesResponse_isTruncated' - A flag that indicates whether there are more traffic policies to be
-- listed. If the response was truncated, you can get the next group of
-- traffic policies by submitting another @ListTrafficPolicies@ request and
-- specifying the value of @TrafficPolicyIdMarker@ in the
-- @TrafficPolicyIdMarker@ request parameter.
--
-- 'trafficPolicyIdMarker', 'listTrafficPoliciesResponse_trafficPolicyIdMarker' - If the value of @IsTruncated@ is @true@, @TrafficPolicyIdMarker@ is the
-- ID of the first traffic policy in the next group of @MaxItems@ traffic
-- policies.
--
-- 'maxItems', 'listTrafficPoliciesResponse_maxItems' - The value that you specified for the @MaxItems@ parameter in the
-- @ListTrafficPolicies@ request that produced the current response.
newListTrafficPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'trafficPolicyIdMarker'
  Prelude.Text ->
  -- | 'maxItems'
  Prelude.Text ->
  ListTrafficPoliciesResponse
newListTrafficPoliciesResponse
  pHttpStatus_
  pIsTruncated_
  pTrafficPolicyIdMarker_
  pMaxItems_ =
    ListTrafficPoliciesResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicySummaries = Prelude.mempty,
        isTruncated = pIsTruncated_,
        trafficPolicyIdMarker =
          pTrafficPolicyIdMarker_,
        maxItems = pMaxItems_
      }

-- | The response's http status code.
listTrafficPoliciesResponse_httpStatus :: Lens.Lens' ListTrafficPoliciesResponse Prelude.Int
listTrafficPoliciesResponse_httpStatus = Lens.lens (\ListTrafficPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListTrafficPoliciesResponse' {} a -> s {httpStatus = a} :: ListTrafficPoliciesResponse)

-- | A list that contains one @TrafficPolicySummary@ element for each traffic
-- policy that was created by the current Amazon Web Services account.
listTrafficPoliciesResponse_trafficPolicySummaries :: Lens.Lens' ListTrafficPoliciesResponse [TrafficPolicySummary]
listTrafficPoliciesResponse_trafficPolicySummaries = Lens.lens (\ListTrafficPoliciesResponse' {trafficPolicySummaries} -> trafficPolicySummaries) (\s@ListTrafficPoliciesResponse' {} a -> s {trafficPolicySummaries = a} :: ListTrafficPoliciesResponse) Prelude.. Lens.coerced

-- | A flag that indicates whether there are more traffic policies to be
-- listed. If the response was truncated, you can get the next group of
-- traffic policies by submitting another @ListTrafficPolicies@ request and
-- specifying the value of @TrafficPolicyIdMarker@ in the
-- @TrafficPolicyIdMarker@ request parameter.
listTrafficPoliciesResponse_isTruncated :: Lens.Lens' ListTrafficPoliciesResponse Prelude.Bool
listTrafficPoliciesResponse_isTruncated = Lens.lens (\ListTrafficPoliciesResponse' {isTruncated} -> isTruncated) (\s@ListTrafficPoliciesResponse' {} a -> s {isTruncated = a} :: ListTrafficPoliciesResponse)

-- | If the value of @IsTruncated@ is @true@, @TrafficPolicyIdMarker@ is the
-- ID of the first traffic policy in the next group of @MaxItems@ traffic
-- policies.
listTrafficPoliciesResponse_trafficPolicyIdMarker :: Lens.Lens' ListTrafficPoliciesResponse Prelude.Text
listTrafficPoliciesResponse_trafficPolicyIdMarker = Lens.lens (\ListTrafficPoliciesResponse' {trafficPolicyIdMarker} -> trafficPolicyIdMarker) (\s@ListTrafficPoliciesResponse' {} a -> s {trafficPolicyIdMarker = a} :: ListTrafficPoliciesResponse)

-- | The value that you specified for the @MaxItems@ parameter in the
-- @ListTrafficPolicies@ request that produced the current response.
listTrafficPoliciesResponse_maxItems :: Lens.Lens' ListTrafficPoliciesResponse Prelude.Text
listTrafficPoliciesResponse_maxItems = Lens.lens (\ListTrafficPoliciesResponse' {maxItems} -> maxItems) (\s@ListTrafficPoliciesResponse' {} a -> s {maxItems = a} :: ListTrafficPoliciesResponse)

instance Prelude.NFData ListTrafficPoliciesResponse where
  rnf ListTrafficPoliciesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trafficPolicySummaries
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf trafficPolicyIdMarker
      `Prelude.seq` Prelude.rnf maxItems

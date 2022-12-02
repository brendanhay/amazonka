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
-- Module      : Amazonka.Route53.ListTrafficPolicyInstancesByPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by
-- using a specify traffic policy version.
--
-- After you submit a @CreateTrafficPolicyInstance@ or an
-- @UpdateTrafficPolicyInstance@ request, there\'s a brief delay while
-- Amazon Route 53 creates the resource record sets that are specified in
-- the traffic policy definition. For more information, see the @State@
-- response element.
--
-- Route 53 returns a maximum of 100 items in each response. If you have a
-- lot of traffic policy instances, you can use the @MaxItems@ parameter to
-- list them in groups of up to 100.
module Amazonka.Route53.ListTrafficPolicyInstancesByPolicy
  ( -- * Creating a Request
    ListTrafficPolicyInstancesByPolicy (..),
    newListTrafficPolicyInstancesByPolicy,

    -- * Request Lenses
    listTrafficPolicyInstancesByPolicy_hostedZoneIdMarker,
    listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByPolicy_maxItems,
    listTrafficPolicyInstancesByPolicy_trafficPolicyId,
    listTrafficPolicyInstancesByPolicy_trafficPolicyVersion,

    -- * Destructuring the Response
    ListTrafficPolicyInstancesByPolicyResponse (..),
    newListTrafficPolicyInstancesByPolicyResponse,

    -- * Response Lenses
    listTrafficPolicyInstancesByPolicyResponse_hostedZoneIdMarker,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByPolicyResponse_httpStatus,
    listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesByPolicyResponse_isTruncated,
    listTrafficPolicyInstancesByPolicyResponse_maxItems,
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
-- your traffic policy instances.
--
-- /See:/ 'newListTrafficPolicyInstancesByPolicy' smart constructor.
data ListTrafficPolicyInstancesByPolicy = ListTrafficPolicyInstancesByPolicy'
  { -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more traffic policy instances. To get more traffic policy
    -- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
    --
    -- For the value of @hostedzoneid@, specify the value of
    -- @HostedZoneIdMarker@ from the previous response, which is the hosted
    -- zone ID of the first traffic policy instance that Amazon Route 53 will
    -- return if you submit another request.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more traffic policy instances to get.
    hostedZoneIdMarker :: Prelude.Maybe ResourceId,
    -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more traffic policy instances. To get more traffic policy
    -- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
    --
    -- For the value of @trafficpolicyinstancetype@, specify the value of
    -- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
    -- the name of the first traffic policy instance that Amazon Route 53 will
    -- return if you submit another request.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more traffic policy instances to get.
    trafficPolicyInstanceTypeMarker :: Prelude.Maybe RRType,
    -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more traffic policy instances. To get more traffic policy
    -- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
    --
    -- For the value of @trafficpolicyinstancename@, specify the value of
    -- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
    -- the name of the first traffic policy instance that Amazon Route 53 will
    -- return if you submit another request.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more traffic policy instances to get.
    trafficPolicyInstanceNameMarker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of traffic policy instances to be included in the
    -- response body for this request. If you have more than @MaxItems@ traffic
    -- policy instances, the value of the @IsTruncated@ element in the response
    -- is @true@, and the values of @HostedZoneIdMarker@,
    -- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
    -- represent the first traffic policy instance that Amazon Route 53 will
    -- return if you submit another request.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | The ID of the traffic policy for which you want to list traffic policy
    -- instances.
    trafficPolicyId :: Prelude.Text,
    -- | The version of the traffic policy for which you want to list traffic
    -- policy instances. The version must be associated with the traffic policy
    -- that is specified by @TrafficPolicyId@.
    trafficPolicyVersion :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicyInstancesByPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneIdMarker', 'listTrafficPolicyInstancesByPolicy_hostedZoneIdMarker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @hostedzoneid@, specify the value of
-- @HostedZoneIdMarker@ from the previous response, which is the hosted
-- zone ID of the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
--
-- 'trafficPolicyInstanceTypeMarker', 'listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceTypeMarker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancetype@, specify the value of
-- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
-- the name of the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
--
-- 'trafficPolicyInstanceNameMarker', 'listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceNameMarker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancename@, specify the value of
-- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
-- the name of the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
--
-- 'maxItems', 'listTrafficPolicyInstancesByPolicy_maxItems' - The maximum number of traffic policy instances to be included in the
-- response body for this request. If you have more than @MaxItems@ traffic
-- policy instances, the value of the @IsTruncated@ element in the response
-- is @true@, and the values of @HostedZoneIdMarker@,
-- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
-- represent the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
--
-- 'trafficPolicyId', 'listTrafficPolicyInstancesByPolicy_trafficPolicyId' - The ID of the traffic policy for which you want to list traffic policy
-- instances.
--
-- 'trafficPolicyVersion', 'listTrafficPolicyInstancesByPolicy_trafficPolicyVersion' - The version of the traffic policy for which you want to list traffic
-- policy instances. The version must be associated with the traffic policy
-- that is specified by @TrafficPolicyId@.
newListTrafficPolicyInstancesByPolicy ::
  -- | 'trafficPolicyId'
  Prelude.Text ->
  -- | 'trafficPolicyVersion'
  Prelude.Natural ->
  ListTrafficPolicyInstancesByPolicy
newListTrafficPolicyInstancesByPolicy
  pTrafficPolicyId_
  pTrafficPolicyVersion_ =
    ListTrafficPolicyInstancesByPolicy'
      { hostedZoneIdMarker =
          Prelude.Nothing,
        trafficPolicyInstanceTypeMarker =
          Prelude.Nothing,
        trafficPolicyInstanceNameMarker =
          Prelude.Nothing,
        maxItems = Prelude.Nothing,
        trafficPolicyId = pTrafficPolicyId_,
        trafficPolicyVersion =
          pTrafficPolicyVersion_
      }

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @hostedzoneid@, specify the value of
-- @HostedZoneIdMarker@ from the previous response, which is the hosted
-- zone ID of the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
listTrafficPolicyInstancesByPolicy_hostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Prelude.Maybe ResourceId)
listTrafficPolicyInstancesByPolicy_hostedZoneIdMarker = Lens.lens (\ListTrafficPolicyInstancesByPolicy' {hostedZoneIdMarker} -> hostedZoneIdMarker) (\s@ListTrafficPolicyInstancesByPolicy' {} a -> s {hostedZoneIdMarker = a} :: ListTrafficPolicyInstancesByPolicy)

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancetype@, specify the value of
-- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
-- the name of the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Prelude.Maybe RRType)
listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceTypeMarker = Lens.lens (\ListTrafficPolicyInstancesByPolicy' {trafficPolicyInstanceTypeMarker} -> trafficPolicyInstanceTypeMarker) (\s@ListTrafficPolicyInstancesByPolicy' {} a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesByPolicy)

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancename@, specify the value of
-- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
-- the name of the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstancesByPolicy_trafficPolicyInstanceNameMarker = Lens.lens (\ListTrafficPolicyInstancesByPolicy' {trafficPolicyInstanceNameMarker} -> trafficPolicyInstanceNameMarker) (\s@ListTrafficPolicyInstancesByPolicy' {} a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesByPolicy)

-- | The maximum number of traffic policy instances to be included in the
-- response body for this request. If you have more than @MaxItems@ traffic
-- policy instances, the value of the @IsTruncated@ element in the response
-- is @true@, and the values of @HostedZoneIdMarker@,
-- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
-- represent the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
listTrafficPolicyInstancesByPolicy_maxItems :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstancesByPolicy_maxItems = Lens.lens (\ListTrafficPolicyInstancesByPolicy' {maxItems} -> maxItems) (\s@ListTrafficPolicyInstancesByPolicy' {} a -> s {maxItems = a} :: ListTrafficPolicyInstancesByPolicy)

-- | The ID of the traffic policy for which you want to list traffic policy
-- instances.
listTrafficPolicyInstancesByPolicy_trafficPolicyId :: Lens.Lens' ListTrafficPolicyInstancesByPolicy Prelude.Text
listTrafficPolicyInstancesByPolicy_trafficPolicyId = Lens.lens (\ListTrafficPolicyInstancesByPolicy' {trafficPolicyId} -> trafficPolicyId) (\s@ListTrafficPolicyInstancesByPolicy' {} a -> s {trafficPolicyId = a} :: ListTrafficPolicyInstancesByPolicy)

-- | The version of the traffic policy for which you want to list traffic
-- policy instances. The version must be associated with the traffic policy
-- that is specified by @TrafficPolicyId@.
listTrafficPolicyInstancesByPolicy_trafficPolicyVersion :: Lens.Lens' ListTrafficPolicyInstancesByPolicy Prelude.Natural
listTrafficPolicyInstancesByPolicy_trafficPolicyVersion = Lens.lens (\ListTrafficPolicyInstancesByPolicy' {trafficPolicyVersion} -> trafficPolicyVersion) (\s@ListTrafficPolicyInstancesByPolicy' {} a -> s {trafficPolicyVersion = a} :: ListTrafficPolicyInstancesByPolicy)

instance
  Core.AWSRequest
    ListTrafficPolicyInstancesByPolicy
  where
  type
    AWSResponse ListTrafficPolicyInstancesByPolicy =
      ListTrafficPolicyInstancesByPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListTrafficPolicyInstancesByPolicyResponse'
            Prelude.<$> (x Data..@? "HostedZoneIdMarker")
              Prelude.<*> (x Data..@? "TrafficPolicyInstanceTypeMarker")
              Prelude.<*> (x Data..@? "TrafficPolicyInstanceNameMarker")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Data..@? "TrafficPolicyInstances"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Data.parseXMLList "TrafficPolicyInstance"
                          )
              Prelude.<*> (x Data..@ "IsTruncated")
              Prelude.<*> (x Data..@ "MaxItems")
      )

instance
  Prelude.Hashable
    ListTrafficPolicyInstancesByPolicy
  where
  hashWithSalt
    _salt
    ListTrafficPolicyInstancesByPolicy' {..} =
      _salt `Prelude.hashWithSalt` hostedZoneIdMarker
        `Prelude.hashWithSalt` trafficPolicyInstanceTypeMarker
        `Prelude.hashWithSalt` trafficPolicyInstanceNameMarker
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` trafficPolicyId
        `Prelude.hashWithSalt` trafficPolicyVersion

instance
  Prelude.NFData
    ListTrafficPolicyInstancesByPolicy
  where
  rnf ListTrafficPolicyInstancesByPolicy' {..} =
    Prelude.rnf hostedZoneIdMarker
      `Prelude.seq` Prelude.rnf trafficPolicyInstanceTypeMarker
      `Prelude.seq` Prelude.rnf trafficPolicyInstanceNameMarker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf trafficPolicyId
      `Prelude.seq` Prelude.rnf trafficPolicyVersion

instance
  Data.ToHeaders
    ListTrafficPolicyInstancesByPolicy
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListTrafficPolicyInstancesByPolicy
  where
  toPath =
    Prelude.const
      "/2013-04-01/trafficpolicyinstances/trafficpolicy"

instance
  Data.ToQuery
    ListTrafficPolicyInstancesByPolicy
  where
  toQuery ListTrafficPolicyInstancesByPolicy' {..} =
    Prelude.mconcat
      [ "hostedzoneid" Data.=: hostedZoneIdMarker,
        "trafficpolicyinstancetype"
          Data.=: trafficPolicyInstanceTypeMarker,
        "trafficpolicyinstancename"
          Data.=: trafficPolicyInstanceNameMarker,
        "maxitems" Data.=: maxItems,
        "id" Data.=: trafficPolicyId,
        "version" Data.=: trafficPolicyVersion
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'newListTrafficPolicyInstancesByPolicyResponse' smart constructor.
data ListTrafficPolicyInstancesByPolicyResponse = ListTrafficPolicyInstancesByPolicyResponse'
  { -- | If @IsTruncated@ is @true@, @HostedZoneIdMarker@ is the ID of the hosted
    -- zone of the first traffic policy instance in the next group of traffic
    -- policy instances.
    hostedZoneIdMarker :: Prelude.Maybe ResourceId,
    -- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceTypeMarker@ is the DNS
    -- type of the resource record sets that are associated with the first
    -- traffic policy instance in the next group of @MaxItems@ traffic policy
    -- instances.
    trafficPolicyInstanceTypeMarker :: Prelude.Maybe RRType,
    -- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
    -- name of the first traffic policy instance in the next group of
    -- @MaxItems@ traffic policy instances.
    trafficPolicyInstanceNameMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains one @TrafficPolicyInstance@ element for each
    -- traffic policy instance that matches the elements in the request.
    trafficPolicyInstances :: [TrafficPolicyInstance],
    -- | A flag that indicates whether there are more traffic policy instances to
    -- be listed. If the response was truncated, you can get the next group of
    -- traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@
    -- again and specifying the values of the @HostedZoneIdMarker@,
    -- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
    -- elements in the corresponding request parameters.
    isTruncated :: Prelude.Bool,
    -- | The value that you specified for the @MaxItems@ parameter in the call to
    -- @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicyInstancesByPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneIdMarker', 'listTrafficPolicyInstancesByPolicyResponse_hostedZoneIdMarker' - If @IsTruncated@ is @true@, @HostedZoneIdMarker@ is the ID of the hosted
-- zone of the first traffic policy instance in the next group of traffic
-- policy instances.
--
-- 'trafficPolicyInstanceTypeMarker', 'listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceTypeMarker' - If @IsTruncated@ is @true@, @TrafficPolicyInstanceTypeMarker@ is the DNS
-- type of the resource record sets that are associated with the first
-- traffic policy instance in the next group of @MaxItems@ traffic policy
-- instances.
--
-- 'trafficPolicyInstanceNameMarker', 'listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceNameMarker' - If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
-- name of the first traffic policy instance in the next group of
-- @MaxItems@ traffic policy instances.
--
-- 'httpStatus', 'listTrafficPolicyInstancesByPolicyResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicyInstances', 'listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstances' - A list that contains one @TrafficPolicyInstance@ element for each
-- traffic policy instance that matches the elements in the request.
--
-- 'isTruncated', 'listTrafficPolicyInstancesByPolicyResponse_isTruncated' - A flag that indicates whether there are more traffic policy instances to
-- be listed. If the response was truncated, you can get the next group of
-- traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@
-- again and specifying the values of the @HostedZoneIdMarker@,
-- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
-- elements in the corresponding request parameters.
--
-- 'maxItems', 'listTrafficPolicyInstancesByPolicyResponse_maxItems' - The value that you specified for the @MaxItems@ parameter in the call to
-- @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
newListTrafficPolicyInstancesByPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'maxItems'
  Prelude.Text ->
  ListTrafficPolicyInstancesByPolicyResponse
newListTrafficPolicyInstancesByPolicyResponse
  pHttpStatus_
  pIsTruncated_
  pMaxItems_ =
    ListTrafficPolicyInstancesByPolicyResponse'
      { hostedZoneIdMarker =
          Prelude.Nothing,
        trafficPolicyInstanceTypeMarker =
          Prelude.Nothing,
        trafficPolicyInstanceNameMarker =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        trafficPolicyInstances =
          Prelude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, @HostedZoneIdMarker@ is the ID of the hosted
-- zone of the first traffic policy instance in the next group of traffic
-- policy instances.
listTrafficPolicyInstancesByPolicyResponse_hostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Prelude.Maybe ResourceId)
listTrafficPolicyInstancesByPolicyResponse_hostedZoneIdMarker = Lens.lens (\ListTrafficPolicyInstancesByPolicyResponse' {hostedZoneIdMarker} -> hostedZoneIdMarker) (\s@ListTrafficPolicyInstancesByPolicyResponse' {} a -> s {hostedZoneIdMarker = a} :: ListTrafficPolicyInstancesByPolicyResponse)

-- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceTypeMarker@ is the DNS
-- type of the resource record sets that are associated with the first
-- traffic policy instance in the next group of @MaxItems@ traffic policy
-- instances.
listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Prelude.Maybe RRType)
listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceTypeMarker = Lens.lens (\ListTrafficPolicyInstancesByPolicyResponse' {trafficPolicyInstanceTypeMarker} -> trafficPolicyInstanceTypeMarker) (\s@ListTrafficPolicyInstancesByPolicyResponse' {} a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesByPolicyResponse)

-- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
-- name of the first traffic policy instance in the next group of
-- @MaxItems@ traffic policy instances.
listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstanceNameMarker = Lens.lens (\ListTrafficPolicyInstancesByPolicyResponse' {trafficPolicyInstanceNameMarker} -> trafficPolicyInstanceNameMarker) (\s@ListTrafficPolicyInstancesByPolicyResponse' {} a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesByPolicyResponse)

-- | The response's http status code.
listTrafficPolicyInstancesByPolicyResponse_httpStatus :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Prelude.Int
listTrafficPolicyInstancesByPolicyResponse_httpStatus = Lens.lens (\ListTrafficPolicyInstancesByPolicyResponse' {httpStatus} -> httpStatus) (\s@ListTrafficPolicyInstancesByPolicyResponse' {} a -> s {httpStatus = a} :: ListTrafficPolicyInstancesByPolicyResponse)

-- | A list that contains one @TrafficPolicyInstance@ element for each
-- traffic policy instance that matches the elements in the request.
listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse [TrafficPolicyInstance]
listTrafficPolicyInstancesByPolicyResponse_trafficPolicyInstances = Lens.lens (\ListTrafficPolicyInstancesByPolicyResponse' {trafficPolicyInstances} -> trafficPolicyInstances) (\s@ListTrafficPolicyInstancesByPolicyResponse' {} a -> s {trafficPolicyInstances = a} :: ListTrafficPolicyInstancesByPolicyResponse) Prelude.. Lens.coerced

-- | A flag that indicates whether there are more traffic policy instances to
-- be listed. If the response was truncated, you can get the next group of
-- traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@
-- again and specifying the values of the @HostedZoneIdMarker@,
-- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
-- elements in the corresponding request parameters.
listTrafficPolicyInstancesByPolicyResponse_isTruncated :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Prelude.Bool
listTrafficPolicyInstancesByPolicyResponse_isTruncated = Lens.lens (\ListTrafficPolicyInstancesByPolicyResponse' {isTruncated} -> isTruncated) (\s@ListTrafficPolicyInstancesByPolicyResponse' {} a -> s {isTruncated = a} :: ListTrafficPolicyInstancesByPolicyResponse)

-- | The value that you specified for the @MaxItems@ parameter in the call to
-- @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
listTrafficPolicyInstancesByPolicyResponse_maxItems :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Prelude.Text
listTrafficPolicyInstancesByPolicyResponse_maxItems = Lens.lens (\ListTrafficPolicyInstancesByPolicyResponse' {maxItems} -> maxItems) (\s@ListTrafficPolicyInstancesByPolicyResponse' {} a -> s {maxItems = a} :: ListTrafficPolicyInstancesByPolicyResponse)

instance
  Prelude.NFData
    ListTrafficPolicyInstancesByPolicyResponse
  where
  rnf ListTrafficPolicyInstancesByPolicyResponse' {..} =
    Prelude.rnf hostedZoneIdMarker
      `Prelude.seq` Prelude.rnf trafficPolicyInstanceTypeMarker
      `Prelude.seq` Prelude.rnf trafficPolicyInstanceNameMarker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf trafficPolicyInstances
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf maxItems

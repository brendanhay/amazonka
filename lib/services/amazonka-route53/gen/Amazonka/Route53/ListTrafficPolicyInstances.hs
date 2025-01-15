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
-- Module      : Amazonka.Route53.ListTrafficPolicyInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by
-- using the current Amazon Web Services account.
--
-- After you submit an @UpdateTrafficPolicyInstance@ request, there\'s a
-- brief delay while Amazon Route 53 creates the resource record sets that
-- are specified in the traffic policy definition. For more information,
-- see the @State@ response element.
--
-- Route 53 returns a maximum of 100 items in each response. If you have a
-- lot of traffic policy instances, you can use the @MaxItems@ parameter to
-- list them in groups of up to 100.
module Amazonka.Route53.ListTrafficPolicyInstances
  ( -- * Creating a Request
    ListTrafficPolicyInstances (..),
    newListTrafficPolicyInstances,

    -- * Request Lenses
    listTrafficPolicyInstances_hostedZoneIdMarker,
    listTrafficPolicyInstances_maxItems,
    listTrafficPolicyInstances_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstances_trafficPolicyInstanceTypeMarker,

    -- * Destructuring the Response
    ListTrafficPolicyInstancesResponse (..),
    newListTrafficPolicyInstancesResponse,

    -- * Response Lenses
    listTrafficPolicyInstancesResponse_hostedZoneIdMarker,
    listTrafficPolicyInstancesResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesResponse_httpStatus,
    listTrafficPolicyInstancesResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesResponse_isTruncated,
    listTrafficPolicyInstancesResponse_maxItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to get information about the traffic policy instances that you
-- created by using the current Amazon Web Services account.
--
-- /See:/ 'newListTrafficPolicyInstances' smart constructor.
data ListTrafficPolicyInstances = ListTrafficPolicyInstances'
  { -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more traffic policy instances. To get more traffic policy
    -- instances, submit another @ListTrafficPolicyInstances@ request. For the
    -- value of @HostedZoneId@, specify the value of @HostedZoneIdMarker@ from
    -- the previous response, which is the hosted zone ID of the first traffic
    -- policy instance in the next group of traffic policy instances.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more traffic policy instances to get.
    hostedZoneIdMarker :: Prelude.Maybe ResourceId,
    -- | The maximum number of traffic policy instances that you want Amazon
    -- Route 53 to return in response to a @ListTrafficPolicyInstances@
    -- request. If you have more than @MaxItems@ traffic policy instances, the
    -- value of the @IsTruncated@ element in the response is @true@, and the
    -- values of @HostedZoneIdMarker@, @TrafficPolicyInstanceNameMarker@, and
    -- @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy
    -- instance in the next group of @MaxItems@ traffic policy instances.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more traffic policy instances. To get more traffic policy
    -- instances, submit another @ListTrafficPolicyInstances@ request. For the
    -- value of @trafficpolicyinstancename@, specify the value of
    -- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
    -- the name of the first traffic policy instance in the next group of
    -- traffic policy instances.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more traffic policy instances to get.
    trafficPolicyInstanceNameMarker :: Prelude.Maybe Prelude.Text,
    -- | If the value of @IsTruncated@ in the previous response was @true@, you
    -- have more traffic policy instances. To get more traffic policy
    -- instances, submit another @ListTrafficPolicyInstances@ request. For the
    -- value of @trafficpolicyinstancetype@, specify the value of
    -- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
    -- the type of the first traffic policy instance in the next group of
    -- traffic policy instances.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more traffic policy instances to get.
    trafficPolicyInstanceTypeMarker :: Prelude.Maybe RRType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicyInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneIdMarker', 'listTrafficPolicyInstances_hostedZoneIdMarker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstances@ request. For the
-- value of @HostedZoneId@, specify the value of @HostedZoneIdMarker@ from
-- the previous response, which is the hosted zone ID of the first traffic
-- policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
--
-- 'maxItems', 'listTrafficPolicyInstances_maxItems' - The maximum number of traffic policy instances that you want Amazon
-- Route 53 to return in response to a @ListTrafficPolicyInstances@
-- request. If you have more than @MaxItems@ traffic policy instances, the
-- value of the @IsTruncated@ element in the response is @true@, and the
-- values of @HostedZoneIdMarker@, @TrafficPolicyInstanceNameMarker@, and
-- @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy
-- instance in the next group of @MaxItems@ traffic policy instances.
--
-- 'trafficPolicyInstanceNameMarker', 'listTrafficPolicyInstances_trafficPolicyInstanceNameMarker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstances@ request. For the
-- value of @trafficpolicyinstancename@, specify the value of
-- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
-- the name of the first traffic policy instance in the next group of
-- traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
--
-- 'trafficPolicyInstanceTypeMarker', 'listTrafficPolicyInstances_trafficPolicyInstanceTypeMarker' - If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstances@ request. For the
-- value of @trafficpolicyinstancetype@, specify the value of
-- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
-- the type of the first traffic policy instance in the next group of
-- traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
newListTrafficPolicyInstances ::
  ListTrafficPolicyInstances
newListTrafficPolicyInstances =
  ListTrafficPolicyInstances'
    { hostedZoneIdMarker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      trafficPolicyInstanceNameMarker =
        Prelude.Nothing,
      trafficPolicyInstanceTypeMarker =
        Prelude.Nothing
    }

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstances@ request. For the
-- value of @HostedZoneId@, specify the value of @HostedZoneIdMarker@ from
-- the previous response, which is the hosted zone ID of the first traffic
-- policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
listTrafficPolicyInstances_hostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstances (Prelude.Maybe ResourceId)
listTrafficPolicyInstances_hostedZoneIdMarker = Lens.lens (\ListTrafficPolicyInstances' {hostedZoneIdMarker} -> hostedZoneIdMarker) (\s@ListTrafficPolicyInstances' {} a -> s {hostedZoneIdMarker = a} :: ListTrafficPolicyInstances)

-- | The maximum number of traffic policy instances that you want Amazon
-- Route 53 to return in response to a @ListTrafficPolicyInstances@
-- request. If you have more than @MaxItems@ traffic policy instances, the
-- value of the @IsTruncated@ element in the response is @true@, and the
-- values of @HostedZoneIdMarker@, @TrafficPolicyInstanceNameMarker@, and
-- @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy
-- instance in the next group of @MaxItems@ traffic policy instances.
listTrafficPolicyInstances_maxItems :: Lens.Lens' ListTrafficPolicyInstances (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstances_maxItems = Lens.lens (\ListTrafficPolicyInstances' {maxItems} -> maxItems) (\s@ListTrafficPolicyInstances' {} a -> s {maxItems = a} :: ListTrafficPolicyInstances)

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstances@ request. For the
-- value of @trafficpolicyinstancename@, specify the value of
-- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
-- the name of the first traffic policy instance in the next group of
-- traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
listTrafficPolicyInstances_trafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstances (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstances_trafficPolicyInstanceNameMarker = Lens.lens (\ListTrafficPolicyInstances' {trafficPolicyInstanceNameMarker} -> trafficPolicyInstanceNameMarker) (\s@ListTrafficPolicyInstances' {} a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstances)

-- | If the value of @IsTruncated@ in the previous response was @true@, you
-- have more traffic policy instances. To get more traffic policy
-- instances, submit another @ListTrafficPolicyInstances@ request. For the
-- value of @trafficpolicyinstancetype@, specify the value of
-- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
-- the type of the first traffic policy instance in the next group of
-- traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
listTrafficPolicyInstances_trafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstances (Prelude.Maybe RRType)
listTrafficPolicyInstances_trafficPolicyInstanceTypeMarker = Lens.lens (\ListTrafficPolicyInstances' {trafficPolicyInstanceTypeMarker} -> trafficPolicyInstanceTypeMarker) (\s@ListTrafficPolicyInstances' {} a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstances)

instance Core.AWSRequest ListTrafficPolicyInstances where
  type
    AWSResponse ListTrafficPolicyInstances =
      ListTrafficPolicyInstancesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListTrafficPolicyInstancesResponse'
            Prelude.<$> (x Data..@? "HostedZoneIdMarker")
            Prelude.<*> (x Data..@? "TrafficPolicyInstanceNameMarker")
            Prelude.<*> (x Data..@? "TrafficPolicyInstanceTypeMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "TrafficPolicyInstances"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "TrafficPolicyInstance"
                        )
            Prelude.<*> (x Data..@ "IsTruncated")
            Prelude.<*> (x Data..@ "MaxItems")
      )

instance Prelude.Hashable ListTrafficPolicyInstances where
  hashWithSalt _salt ListTrafficPolicyInstances' {..} =
    _salt
      `Prelude.hashWithSalt` hostedZoneIdMarker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` trafficPolicyInstanceNameMarker
      `Prelude.hashWithSalt` trafficPolicyInstanceTypeMarker

instance Prelude.NFData ListTrafficPolicyInstances where
  rnf ListTrafficPolicyInstances' {..} =
    Prelude.rnf hostedZoneIdMarker `Prelude.seq`
      Prelude.rnf maxItems `Prelude.seq`
        Prelude.rnf trafficPolicyInstanceNameMarker `Prelude.seq`
          Prelude.rnf trafficPolicyInstanceTypeMarker

instance Data.ToHeaders ListTrafficPolicyInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTrafficPolicyInstances where
  toPath =
    Prelude.const "/2013-04-01/trafficpolicyinstances"

instance Data.ToQuery ListTrafficPolicyInstances where
  toQuery ListTrafficPolicyInstances' {..} =
    Prelude.mconcat
      [ "hostedzoneid" Data.=: hostedZoneIdMarker,
        "maxitems" Data.=: maxItems,
        "trafficpolicyinstancename"
          Data.=: trafficPolicyInstanceNameMarker,
        "trafficpolicyinstancetype"
          Data.=: trafficPolicyInstanceTypeMarker
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'newListTrafficPolicyInstancesResponse' smart constructor.
data ListTrafficPolicyInstancesResponse = ListTrafficPolicyInstancesResponse'
  { -- | If @IsTruncated@ is @true@, @HostedZoneIdMarker@ is the ID of the hosted
    -- zone of the first traffic policy instance that Route 53 will return if
    -- you submit another @ListTrafficPolicyInstances@ request.
    hostedZoneIdMarker :: Prelude.Maybe ResourceId,
    -- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
    -- name of the first traffic policy instance that Route 53 will return if
    -- you submit another @ListTrafficPolicyInstances@ request.
    trafficPolicyInstanceNameMarker :: Prelude.Maybe Prelude.Text,
    -- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceTypeMarker@ is the DNS
    -- type of the resource record sets that are associated with the first
    -- traffic policy instance that Amazon Route 53 will return if you submit
    -- another @ListTrafficPolicyInstances@ request.
    trafficPolicyInstanceTypeMarker :: Prelude.Maybe RRType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains one @TrafficPolicyInstance@ element for each
    -- traffic policy instance that matches the elements in the request.
    trafficPolicyInstances :: [TrafficPolicyInstance],
    -- | A flag that indicates whether there are more traffic policy instances to
    -- be listed. If the response was truncated, you can get more traffic
    -- policy instances by calling @ListTrafficPolicyInstances@ again and
    -- specifying the values of the @HostedZoneIdMarker@,
    -- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
    -- in the corresponding request parameters.
    isTruncated :: Prelude.Bool,
    -- | The value that you specified for the @MaxItems@ parameter in the call to
    -- @ListTrafficPolicyInstances@ that produced the current response.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicyInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneIdMarker', 'listTrafficPolicyInstancesResponse_hostedZoneIdMarker' - If @IsTruncated@ is @true@, @HostedZoneIdMarker@ is the ID of the hosted
-- zone of the first traffic policy instance that Route 53 will return if
-- you submit another @ListTrafficPolicyInstances@ request.
--
-- 'trafficPolicyInstanceNameMarker', 'listTrafficPolicyInstancesResponse_trafficPolicyInstanceNameMarker' - If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
-- name of the first traffic policy instance that Route 53 will return if
-- you submit another @ListTrafficPolicyInstances@ request.
--
-- 'trafficPolicyInstanceTypeMarker', 'listTrafficPolicyInstancesResponse_trafficPolicyInstanceTypeMarker' - If @IsTruncated@ is @true@, @TrafficPolicyInstanceTypeMarker@ is the DNS
-- type of the resource record sets that are associated with the first
-- traffic policy instance that Amazon Route 53 will return if you submit
-- another @ListTrafficPolicyInstances@ request.
--
-- 'httpStatus', 'listTrafficPolicyInstancesResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicyInstances', 'listTrafficPolicyInstancesResponse_trafficPolicyInstances' - A list that contains one @TrafficPolicyInstance@ element for each
-- traffic policy instance that matches the elements in the request.
--
-- 'isTruncated', 'listTrafficPolicyInstancesResponse_isTruncated' - A flag that indicates whether there are more traffic policy instances to
-- be listed. If the response was truncated, you can get more traffic
-- policy instances by calling @ListTrafficPolicyInstances@ again and
-- specifying the values of the @HostedZoneIdMarker@,
-- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
-- in the corresponding request parameters.
--
-- 'maxItems', 'listTrafficPolicyInstancesResponse_maxItems' - The value that you specified for the @MaxItems@ parameter in the call to
-- @ListTrafficPolicyInstances@ that produced the current response.
newListTrafficPolicyInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'maxItems'
  Prelude.Text ->
  ListTrafficPolicyInstancesResponse
newListTrafficPolicyInstancesResponse
  pHttpStatus_
  pIsTruncated_
  pMaxItems_ =
    ListTrafficPolicyInstancesResponse'
      { hostedZoneIdMarker =
          Prelude.Nothing,
        trafficPolicyInstanceNameMarker =
          Prelude.Nothing,
        trafficPolicyInstanceTypeMarker =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        trafficPolicyInstances = Prelude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, @HostedZoneIdMarker@ is the ID of the hosted
-- zone of the first traffic policy instance that Route 53 will return if
-- you submit another @ListTrafficPolicyInstances@ request.
listTrafficPolicyInstancesResponse_hostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Prelude.Maybe ResourceId)
listTrafficPolicyInstancesResponse_hostedZoneIdMarker = Lens.lens (\ListTrafficPolicyInstancesResponse' {hostedZoneIdMarker} -> hostedZoneIdMarker) (\s@ListTrafficPolicyInstancesResponse' {} a -> s {hostedZoneIdMarker = a} :: ListTrafficPolicyInstancesResponse)

-- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
-- name of the first traffic policy instance that Route 53 will return if
-- you submit another @ListTrafficPolicyInstances@ request.
listTrafficPolicyInstancesResponse_trafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstancesResponse_trafficPolicyInstanceNameMarker = Lens.lens (\ListTrafficPolicyInstancesResponse' {trafficPolicyInstanceNameMarker} -> trafficPolicyInstanceNameMarker) (\s@ListTrafficPolicyInstancesResponse' {} a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesResponse)

-- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceTypeMarker@ is the DNS
-- type of the resource record sets that are associated with the first
-- traffic policy instance that Amazon Route 53 will return if you submit
-- another @ListTrafficPolicyInstances@ request.
listTrafficPolicyInstancesResponse_trafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Prelude.Maybe RRType)
listTrafficPolicyInstancesResponse_trafficPolicyInstanceTypeMarker = Lens.lens (\ListTrafficPolicyInstancesResponse' {trafficPolicyInstanceTypeMarker} -> trafficPolicyInstanceTypeMarker) (\s@ListTrafficPolicyInstancesResponse' {} a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesResponse)

-- | The response's http status code.
listTrafficPolicyInstancesResponse_httpStatus :: Lens.Lens' ListTrafficPolicyInstancesResponse Prelude.Int
listTrafficPolicyInstancesResponse_httpStatus = Lens.lens (\ListTrafficPolicyInstancesResponse' {httpStatus} -> httpStatus) (\s@ListTrafficPolicyInstancesResponse' {} a -> s {httpStatus = a} :: ListTrafficPolicyInstancesResponse)

-- | A list that contains one @TrafficPolicyInstance@ element for each
-- traffic policy instance that matches the elements in the request.
listTrafficPolicyInstancesResponse_trafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesResponse [TrafficPolicyInstance]
listTrafficPolicyInstancesResponse_trafficPolicyInstances = Lens.lens (\ListTrafficPolicyInstancesResponse' {trafficPolicyInstances} -> trafficPolicyInstances) (\s@ListTrafficPolicyInstancesResponse' {} a -> s {trafficPolicyInstances = a} :: ListTrafficPolicyInstancesResponse) Prelude.. Lens.coerced

-- | A flag that indicates whether there are more traffic policy instances to
-- be listed. If the response was truncated, you can get more traffic
-- policy instances by calling @ListTrafficPolicyInstances@ again and
-- specifying the values of the @HostedZoneIdMarker@,
-- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
-- in the corresponding request parameters.
listTrafficPolicyInstancesResponse_isTruncated :: Lens.Lens' ListTrafficPolicyInstancesResponse Prelude.Bool
listTrafficPolicyInstancesResponse_isTruncated = Lens.lens (\ListTrafficPolicyInstancesResponse' {isTruncated} -> isTruncated) (\s@ListTrafficPolicyInstancesResponse' {} a -> s {isTruncated = a} :: ListTrafficPolicyInstancesResponse)

-- | The value that you specified for the @MaxItems@ parameter in the call to
-- @ListTrafficPolicyInstances@ that produced the current response.
listTrafficPolicyInstancesResponse_maxItems :: Lens.Lens' ListTrafficPolicyInstancesResponse Prelude.Text
listTrafficPolicyInstancesResponse_maxItems = Lens.lens (\ListTrafficPolicyInstancesResponse' {maxItems} -> maxItems) (\s@ListTrafficPolicyInstancesResponse' {} a -> s {maxItems = a} :: ListTrafficPolicyInstancesResponse)

instance
  Prelude.NFData
    ListTrafficPolicyInstancesResponse
  where
  rnf ListTrafficPolicyInstancesResponse' {..} =
    Prelude.rnf hostedZoneIdMarker `Prelude.seq`
      Prelude.rnf trafficPolicyInstanceNameMarker `Prelude.seq`
        Prelude.rnf trafficPolicyInstanceTypeMarker `Prelude.seq`
          Prelude.rnf httpStatus `Prelude.seq`
            Prelude.rnf trafficPolicyInstances `Prelude.seq`
              Prelude.rnf isTruncated `Prelude.seq`
                Prelude.rnf maxItems

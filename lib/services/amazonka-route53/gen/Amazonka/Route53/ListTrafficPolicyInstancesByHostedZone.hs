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
-- Module      : Amazonka.Route53.ListTrafficPolicyInstancesByHostedZone
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created in
-- a specified hosted zone.
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
module Amazonka.Route53.ListTrafficPolicyInstancesByHostedZone
  ( -- * Creating a Request
    ListTrafficPolicyInstancesByHostedZone (..),
    newListTrafficPolicyInstancesByHostedZone,

    -- * Request Lenses
    listTrafficPolicyInstancesByHostedZone_maxItems,
    listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByHostedZone_hostedZoneId,

    -- * Destructuring the Response
    ListTrafficPolicyInstancesByHostedZoneResponse (..),
    newListTrafficPolicyInstancesByHostedZoneResponse,

    -- * Response Lenses
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceNameMarker,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceTypeMarker,
    listTrafficPolicyInstancesByHostedZoneResponse_httpStatus,
    listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstances,
    listTrafficPolicyInstancesByHostedZoneResponse_isTruncated,
    listTrafficPolicyInstancesByHostedZoneResponse_maxItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request for the traffic policy instances that you created in a
-- specified hosted zone.
--
-- /See:/ 'newListTrafficPolicyInstancesByHostedZone' smart constructor.
data ListTrafficPolicyInstancesByHostedZone = ListTrafficPolicyInstancesByHostedZone'
  { -- | The maximum number of traffic policy instances to be included in the
    -- response body for this request. If you have more than @MaxItems@ traffic
    -- policy instances, the value of the @IsTruncated@ element in the response
    -- is @true@, and the values of @HostedZoneIdMarker@,
    -- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
    -- represent the first traffic policy instance that Amazon Route 53 will
    -- return if you submit another request.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | If the value of @IsTruncated@ in the previous response is true, you have
    -- more traffic policy instances. To get more traffic policy instances,
    -- submit another @ListTrafficPolicyInstances@ request. For the value of
    -- @trafficpolicyinstancename@, specify the value of
    -- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
    -- the name of the first traffic policy instance in the next group of
    -- traffic policy instances.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more traffic policy instances to get.
    trafficPolicyInstanceNameMarker :: Prelude.Maybe Prelude.Text,
    -- | If the value of @IsTruncated@ in the previous response is true, you have
    -- more traffic policy instances. To get more traffic policy instances,
    -- submit another @ListTrafficPolicyInstances@ request. For the value of
    -- @trafficpolicyinstancetype@, specify the value of
    -- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
    -- the type of the first traffic policy instance in the next group of
    -- traffic policy instances.
    --
    -- If the value of @IsTruncated@ in the previous response was @false@,
    -- there are no more traffic policy instances to get.
    trafficPolicyInstanceTypeMarker :: Prelude.Maybe RRType,
    -- | The ID of the hosted zone that you want to list traffic policy instances
    -- for.
    hostedZoneId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicyInstancesByHostedZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listTrafficPolicyInstancesByHostedZone_maxItems' - The maximum number of traffic policy instances to be included in the
-- response body for this request. If you have more than @MaxItems@ traffic
-- policy instances, the value of the @IsTruncated@ element in the response
-- is @true@, and the values of @HostedZoneIdMarker@,
-- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
-- represent the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
--
-- 'trafficPolicyInstanceNameMarker', 'listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceNameMarker' - If the value of @IsTruncated@ in the previous response is true, you have
-- more traffic policy instances. To get more traffic policy instances,
-- submit another @ListTrafficPolicyInstances@ request. For the value of
-- @trafficpolicyinstancename@, specify the value of
-- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
-- the name of the first traffic policy instance in the next group of
-- traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
--
-- 'trafficPolicyInstanceTypeMarker', 'listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceTypeMarker' - If the value of @IsTruncated@ in the previous response is true, you have
-- more traffic policy instances. To get more traffic policy instances,
-- submit another @ListTrafficPolicyInstances@ request. For the value of
-- @trafficpolicyinstancetype@, specify the value of
-- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
-- the type of the first traffic policy instance in the next group of
-- traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
--
-- 'hostedZoneId', 'listTrafficPolicyInstancesByHostedZone_hostedZoneId' - The ID of the hosted zone that you want to list traffic policy instances
-- for.
newListTrafficPolicyInstancesByHostedZone ::
  -- | 'hostedZoneId'
  ResourceId ->
  ListTrafficPolicyInstancesByHostedZone
newListTrafficPolicyInstancesByHostedZone
  pHostedZoneId_ =
    ListTrafficPolicyInstancesByHostedZone'
      { maxItems =
          Prelude.Nothing,
        trafficPolicyInstanceNameMarker =
          Prelude.Nothing,
        trafficPolicyInstanceTypeMarker =
          Prelude.Nothing,
        hostedZoneId = pHostedZoneId_
      }

-- | The maximum number of traffic policy instances to be included in the
-- response body for this request. If you have more than @MaxItems@ traffic
-- policy instances, the value of the @IsTruncated@ element in the response
-- is @true@, and the values of @HostedZoneIdMarker@,
-- @TrafficPolicyInstanceNameMarker@, and @TrafficPolicyInstanceTypeMarker@
-- represent the first traffic policy instance that Amazon Route 53 will
-- return if you submit another request.
listTrafficPolicyInstancesByHostedZone_maxItems :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstancesByHostedZone_maxItems = Lens.lens (\ListTrafficPolicyInstancesByHostedZone' {maxItems} -> maxItems) (\s@ListTrafficPolicyInstancesByHostedZone' {} a -> s {maxItems = a} :: ListTrafficPolicyInstancesByHostedZone)

-- | If the value of @IsTruncated@ in the previous response is true, you have
-- more traffic policy instances. To get more traffic policy instances,
-- submit another @ListTrafficPolicyInstances@ request. For the value of
-- @trafficpolicyinstancename@, specify the value of
-- @TrafficPolicyInstanceNameMarker@ from the previous response, which is
-- the name of the first traffic policy instance in the next group of
-- traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceNameMarker = Lens.lens (\ListTrafficPolicyInstancesByHostedZone' {trafficPolicyInstanceNameMarker} -> trafficPolicyInstanceNameMarker) (\s@ListTrafficPolicyInstancesByHostedZone' {} a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesByHostedZone)

-- | If the value of @IsTruncated@ in the previous response is true, you have
-- more traffic policy instances. To get more traffic policy instances,
-- submit another @ListTrafficPolicyInstances@ request. For the value of
-- @trafficpolicyinstancetype@, specify the value of
-- @TrafficPolicyInstanceTypeMarker@ from the previous response, which is
-- the type of the first traffic policy instance in the next group of
-- traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@,
-- there are no more traffic policy instances to get.
listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Prelude.Maybe RRType)
listTrafficPolicyInstancesByHostedZone_trafficPolicyInstanceTypeMarker = Lens.lens (\ListTrafficPolicyInstancesByHostedZone' {trafficPolicyInstanceTypeMarker} -> trafficPolicyInstanceTypeMarker) (\s@ListTrafficPolicyInstancesByHostedZone' {} a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesByHostedZone)

-- | The ID of the hosted zone that you want to list traffic policy instances
-- for.
listTrafficPolicyInstancesByHostedZone_hostedZoneId :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone ResourceId
listTrafficPolicyInstancesByHostedZone_hostedZoneId = Lens.lens (\ListTrafficPolicyInstancesByHostedZone' {hostedZoneId} -> hostedZoneId) (\s@ListTrafficPolicyInstancesByHostedZone' {} a -> s {hostedZoneId = a} :: ListTrafficPolicyInstancesByHostedZone)

instance
  Core.AWSRequest
    ListTrafficPolicyInstancesByHostedZone
  where
  type
    AWSResponse
      ListTrafficPolicyInstancesByHostedZone =
      ListTrafficPolicyInstancesByHostedZoneResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListTrafficPolicyInstancesByHostedZoneResponse'
            Prelude.<$> (x Data..@? "TrafficPolicyInstanceNameMarker")
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

instance
  Prelude.Hashable
    ListTrafficPolicyInstancesByHostedZone
  where
  hashWithSalt
    _salt
    ListTrafficPolicyInstancesByHostedZone' {..} =
      _salt
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` trafficPolicyInstanceNameMarker
        `Prelude.hashWithSalt` trafficPolicyInstanceTypeMarker
        `Prelude.hashWithSalt` hostedZoneId

instance
  Prelude.NFData
    ListTrafficPolicyInstancesByHostedZone
  where
  rnf ListTrafficPolicyInstancesByHostedZone' {..} =
    Prelude.rnf maxItems `Prelude.seq`
      Prelude.rnf trafficPolicyInstanceNameMarker `Prelude.seq`
        Prelude.rnf trafficPolicyInstanceTypeMarker `Prelude.seq`
          Prelude.rnf hostedZoneId

instance
  Data.ToHeaders
    ListTrafficPolicyInstancesByHostedZone
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListTrafficPolicyInstancesByHostedZone
  where
  toPath =
    Prelude.const
      "/2013-04-01/trafficpolicyinstances/hostedzone"

instance
  Data.ToQuery
    ListTrafficPolicyInstancesByHostedZone
  where
  toQuery ListTrafficPolicyInstancesByHostedZone' {..} =
    Prelude.mconcat
      [ "maxitems" Data.=: maxItems,
        "trafficpolicyinstancename"
          Data.=: trafficPolicyInstanceNameMarker,
        "trafficpolicyinstancetype"
          Data.=: trafficPolicyInstanceTypeMarker,
        "id" Data.=: hostedZoneId
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'newListTrafficPolicyInstancesByHostedZoneResponse' smart constructor.
data ListTrafficPolicyInstancesByHostedZoneResponse = ListTrafficPolicyInstancesByHostedZoneResponse'
  { -- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
    -- name of the first traffic policy instance in the next group of traffic
    -- policy instances.
    trafficPolicyInstanceNameMarker :: Prelude.Maybe Prelude.Text,
    -- | If @IsTruncated@ is true, @TrafficPolicyInstanceTypeMarker@ is the DNS
    -- type of the resource record sets that are associated with the first
    -- traffic policy instance in the next group of traffic policy instances.
    trafficPolicyInstanceTypeMarker :: Prelude.Maybe RRType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list that contains one @TrafficPolicyInstance@ element for each
    -- traffic policy instance that matches the elements in the request.
    trafficPolicyInstances :: [TrafficPolicyInstance],
    -- | A flag that indicates whether there are more traffic policy instances to
    -- be listed. If the response was truncated, you can get the next group of
    -- traffic policy instances by submitting another
    -- @ListTrafficPolicyInstancesByHostedZone@ request and specifying the
    -- values of @HostedZoneIdMarker@, @TrafficPolicyInstanceNameMarker@, and
    -- @TrafficPolicyInstanceTypeMarker@ in the corresponding request
    -- parameters.
    isTruncated :: Prelude.Bool,
    -- | The value that you specified for the @MaxItems@ parameter in the
    -- @ListTrafficPolicyInstancesByHostedZone@ request that produced the
    -- current response.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrafficPolicyInstancesByHostedZoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficPolicyInstanceNameMarker', 'listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceNameMarker' - If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
-- name of the first traffic policy instance in the next group of traffic
-- policy instances.
--
-- 'trafficPolicyInstanceTypeMarker', 'listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceTypeMarker' - If @IsTruncated@ is true, @TrafficPolicyInstanceTypeMarker@ is the DNS
-- type of the resource record sets that are associated with the first
-- traffic policy instance in the next group of traffic policy instances.
--
-- 'httpStatus', 'listTrafficPolicyInstancesByHostedZoneResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicyInstances', 'listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstances' - A list that contains one @TrafficPolicyInstance@ element for each
-- traffic policy instance that matches the elements in the request.
--
-- 'isTruncated', 'listTrafficPolicyInstancesByHostedZoneResponse_isTruncated' - A flag that indicates whether there are more traffic policy instances to
-- be listed. If the response was truncated, you can get the next group of
-- traffic policy instances by submitting another
-- @ListTrafficPolicyInstancesByHostedZone@ request and specifying the
-- values of @HostedZoneIdMarker@, @TrafficPolicyInstanceNameMarker@, and
-- @TrafficPolicyInstanceTypeMarker@ in the corresponding request
-- parameters.
--
-- 'maxItems', 'listTrafficPolicyInstancesByHostedZoneResponse_maxItems' - The value that you specified for the @MaxItems@ parameter in the
-- @ListTrafficPolicyInstancesByHostedZone@ request that produced the
-- current response.
newListTrafficPolicyInstancesByHostedZoneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'maxItems'
  Prelude.Text ->
  ListTrafficPolicyInstancesByHostedZoneResponse
newListTrafficPolicyInstancesByHostedZoneResponse
  pHttpStatus_
  pIsTruncated_
  pMaxItems_ =
    ListTrafficPolicyInstancesByHostedZoneResponse'
      { trafficPolicyInstanceNameMarker =
          Prelude.Nothing,
        trafficPolicyInstanceTypeMarker =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        trafficPolicyInstances =
          Prelude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@, @TrafficPolicyInstanceNameMarker@ is the
-- name of the first traffic policy instance in the next group of traffic
-- policy instances.
listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse (Prelude.Maybe Prelude.Text)
listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceNameMarker = Lens.lens (\ListTrafficPolicyInstancesByHostedZoneResponse' {trafficPolicyInstanceNameMarker} -> trafficPolicyInstanceNameMarker) (\s@ListTrafficPolicyInstancesByHostedZoneResponse' {} a -> s {trafficPolicyInstanceNameMarker = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)

-- | If @IsTruncated@ is true, @TrafficPolicyInstanceTypeMarker@ is the DNS
-- type of the resource record sets that are associated with the first
-- traffic policy instance in the next group of traffic policy instances.
listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse (Prelude.Maybe RRType)
listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstanceTypeMarker = Lens.lens (\ListTrafficPolicyInstancesByHostedZoneResponse' {trafficPolicyInstanceTypeMarker} -> trafficPolicyInstanceTypeMarker) (\s@ListTrafficPolicyInstancesByHostedZoneResponse' {} a -> s {trafficPolicyInstanceTypeMarker = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)

-- | The response's http status code.
listTrafficPolicyInstancesByHostedZoneResponse_httpStatus :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Prelude.Int
listTrafficPolicyInstancesByHostedZoneResponse_httpStatus = Lens.lens (\ListTrafficPolicyInstancesByHostedZoneResponse' {httpStatus} -> httpStatus) (\s@ListTrafficPolicyInstancesByHostedZoneResponse' {} a -> s {httpStatus = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)

-- | A list that contains one @TrafficPolicyInstance@ element for each
-- traffic policy instance that matches the elements in the request.
listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse [TrafficPolicyInstance]
listTrafficPolicyInstancesByHostedZoneResponse_trafficPolicyInstances = Lens.lens (\ListTrafficPolicyInstancesByHostedZoneResponse' {trafficPolicyInstances} -> trafficPolicyInstances) (\s@ListTrafficPolicyInstancesByHostedZoneResponse' {} a -> s {trafficPolicyInstances = a} :: ListTrafficPolicyInstancesByHostedZoneResponse) Prelude.. Lens.coerced

-- | A flag that indicates whether there are more traffic policy instances to
-- be listed. If the response was truncated, you can get the next group of
-- traffic policy instances by submitting another
-- @ListTrafficPolicyInstancesByHostedZone@ request and specifying the
-- values of @HostedZoneIdMarker@, @TrafficPolicyInstanceNameMarker@, and
-- @TrafficPolicyInstanceTypeMarker@ in the corresponding request
-- parameters.
listTrafficPolicyInstancesByHostedZoneResponse_isTruncated :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Prelude.Bool
listTrafficPolicyInstancesByHostedZoneResponse_isTruncated = Lens.lens (\ListTrafficPolicyInstancesByHostedZoneResponse' {isTruncated} -> isTruncated) (\s@ListTrafficPolicyInstancesByHostedZoneResponse' {} a -> s {isTruncated = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)

-- | The value that you specified for the @MaxItems@ parameter in the
-- @ListTrafficPolicyInstancesByHostedZone@ request that produced the
-- current response.
listTrafficPolicyInstancesByHostedZoneResponse_maxItems :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Prelude.Text
listTrafficPolicyInstancesByHostedZoneResponse_maxItems = Lens.lens (\ListTrafficPolicyInstancesByHostedZoneResponse' {maxItems} -> maxItems) (\s@ListTrafficPolicyInstancesByHostedZoneResponse' {} a -> s {maxItems = a} :: ListTrafficPolicyInstancesByHostedZoneResponse)

instance
  Prelude.NFData
    ListTrafficPolicyInstancesByHostedZoneResponse
  where
  rnf
    ListTrafficPolicyInstancesByHostedZoneResponse' {..} =
      Prelude.rnf trafficPolicyInstanceNameMarker `Prelude.seq`
        Prelude.rnf trafficPolicyInstanceTypeMarker `Prelude.seq`
          Prelude.rnf httpStatus `Prelude.seq`
            Prelude.rnf trafficPolicyInstances `Prelude.seq`
              Prelude.rnf isTruncated `Prelude.seq`
                Prelude.rnf maxItems

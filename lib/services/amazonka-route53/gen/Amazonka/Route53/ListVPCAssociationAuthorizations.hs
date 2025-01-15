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
-- Module      : Amazonka.Route53.ListVPCAssociationAuthorizations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the VPCs that were created by other accounts and that can
-- be associated with a specified hosted zone because you\'ve submitted one
-- or more @CreateVPCAssociationAuthorization@ requests.
--
-- The response includes a @VPCs@ element with a @VPC@ child element for
-- each VPC that can be associated with the hosted zone.
--
-- This operation returns paginated results.
module Amazonka.Route53.ListVPCAssociationAuthorizations
  ( -- * Creating a Request
    ListVPCAssociationAuthorizations (..),
    newListVPCAssociationAuthorizations,

    -- * Request Lenses
    listVPCAssociationAuthorizations_maxResults,
    listVPCAssociationAuthorizations_nextToken,
    listVPCAssociationAuthorizations_hostedZoneId,

    -- * Destructuring the Response
    ListVPCAssociationAuthorizationsResponse (..),
    newListVPCAssociationAuthorizationsResponse,

    -- * Response Lenses
    listVPCAssociationAuthorizationsResponse_nextToken,
    listVPCAssociationAuthorizationsResponse_httpStatus,
    listVPCAssociationAuthorizationsResponse_hostedZoneId,
    listVPCAssociationAuthorizationsResponse_vPCs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about that can be associated
-- with your hosted zone.
--
-- /See:/ 'newListVPCAssociationAuthorizations' smart constructor.
data ListVPCAssociationAuthorizations = ListVPCAssociationAuthorizations'
  { -- | /Optional/: An integer that specifies the maximum number of VPCs that
    -- you want Amazon Route 53 to return. If you don\'t specify a value for
    -- @MaxResults@, Route 53 returns up to 50 VPCs per page.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | /Optional/: If a response includes a @NextToken@ element, there are more
    -- VPCs that can be associated with the specified hosted zone. To get the
    -- next page of results, submit another request, and include the value of
    -- @NextToken@ from the response in the @nexttoken@ parameter in another
    -- @ListVPCAssociationAuthorizations@ request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the hosted zone for which you want a list of VPCs that can be
    -- associated with the hosted zone.
    hostedZoneId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVPCAssociationAuthorizations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVPCAssociationAuthorizations_maxResults' - /Optional/: An integer that specifies the maximum number of VPCs that
-- you want Amazon Route 53 to return. If you don\'t specify a value for
-- @MaxResults@, Route 53 returns up to 50 VPCs per page.
--
-- 'nextToken', 'listVPCAssociationAuthorizations_nextToken' - /Optional/: If a response includes a @NextToken@ element, there are more
-- VPCs that can be associated with the specified hosted zone. To get the
-- next page of results, submit another request, and include the value of
-- @NextToken@ from the response in the @nexttoken@ parameter in another
-- @ListVPCAssociationAuthorizations@ request.
--
-- 'hostedZoneId', 'listVPCAssociationAuthorizations_hostedZoneId' - The ID of the hosted zone for which you want a list of VPCs that can be
-- associated with the hosted zone.
newListVPCAssociationAuthorizations ::
  -- | 'hostedZoneId'
  ResourceId ->
  ListVPCAssociationAuthorizations
newListVPCAssociationAuthorizations pHostedZoneId_ =
  ListVPCAssociationAuthorizations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      hostedZoneId = pHostedZoneId_
    }

-- | /Optional/: An integer that specifies the maximum number of VPCs that
-- you want Amazon Route 53 to return. If you don\'t specify a value for
-- @MaxResults@, Route 53 returns up to 50 VPCs per page.
listVPCAssociationAuthorizations_maxResults :: Lens.Lens' ListVPCAssociationAuthorizations (Prelude.Maybe Prelude.Text)
listVPCAssociationAuthorizations_maxResults = Lens.lens (\ListVPCAssociationAuthorizations' {maxResults} -> maxResults) (\s@ListVPCAssociationAuthorizations' {} a -> s {maxResults = a} :: ListVPCAssociationAuthorizations)

-- | /Optional/: If a response includes a @NextToken@ element, there are more
-- VPCs that can be associated with the specified hosted zone. To get the
-- next page of results, submit another request, and include the value of
-- @NextToken@ from the response in the @nexttoken@ parameter in another
-- @ListVPCAssociationAuthorizations@ request.
listVPCAssociationAuthorizations_nextToken :: Lens.Lens' ListVPCAssociationAuthorizations (Prelude.Maybe Prelude.Text)
listVPCAssociationAuthorizations_nextToken = Lens.lens (\ListVPCAssociationAuthorizations' {nextToken} -> nextToken) (\s@ListVPCAssociationAuthorizations' {} a -> s {nextToken = a} :: ListVPCAssociationAuthorizations)

-- | The ID of the hosted zone for which you want a list of VPCs that can be
-- associated with the hosted zone.
listVPCAssociationAuthorizations_hostedZoneId :: Lens.Lens' ListVPCAssociationAuthorizations ResourceId
listVPCAssociationAuthorizations_hostedZoneId = Lens.lens (\ListVPCAssociationAuthorizations' {hostedZoneId} -> hostedZoneId) (\s@ListVPCAssociationAuthorizations' {} a -> s {hostedZoneId = a} :: ListVPCAssociationAuthorizations)

instance
  Core.AWSPager
    ListVPCAssociationAuthorizations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVPCAssociationAuthorizationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listVPCAssociationAuthorizationsResponse_vPCs
            Prelude.. Lens.to Prelude.toList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listVPCAssociationAuthorizations_nextToken
              Lens..~ rs
              Lens.^? listVPCAssociationAuthorizationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListVPCAssociationAuthorizations
  where
  type
    AWSResponse ListVPCAssociationAuthorizations =
      ListVPCAssociationAuthorizationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListVPCAssociationAuthorizationsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "HostedZoneId")
            Prelude.<*> ( x Data..@? "VPCs" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList1 "VPC"
                        )
      )

instance
  Prelude.Hashable
    ListVPCAssociationAuthorizations
  where
  hashWithSalt
    _salt
    ListVPCAssociationAuthorizations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` hostedZoneId

instance
  Prelude.NFData
    ListVPCAssociationAuthorizations
  where
  rnf ListVPCAssociationAuthorizations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf hostedZoneId

instance
  Data.ToHeaders
    ListVPCAssociationAuthorizations
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVPCAssociationAuthorizations where
  toPath ListVPCAssociationAuthorizations' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/authorizevpcassociation"
      ]

instance
  Data.ToQuery
    ListVPCAssociationAuthorizations
  where
  toQuery ListVPCAssociationAuthorizations' {..} =
    Prelude.mconcat
      [ "maxresults" Data.=: maxResults,
        "nexttoken" Data.=: nextToken
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'newListVPCAssociationAuthorizationsResponse' smart constructor.
data ListVPCAssociationAuthorizationsResponse = ListVPCAssociationAuthorizationsResponse'
  { -- | When the response includes a @NextToken@ element, there are more VPCs
    -- that can be associated with the specified hosted zone. To get the next
    -- page of VPCs, submit another @ListVPCAssociationAuthorizations@ request,
    -- and include the value of the @NextToken@ element from the response in
    -- the @nexttoken@ request parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the hosted zone that you can associate the listed VPCs with.
    hostedZoneId :: ResourceId,
    -- | The list of VPCs that are authorized to be associated with the specified
    -- hosted zone.
    vPCs :: Prelude.NonEmpty VPC
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVPCAssociationAuthorizationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVPCAssociationAuthorizationsResponse_nextToken' - When the response includes a @NextToken@ element, there are more VPCs
-- that can be associated with the specified hosted zone. To get the next
-- page of VPCs, submit another @ListVPCAssociationAuthorizations@ request,
-- and include the value of the @NextToken@ element from the response in
-- the @nexttoken@ request parameter.
--
-- 'httpStatus', 'listVPCAssociationAuthorizationsResponse_httpStatus' - The response's http status code.
--
-- 'hostedZoneId', 'listVPCAssociationAuthorizationsResponse_hostedZoneId' - The ID of the hosted zone that you can associate the listed VPCs with.
--
-- 'vPCs', 'listVPCAssociationAuthorizationsResponse_vPCs' - The list of VPCs that are authorized to be associated with the specified
-- hosted zone.
newListVPCAssociationAuthorizationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vPCs'
  Prelude.NonEmpty VPC ->
  ListVPCAssociationAuthorizationsResponse
newListVPCAssociationAuthorizationsResponse
  pHttpStatus_
  pHostedZoneId_
  pVPCs_ =
    ListVPCAssociationAuthorizationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        hostedZoneId = pHostedZoneId_,
        vPCs = Lens.coerced Lens.# pVPCs_
      }

-- | When the response includes a @NextToken@ element, there are more VPCs
-- that can be associated with the specified hosted zone. To get the next
-- page of VPCs, submit another @ListVPCAssociationAuthorizations@ request,
-- and include the value of the @NextToken@ element from the response in
-- the @nexttoken@ request parameter.
listVPCAssociationAuthorizationsResponse_nextToken :: Lens.Lens' ListVPCAssociationAuthorizationsResponse (Prelude.Maybe Prelude.Text)
listVPCAssociationAuthorizationsResponse_nextToken = Lens.lens (\ListVPCAssociationAuthorizationsResponse' {nextToken} -> nextToken) (\s@ListVPCAssociationAuthorizationsResponse' {} a -> s {nextToken = a} :: ListVPCAssociationAuthorizationsResponse)

-- | The response's http status code.
listVPCAssociationAuthorizationsResponse_httpStatus :: Lens.Lens' ListVPCAssociationAuthorizationsResponse Prelude.Int
listVPCAssociationAuthorizationsResponse_httpStatus = Lens.lens (\ListVPCAssociationAuthorizationsResponse' {httpStatus} -> httpStatus) (\s@ListVPCAssociationAuthorizationsResponse' {} a -> s {httpStatus = a} :: ListVPCAssociationAuthorizationsResponse)

-- | The ID of the hosted zone that you can associate the listed VPCs with.
listVPCAssociationAuthorizationsResponse_hostedZoneId :: Lens.Lens' ListVPCAssociationAuthorizationsResponse ResourceId
listVPCAssociationAuthorizationsResponse_hostedZoneId = Lens.lens (\ListVPCAssociationAuthorizationsResponse' {hostedZoneId} -> hostedZoneId) (\s@ListVPCAssociationAuthorizationsResponse' {} a -> s {hostedZoneId = a} :: ListVPCAssociationAuthorizationsResponse)

-- | The list of VPCs that are authorized to be associated with the specified
-- hosted zone.
listVPCAssociationAuthorizationsResponse_vPCs :: Lens.Lens' ListVPCAssociationAuthorizationsResponse (Prelude.NonEmpty VPC)
listVPCAssociationAuthorizationsResponse_vPCs = Lens.lens (\ListVPCAssociationAuthorizationsResponse' {vPCs} -> vPCs) (\s@ListVPCAssociationAuthorizationsResponse' {} a -> s {vPCs = a} :: ListVPCAssociationAuthorizationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListVPCAssociationAuthorizationsResponse
  where
  rnf ListVPCAssociationAuthorizationsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf hostedZoneId `Prelude.seq`
          Prelude.rnf vPCs

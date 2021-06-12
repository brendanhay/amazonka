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
-- Module      : Network.AWS.Route53.ListVPCAssociationAuthorizations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.Route53.ListVPCAssociationAuthorizations
  ( -- * Creating a Request
    ListVPCAssociationAuthorizations (..),
    newListVPCAssociationAuthorizations,

    -- * Request Lenses
    listVPCAssociationAuthorizations_nextToken,
    listVPCAssociationAuthorizations_maxResults,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A complex type that contains information about that can be associated
-- with your hosted zone.
--
-- /See:/ 'newListVPCAssociationAuthorizations' smart constructor.
data ListVPCAssociationAuthorizations = ListVPCAssociationAuthorizations'
  { -- | /Optional/: If a response includes a @NextToken@ element, there are more
    -- VPCs that can be associated with the specified hosted zone. To get the
    -- next page of results, submit another request, and include the value of
    -- @NextToken@ from the response in the @nexttoken@ parameter in another
    -- @ListVPCAssociationAuthorizations@ request.
    nextToken :: Core.Maybe Core.Text,
    -- | /Optional/: An integer that specifies the maximum number of VPCs that
    -- you want Amazon Route 53 to return. If you don\'t specify a value for
    -- @MaxResults@, Route 53 returns up to 50 VPCs per page.
    maxResults :: Core.Maybe Core.Text,
    -- | The ID of the hosted zone for which you want a list of VPCs that can be
    -- associated with the hosted zone.
    hostedZoneId :: ResourceId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListVPCAssociationAuthorizations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVPCAssociationAuthorizations_nextToken' - /Optional/: If a response includes a @NextToken@ element, there are more
-- VPCs that can be associated with the specified hosted zone. To get the
-- next page of results, submit another request, and include the value of
-- @NextToken@ from the response in the @nexttoken@ parameter in another
-- @ListVPCAssociationAuthorizations@ request.
--
-- 'maxResults', 'listVPCAssociationAuthorizations_maxResults' - /Optional/: An integer that specifies the maximum number of VPCs that
-- you want Amazon Route 53 to return. If you don\'t specify a value for
-- @MaxResults@, Route 53 returns up to 50 VPCs per page.
--
-- 'hostedZoneId', 'listVPCAssociationAuthorizations_hostedZoneId' - The ID of the hosted zone for which you want a list of VPCs that can be
-- associated with the hosted zone.
newListVPCAssociationAuthorizations ::
  -- | 'hostedZoneId'
  ResourceId ->
  ListVPCAssociationAuthorizations
newListVPCAssociationAuthorizations pHostedZoneId_ =
  ListVPCAssociationAuthorizations'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      hostedZoneId = pHostedZoneId_
    }

-- | /Optional/: If a response includes a @NextToken@ element, there are more
-- VPCs that can be associated with the specified hosted zone. To get the
-- next page of results, submit another request, and include the value of
-- @NextToken@ from the response in the @nexttoken@ parameter in another
-- @ListVPCAssociationAuthorizations@ request.
listVPCAssociationAuthorizations_nextToken :: Lens.Lens' ListVPCAssociationAuthorizations (Core.Maybe Core.Text)
listVPCAssociationAuthorizations_nextToken = Lens.lens (\ListVPCAssociationAuthorizations' {nextToken} -> nextToken) (\s@ListVPCAssociationAuthorizations' {} a -> s {nextToken = a} :: ListVPCAssociationAuthorizations)

-- | /Optional/: An integer that specifies the maximum number of VPCs that
-- you want Amazon Route 53 to return. If you don\'t specify a value for
-- @MaxResults@, Route 53 returns up to 50 VPCs per page.
listVPCAssociationAuthorizations_maxResults :: Lens.Lens' ListVPCAssociationAuthorizations (Core.Maybe Core.Text)
listVPCAssociationAuthorizations_maxResults = Lens.lens (\ListVPCAssociationAuthorizations' {maxResults} -> maxResults) (\s@ListVPCAssociationAuthorizations' {} a -> s {maxResults = a} :: ListVPCAssociationAuthorizations)

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
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listVPCAssociationAuthorizationsResponse_vPCs
              Core.. Lens.to Core.toList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listVPCAssociationAuthorizations_nextToken
          Lens..~ rs
          Lens.^? listVPCAssociationAuthorizationsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListVPCAssociationAuthorizations
  where
  type
    AWSResponse ListVPCAssociationAuthorizations =
      ListVPCAssociationAuthorizationsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListVPCAssociationAuthorizationsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "HostedZoneId")
            Core.<*> ( x Core..@? "VPCs" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList1 "VPC"
                     )
      )

instance
  Core.Hashable
    ListVPCAssociationAuthorizations

instance Core.NFData ListVPCAssociationAuthorizations

instance
  Core.ToHeaders
    ListVPCAssociationAuthorizations
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListVPCAssociationAuthorizations where
  toPath ListVPCAssociationAuthorizations' {..} =
    Core.mconcat
      [ "/2013-04-01/hostedzone/",
        Core.toBS hostedZoneId,
        "/authorizevpcassociation"
      ]

instance
  Core.ToQuery
    ListVPCAssociationAuthorizations
  where
  toQuery ListVPCAssociationAuthorizations' {..} =
    Core.mconcat
      [ "nexttoken" Core.=: nextToken,
        "maxresults" Core.=: maxResults
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
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ID of the hosted zone that you can associate the listed VPCs with.
    hostedZoneId :: ResourceId,
    -- | The list of VPCs that are authorized to be associated with the specified
    -- hosted zone.
    vPCs :: Core.NonEmpty VPC
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vPCs'
  Core.NonEmpty VPC ->
  ListVPCAssociationAuthorizationsResponse
newListVPCAssociationAuthorizationsResponse
  pHttpStatus_
  pHostedZoneId_
  pVPCs_ =
    ListVPCAssociationAuthorizationsResponse'
      { nextToken =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        hostedZoneId = pHostedZoneId_,
        vPCs = Lens._Coerce Lens.# pVPCs_
      }

-- | When the response includes a @NextToken@ element, there are more VPCs
-- that can be associated with the specified hosted zone. To get the next
-- page of VPCs, submit another @ListVPCAssociationAuthorizations@ request,
-- and include the value of the @NextToken@ element from the response in
-- the @nexttoken@ request parameter.
listVPCAssociationAuthorizationsResponse_nextToken :: Lens.Lens' ListVPCAssociationAuthorizationsResponse (Core.Maybe Core.Text)
listVPCAssociationAuthorizationsResponse_nextToken = Lens.lens (\ListVPCAssociationAuthorizationsResponse' {nextToken} -> nextToken) (\s@ListVPCAssociationAuthorizationsResponse' {} a -> s {nextToken = a} :: ListVPCAssociationAuthorizationsResponse)

-- | The response's http status code.
listVPCAssociationAuthorizationsResponse_httpStatus :: Lens.Lens' ListVPCAssociationAuthorizationsResponse Core.Int
listVPCAssociationAuthorizationsResponse_httpStatus = Lens.lens (\ListVPCAssociationAuthorizationsResponse' {httpStatus} -> httpStatus) (\s@ListVPCAssociationAuthorizationsResponse' {} a -> s {httpStatus = a} :: ListVPCAssociationAuthorizationsResponse)

-- | The ID of the hosted zone that you can associate the listed VPCs with.
listVPCAssociationAuthorizationsResponse_hostedZoneId :: Lens.Lens' ListVPCAssociationAuthorizationsResponse ResourceId
listVPCAssociationAuthorizationsResponse_hostedZoneId = Lens.lens (\ListVPCAssociationAuthorizationsResponse' {hostedZoneId} -> hostedZoneId) (\s@ListVPCAssociationAuthorizationsResponse' {} a -> s {hostedZoneId = a} :: ListVPCAssociationAuthorizationsResponse)

-- | The list of VPCs that are authorized to be associated with the specified
-- hosted zone.
listVPCAssociationAuthorizationsResponse_vPCs :: Lens.Lens' ListVPCAssociationAuthorizationsResponse (Core.NonEmpty VPC)
listVPCAssociationAuthorizationsResponse_vPCs = Lens.lens (\ListVPCAssociationAuthorizationsResponse' {vPCs} -> vPCs) (\s@ListVPCAssociationAuthorizationsResponse' {} a -> s {vPCs = a} :: ListVPCAssociationAuthorizationsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListVPCAssociationAuthorizationsResponse

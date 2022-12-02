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
-- Module      : Amazonka.Route53.ListResourceRecordSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource record sets in a specified hosted zone.
--
-- @ListResourceRecordSets@ returns up to 300 resource record sets at a
-- time in ASCII order, beginning at a position specified by the @name@ and
-- @type@ elements.
--
-- __Sort order__
--
-- @ListResourceRecordSets@ sorts results first by DNS name with the labels
-- reversed, for example:
--
-- @com.example.www.@
--
-- Note the trailing dot, which can change the sort order when the record
-- name contains characters that appear before @.@ (decimal 46) in the
-- ASCII table. These characters include the following:
-- @! \" # $ % & \' ( ) * + , -@
--
-- When multiple records have the same DNS name, @ListResourceRecordSets@
-- sorts results by the record type.
--
-- __Specifying where to start listing records__
--
-- You can use the name and type elements to specify the resource record
-- set that the list begins with:
--
-- [If you do not specify Name or Type]
--     The results begin with the first resource record set that the hosted
--     zone contains.
--
-- [If you specify Name but not Type]
--     The results begin with the first resource record set in the list
--     whose name is greater than or equal to @Name@.
--
-- [If you specify Type but not Name]
--     Amazon Route 53 returns the @InvalidInput@ error.
--
-- [If you specify both Name and Type]
--     The results begin with the first resource record set in the list
--     whose name is greater than or equal to @Name@, and whose type is
--     greater than or equal to @Type@.
--
-- __Resource record sets that are PENDING__
--
-- This action returns the most current version of the records. This
-- includes records that are @PENDING@, and that are not yet available on
-- all Route 53 DNS servers.
--
-- __Changing resource record sets__
--
-- To ensure that you get an accurate listing of the resource record sets
-- for a hosted zone at a point in time, do not submit a
-- @ChangeResourceRecordSets@ request while you\'re paging through the
-- results of a @ListResourceRecordSets@ request. If you do, some pages may
-- display results without the latest changes while other pages display
-- results with the latest changes.
--
-- __Displaying the next page of results__
--
-- If a @ListResourceRecordSets@ command returns more than one page of
-- results, the value of @IsTruncated@ is @true@. To display the next page
-- of results, get the values of @NextRecordName@, @NextRecordType@, and
-- @NextRecordIdentifier@ (if any) from the response. Then submit another
-- @ListResourceRecordSets@ request, and specify those values for
-- @StartRecordName@, @StartRecordType@, and @StartRecordIdentifier@.
--
-- This operation returns paginated results.
module Amazonka.Route53.ListResourceRecordSets
  ( -- * Creating a Request
    ListResourceRecordSets (..),
    newListResourceRecordSets,

    -- * Request Lenses
    listResourceRecordSets_startRecordType,
    listResourceRecordSets_maxItems,
    listResourceRecordSets_startRecordIdentifier,
    listResourceRecordSets_startRecordName,
    listResourceRecordSets_hostedZoneId,

    -- * Destructuring the Response
    ListResourceRecordSetsResponse (..),
    newListResourceRecordSetsResponse,

    -- * Response Lenses
    listResourceRecordSetsResponse_nextRecordIdentifier,
    listResourceRecordSetsResponse_nextRecordName,
    listResourceRecordSetsResponse_nextRecordType,
    listResourceRecordSetsResponse_httpStatus,
    listResourceRecordSetsResponse_resourceRecordSets,
    listResourceRecordSetsResponse_isTruncated,
    listResourceRecordSetsResponse_maxItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request for the resource record sets that are associated with a
-- specified hosted zone.
--
-- /See:/ 'newListResourceRecordSets' smart constructor.
data ListResourceRecordSets = ListResourceRecordSets'
  { -- | The type of resource record set to begin the record listing from.
    --
    -- Valid values for basic resource record sets: @A@ | @AAAA@ | @CAA@ |
    -- @CNAME@ | @MX@ | @NAPTR@ | @NS@ | @PTR@ | @SOA@ | @SPF@ | @SRV@ | @TXT@
    --
    -- Values for weighted, latency, geolocation, and failover resource record
    -- sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ |
    -- @SRV@ | @TXT@
    --
    -- Values for alias resource record sets:
    --
    -- -   __API Gateway custom regional API or edge-optimized API__: A
    --
    -- -   __CloudFront distribution__: A or AAAA
    --
    -- -   __Elastic Beanstalk environment that has a regionalized subdomain__:
    --     A
    --
    -- -   __Elastic Load Balancing load balancer__: A | AAAA
    --
    -- -   __S3 bucket__: A
    --
    -- -   __VPC interface VPC endpoint__: A
    --
    -- -   __Another resource record set in this hosted zone:__ The type of the
    --     resource record set that the alias references.
    --
    -- Constraint: Specifying @type@ without specifying @name@ returns an
    -- @InvalidInput@ error.
    startRecordType :: Prelude.Maybe RRType,
    -- | (Optional) The maximum number of resource records sets to include in the
    -- response body for this request. If the response includes more than
    -- @maxitems@ resource record sets, the value of the @IsTruncated@ element
    -- in the response is @true@, and the values of the @NextRecordName@ and
    -- @NextRecordType@ elements in the response identify the first resource
    -- record set in the next group of @maxitems@ resource record sets.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | /Resource record sets that have a routing policy other than simple:/ If
    -- results were truncated for a given DNS name and type, specify the value
    -- of @NextRecordIdentifier@ from the previous response to get the next
    -- resource record set that has the current DNS name and type.
    startRecordIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The first name in the lexicographic ordering of resource record sets
    -- that you want to list. If the specified record name doesn\'t exist, the
    -- results begin with the first resource record set that has a name greater
    -- than the value of @name@.
    startRecordName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the hosted zone that contains the resource record sets that
    -- you want to list.
    hostedZoneId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceRecordSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startRecordType', 'listResourceRecordSets_startRecordType' - The type of resource record set to begin the record listing from.
--
-- Valid values for basic resource record sets: @A@ | @AAAA@ | @CAA@ |
-- @CNAME@ | @MX@ | @NAPTR@ | @NS@ | @PTR@ | @SOA@ | @SPF@ | @SRV@ | @TXT@
--
-- Values for weighted, latency, geolocation, and failover resource record
-- sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ |
-- @SRV@ | @TXT@
--
-- Values for alias resource record sets:
--
-- -   __API Gateway custom regional API or edge-optimized API__: A
--
-- -   __CloudFront distribution__: A or AAAA
--
-- -   __Elastic Beanstalk environment that has a regionalized subdomain__:
--     A
--
-- -   __Elastic Load Balancing load balancer__: A | AAAA
--
-- -   __S3 bucket__: A
--
-- -   __VPC interface VPC endpoint__: A
--
-- -   __Another resource record set in this hosted zone:__ The type of the
--     resource record set that the alias references.
--
-- Constraint: Specifying @type@ without specifying @name@ returns an
-- @InvalidInput@ error.
--
-- 'maxItems', 'listResourceRecordSets_maxItems' - (Optional) The maximum number of resource records sets to include in the
-- response body for this request. If the response includes more than
-- @maxitems@ resource record sets, the value of the @IsTruncated@ element
-- in the response is @true@, and the values of the @NextRecordName@ and
-- @NextRecordType@ elements in the response identify the first resource
-- record set in the next group of @maxitems@ resource record sets.
--
-- 'startRecordIdentifier', 'listResourceRecordSets_startRecordIdentifier' - /Resource record sets that have a routing policy other than simple:/ If
-- results were truncated for a given DNS name and type, specify the value
-- of @NextRecordIdentifier@ from the previous response to get the next
-- resource record set that has the current DNS name and type.
--
-- 'startRecordName', 'listResourceRecordSets_startRecordName' - The first name in the lexicographic ordering of resource record sets
-- that you want to list. If the specified record name doesn\'t exist, the
-- results begin with the first resource record set that has a name greater
-- than the value of @name@.
--
-- 'hostedZoneId', 'listResourceRecordSets_hostedZoneId' - The ID of the hosted zone that contains the resource record sets that
-- you want to list.
newListResourceRecordSets ::
  -- | 'hostedZoneId'
  ResourceId ->
  ListResourceRecordSets
newListResourceRecordSets pHostedZoneId_ =
  ListResourceRecordSets'
    { startRecordType =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      startRecordIdentifier = Prelude.Nothing,
      startRecordName = Prelude.Nothing,
      hostedZoneId = pHostedZoneId_
    }

-- | The type of resource record set to begin the record listing from.
--
-- Valid values for basic resource record sets: @A@ | @AAAA@ | @CAA@ |
-- @CNAME@ | @MX@ | @NAPTR@ | @NS@ | @PTR@ | @SOA@ | @SPF@ | @SRV@ | @TXT@
--
-- Values for weighted, latency, geolocation, and failover resource record
-- sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ |
-- @SRV@ | @TXT@
--
-- Values for alias resource record sets:
--
-- -   __API Gateway custom regional API or edge-optimized API__: A
--
-- -   __CloudFront distribution__: A or AAAA
--
-- -   __Elastic Beanstalk environment that has a regionalized subdomain__:
--     A
--
-- -   __Elastic Load Balancing load balancer__: A | AAAA
--
-- -   __S3 bucket__: A
--
-- -   __VPC interface VPC endpoint__: A
--
-- -   __Another resource record set in this hosted zone:__ The type of the
--     resource record set that the alias references.
--
-- Constraint: Specifying @type@ without specifying @name@ returns an
-- @InvalidInput@ error.
listResourceRecordSets_startRecordType :: Lens.Lens' ListResourceRecordSets (Prelude.Maybe RRType)
listResourceRecordSets_startRecordType = Lens.lens (\ListResourceRecordSets' {startRecordType} -> startRecordType) (\s@ListResourceRecordSets' {} a -> s {startRecordType = a} :: ListResourceRecordSets)

-- | (Optional) The maximum number of resource records sets to include in the
-- response body for this request. If the response includes more than
-- @maxitems@ resource record sets, the value of the @IsTruncated@ element
-- in the response is @true@, and the values of the @NextRecordName@ and
-- @NextRecordType@ elements in the response identify the first resource
-- record set in the next group of @maxitems@ resource record sets.
listResourceRecordSets_maxItems :: Lens.Lens' ListResourceRecordSets (Prelude.Maybe Prelude.Text)
listResourceRecordSets_maxItems = Lens.lens (\ListResourceRecordSets' {maxItems} -> maxItems) (\s@ListResourceRecordSets' {} a -> s {maxItems = a} :: ListResourceRecordSets)

-- | /Resource record sets that have a routing policy other than simple:/ If
-- results were truncated for a given DNS name and type, specify the value
-- of @NextRecordIdentifier@ from the previous response to get the next
-- resource record set that has the current DNS name and type.
listResourceRecordSets_startRecordIdentifier :: Lens.Lens' ListResourceRecordSets (Prelude.Maybe Prelude.Text)
listResourceRecordSets_startRecordIdentifier = Lens.lens (\ListResourceRecordSets' {startRecordIdentifier} -> startRecordIdentifier) (\s@ListResourceRecordSets' {} a -> s {startRecordIdentifier = a} :: ListResourceRecordSets)

-- | The first name in the lexicographic ordering of resource record sets
-- that you want to list. If the specified record name doesn\'t exist, the
-- results begin with the first resource record set that has a name greater
-- than the value of @name@.
listResourceRecordSets_startRecordName :: Lens.Lens' ListResourceRecordSets (Prelude.Maybe Prelude.Text)
listResourceRecordSets_startRecordName = Lens.lens (\ListResourceRecordSets' {startRecordName} -> startRecordName) (\s@ListResourceRecordSets' {} a -> s {startRecordName = a} :: ListResourceRecordSets)

-- | The ID of the hosted zone that contains the resource record sets that
-- you want to list.
listResourceRecordSets_hostedZoneId :: Lens.Lens' ListResourceRecordSets ResourceId
listResourceRecordSets_hostedZoneId = Lens.lens (\ListResourceRecordSets' {hostedZoneId} -> hostedZoneId) (\s@ListResourceRecordSets' {} a -> s {hostedZoneId = a} :: ListResourceRecordSets)

instance Core.AWSPager ListResourceRecordSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^. listResourceRecordSetsResponse_isTruncated
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listResourceRecordSetsResponse_nextRecordName
              Prelude.. Lens._Just
        )
        Prelude.&& Prelude.isNothing
          ( rs
              Lens.^? listResourceRecordSetsResponse_nextRecordType
                Prelude.. Lens._Just
          )
        Prelude.&& Prelude.isNothing
          ( rs
              Lens.^? listResourceRecordSetsResponse_nextRecordIdentifier
                Prelude.. Lens._Just
          ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceRecordSets_startRecordName
          Lens..~ rs
          Lens.^? listResourceRecordSetsResponse_nextRecordName
            Prelude.. Lens._Just
          Prelude.& listResourceRecordSets_startRecordType
          Lens..~ rs
          Lens.^? listResourceRecordSetsResponse_nextRecordType
            Prelude.. Lens._Just
          Prelude.& listResourceRecordSets_startRecordIdentifier
          Lens..~ rs
          Lens.^? listResourceRecordSetsResponse_nextRecordIdentifier
            Prelude.. Lens._Just

instance Core.AWSRequest ListResourceRecordSets where
  type
    AWSResponse ListResourceRecordSets =
      ListResourceRecordSetsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListResourceRecordSetsResponse'
            Prelude.<$> (x Data..@? "NextRecordIdentifier")
            Prelude.<*> (x Data..@? "NextRecordName")
            Prelude.<*> (x Data..@? "NextRecordType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "ResourceRecordSets"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "ResourceRecordSet"
                        )
            Prelude.<*> (x Data..@ "IsTruncated")
            Prelude.<*> (x Data..@ "MaxItems")
      )

instance Prelude.Hashable ListResourceRecordSets where
  hashWithSalt _salt ListResourceRecordSets' {..} =
    _salt `Prelude.hashWithSalt` startRecordType
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` startRecordIdentifier
      `Prelude.hashWithSalt` startRecordName
      `Prelude.hashWithSalt` hostedZoneId

instance Prelude.NFData ListResourceRecordSets where
  rnf ListResourceRecordSets' {..} =
    Prelude.rnf startRecordType
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf startRecordIdentifier
      `Prelude.seq` Prelude.rnf startRecordName
      `Prelude.seq` Prelude.rnf hostedZoneId

instance Data.ToHeaders ListResourceRecordSets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListResourceRecordSets where
  toPath ListResourceRecordSets' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/rrset"
      ]

instance Data.ToQuery ListResourceRecordSets where
  toQuery ListResourceRecordSets' {..} =
    Prelude.mconcat
      [ "type" Data.=: startRecordType,
        "maxitems" Data.=: maxItems,
        "identifier" Data.=: startRecordIdentifier,
        "name" Data.=: startRecordName
      ]

-- | A complex type that contains list information for the resource record
-- set.
--
-- /See:/ 'newListResourceRecordSetsResponse' smart constructor.
data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse'
  { -- | /Resource record sets that have a routing policy other than simple:/ If
    -- results were truncated for a given DNS name and type, the value of
    -- @SetIdentifier@ for the next resource record set that has the current
    -- DNS name and type.
    --
    -- For information about routing policies, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html Choosing a Routing Policy>
    -- in the /Amazon Route 53 Developer Guide/.
    nextRecordIdentifier :: Prelude.Maybe Prelude.Text,
    -- | If the results were truncated, the name of the next record in the list.
    --
    -- This element is present only if @IsTruncated@ is true.
    nextRecordName :: Prelude.Maybe Prelude.Text,
    -- | If the results were truncated, the type of the next record in the list.
    --
    -- This element is present only if @IsTruncated@ is true.
    nextRecordType :: Prelude.Maybe RRType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about multiple resource record sets.
    resourceRecordSets :: [ResourceRecordSet],
    -- | A flag that indicates whether more resource record sets remain to be
    -- listed. If your results were truncated, you can make a follow-up
    -- pagination request by using the @NextRecordName@ element.
    isTruncated :: Prelude.Bool,
    -- | The maximum number of records you requested.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceRecordSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextRecordIdentifier', 'listResourceRecordSetsResponse_nextRecordIdentifier' - /Resource record sets that have a routing policy other than simple:/ If
-- results were truncated for a given DNS name and type, the value of
-- @SetIdentifier@ for the next resource record set that has the current
-- DNS name and type.
--
-- For information about routing policies, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html Choosing a Routing Policy>
-- in the /Amazon Route 53 Developer Guide/.
--
-- 'nextRecordName', 'listResourceRecordSetsResponse_nextRecordName' - If the results were truncated, the name of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true.
--
-- 'nextRecordType', 'listResourceRecordSetsResponse_nextRecordType' - If the results were truncated, the type of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true.
--
-- 'httpStatus', 'listResourceRecordSetsResponse_httpStatus' - The response's http status code.
--
-- 'resourceRecordSets', 'listResourceRecordSetsResponse_resourceRecordSets' - Information about multiple resource record sets.
--
-- 'isTruncated', 'listResourceRecordSetsResponse_isTruncated' - A flag that indicates whether more resource record sets remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request by using the @NextRecordName@ element.
--
-- 'maxItems', 'listResourceRecordSetsResponse_maxItems' - The maximum number of records you requested.
newListResourceRecordSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'maxItems'
  Prelude.Text ->
  ListResourceRecordSetsResponse
newListResourceRecordSetsResponse
  pHttpStatus_
  pIsTruncated_
  pMaxItems_ =
    ListResourceRecordSetsResponse'
      { nextRecordIdentifier =
          Prelude.Nothing,
        nextRecordName = Prelude.Nothing,
        nextRecordType = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        resourceRecordSets = Prelude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | /Resource record sets that have a routing policy other than simple:/ If
-- results were truncated for a given DNS name and type, the value of
-- @SetIdentifier@ for the next resource record set that has the current
-- DNS name and type.
--
-- For information about routing policies, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html Choosing a Routing Policy>
-- in the /Amazon Route 53 Developer Guide/.
listResourceRecordSetsResponse_nextRecordIdentifier :: Lens.Lens' ListResourceRecordSetsResponse (Prelude.Maybe Prelude.Text)
listResourceRecordSetsResponse_nextRecordIdentifier = Lens.lens (\ListResourceRecordSetsResponse' {nextRecordIdentifier} -> nextRecordIdentifier) (\s@ListResourceRecordSetsResponse' {} a -> s {nextRecordIdentifier = a} :: ListResourceRecordSetsResponse)

-- | If the results were truncated, the name of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true.
listResourceRecordSetsResponse_nextRecordName :: Lens.Lens' ListResourceRecordSetsResponse (Prelude.Maybe Prelude.Text)
listResourceRecordSetsResponse_nextRecordName = Lens.lens (\ListResourceRecordSetsResponse' {nextRecordName} -> nextRecordName) (\s@ListResourceRecordSetsResponse' {} a -> s {nextRecordName = a} :: ListResourceRecordSetsResponse)

-- | If the results were truncated, the type of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true.
listResourceRecordSetsResponse_nextRecordType :: Lens.Lens' ListResourceRecordSetsResponse (Prelude.Maybe RRType)
listResourceRecordSetsResponse_nextRecordType = Lens.lens (\ListResourceRecordSetsResponse' {nextRecordType} -> nextRecordType) (\s@ListResourceRecordSetsResponse' {} a -> s {nextRecordType = a} :: ListResourceRecordSetsResponse)

-- | The response's http status code.
listResourceRecordSetsResponse_httpStatus :: Lens.Lens' ListResourceRecordSetsResponse Prelude.Int
listResourceRecordSetsResponse_httpStatus = Lens.lens (\ListResourceRecordSetsResponse' {httpStatus} -> httpStatus) (\s@ListResourceRecordSetsResponse' {} a -> s {httpStatus = a} :: ListResourceRecordSetsResponse)

-- | Information about multiple resource record sets.
listResourceRecordSetsResponse_resourceRecordSets :: Lens.Lens' ListResourceRecordSetsResponse [ResourceRecordSet]
listResourceRecordSetsResponse_resourceRecordSets = Lens.lens (\ListResourceRecordSetsResponse' {resourceRecordSets} -> resourceRecordSets) (\s@ListResourceRecordSetsResponse' {} a -> s {resourceRecordSets = a} :: ListResourceRecordSetsResponse) Prelude.. Lens.coerced

-- | A flag that indicates whether more resource record sets remain to be
-- listed. If your results were truncated, you can make a follow-up
-- pagination request by using the @NextRecordName@ element.
listResourceRecordSetsResponse_isTruncated :: Lens.Lens' ListResourceRecordSetsResponse Prelude.Bool
listResourceRecordSetsResponse_isTruncated = Lens.lens (\ListResourceRecordSetsResponse' {isTruncated} -> isTruncated) (\s@ListResourceRecordSetsResponse' {} a -> s {isTruncated = a} :: ListResourceRecordSetsResponse)

-- | The maximum number of records you requested.
listResourceRecordSetsResponse_maxItems :: Lens.Lens' ListResourceRecordSetsResponse Prelude.Text
listResourceRecordSetsResponse_maxItems = Lens.lens (\ListResourceRecordSetsResponse' {maxItems} -> maxItems) (\s@ListResourceRecordSetsResponse' {} a -> s {maxItems = a} :: ListResourceRecordSetsResponse)

instance
  Prelude.NFData
    ListResourceRecordSetsResponse
  where
  rnf ListResourceRecordSetsResponse' {..} =
    Prelude.rnf nextRecordIdentifier
      `Prelude.seq` Prelude.rnf nextRecordName
      `Prelude.seq` Prelude.rnf nextRecordType
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceRecordSets
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf maxItems

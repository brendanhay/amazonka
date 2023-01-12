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
-- Module      : Amazonka.Route53.ListHostedZonesByName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of your hosted zones in lexicographic order. The
-- response includes a @HostedZones@ child element for each hosted zone
-- created by the current Amazon Web Services account.
--
-- @ListHostedZonesByName@ sorts hosted zones by name with the labels
-- reversed. For example:
--
-- @com.example.www.@
--
-- Note the trailing dot, which can change the sort order in some
-- circumstances.
--
-- If the domain name includes escape characters or Punycode,
-- @ListHostedZonesByName@ alphabetizes the domain name using the escaped
-- or Punycoded value, which is the format that Amazon Route 53 saves in
-- its database. For example, to create a hosted zone for exämple.com, you
-- specify ex\\344mple.com for the domain name. @ListHostedZonesByName@
-- alphabetizes it as:
--
-- @com.ex\\344mple.@
--
-- The labels are reversed and alphabetized using the escaped value. For
-- more information about valid domain name formats, including
-- internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Route 53 returns up to 100 items in each response. If you have a lot of
-- hosted zones, use the @MaxItems@ parameter to list them in groups of up
-- to 100. The response includes values that help navigate from one group
-- of @MaxItems@ hosted zones to the next:
--
-- -   The @DNSName@ and @HostedZoneId@ elements in the response contain
--     the values, if any, specified for the @dnsname@ and @hostedzoneid@
--     parameters in the request that produced the current response.
--
-- -   The @MaxItems@ element in the response contains the value, if any,
--     that you specified for the @maxitems@ parameter in the request that
--     produced the current response.
--
-- -   If the value of @IsTruncated@ in the response is true, there are
--     more hosted zones associated with the current Amazon Web Services
--     account.
--
--     If @IsTruncated@ is false, this response includes the last hosted
--     zone that is associated with the current account. The @NextDNSName@
--     element and @NextHostedZoneId@ elements are omitted from the
--     response.
--
-- -   The @NextDNSName@ and @NextHostedZoneId@ elements in the response
--     contain the domain name and the hosted zone ID of the next hosted
--     zone that is associated with the current Amazon Web Services
--     account. If you want to list more hosted zones, make another call to
--     @ListHostedZonesByName@, and specify the value of @NextDNSName@ and
--     @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters,
--     respectively.
module Amazonka.Route53.ListHostedZonesByName
  ( -- * Creating a Request
    ListHostedZonesByName (..),
    newListHostedZonesByName,

    -- * Request Lenses
    listHostedZonesByName_dNSName,
    listHostedZonesByName_hostedZoneId,
    listHostedZonesByName_maxItems,

    -- * Destructuring the Response
    ListHostedZonesByNameResponse (..),
    newListHostedZonesByNameResponse,

    -- * Response Lenses
    listHostedZonesByNameResponse_dNSName,
    listHostedZonesByNameResponse_hostedZoneId,
    listHostedZonesByNameResponse_nextDNSName,
    listHostedZonesByNameResponse_nextHostedZoneId,
    listHostedZonesByNameResponse_httpStatus,
    listHostedZonesByNameResponse_hostedZones,
    listHostedZonesByNameResponse_isTruncated,
    listHostedZonesByNameResponse_maxItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | Retrieves a list of the public and private hosted zones that are
-- associated with the current Amazon Web Services account in ASCII order
-- by domain name.
--
-- /See:/ 'newListHostedZonesByName' smart constructor.
data ListHostedZonesByName = ListHostedZonesByName'
  { -- | (Optional) For your first request to @ListHostedZonesByName@, include
    -- the @dnsname@ parameter only if you want to specify the name of the
    -- first hosted zone in the response. If you don\'t include the @dnsname@
    -- parameter, Amazon Route 53 returns all of the hosted zones that were
    -- created by the current Amazon Web Services account, in ASCII order. For
    -- subsequent requests, include both @dnsname@ and @hostedzoneid@
    -- parameters. For @dnsname@, specify the value of @NextDNSName@ from the
    -- previous response.
    dNSName :: Prelude.Maybe Prelude.Text,
    -- | (Optional) For your first request to @ListHostedZonesByName@, do not
    -- include the @hostedzoneid@ parameter.
    --
    -- If you have more hosted zones than the value of @maxitems@,
    -- @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones.
    -- To get the next group of @maxitems@ hosted zones, submit another request
    -- to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@
    -- parameters. For the value of @hostedzoneid@, specify the value of the
    -- @NextHostedZoneId@ element from the previous response.
    hostedZoneId :: Prelude.Maybe ResourceId,
    -- | The maximum number of hosted zones to be included in the response body
    -- for this request. If you have more than @maxitems@ hosted zones, then
    -- the value of the @IsTruncated@ element in the response is true, and the
    -- values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted
    -- zone in the next group of @maxitems@ hosted zones.
    maxItems :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostedZonesByName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dNSName', 'listHostedZonesByName_dNSName' - (Optional) For your first request to @ListHostedZonesByName@, include
-- the @dnsname@ parameter only if you want to specify the name of the
-- first hosted zone in the response. If you don\'t include the @dnsname@
-- parameter, Amazon Route 53 returns all of the hosted zones that were
-- created by the current Amazon Web Services account, in ASCII order. For
-- subsequent requests, include both @dnsname@ and @hostedzoneid@
-- parameters. For @dnsname@, specify the value of @NextDNSName@ from the
-- previous response.
--
-- 'hostedZoneId', 'listHostedZonesByName_hostedZoneId' - (Optional) For your first request to @ListHostedZonesByName@, do not
-- include the @hostedzoneid@ parameter.
--
-- If you have more hosted zones than the value of @maxitems@,
-- @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones.
-- To get the next group of @maxitems@ hosted zones, submit another request
-- to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@
-- parameters. For the value of @hostedzoneid@, specify the value of the
-- @NextHostedZoneId@ element from the previous response.
--
-- 'maxItems', 'listHostedZonesByName_maxItems' - The maximum number of hosted zones to be included in the response body
-- for this request. If you have more than @maxitems@ hosted zones, then
-- the value of the @IsTruncated@ element in the response is true, and the
-- values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted
-- zone in the next group of @maxitems@ hosted zones.
newListHostedZonesByName ::
  ListHostedZonesByName
newListHostedZonesByName =
  ListHostedZonesByName'
    { dNSName = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | (Optional) For your first request to @ListHostedZonesByName@, include
-- the @dnsname@ parameter only if you want to specify the name of the
-- first hosted zone in the response. If you don\'t include the @dnsname@
-- parameter, Amazon Route 53 returns all of the hosted zones that were
-- created by the current Amazon Web Services account, in ASCII order. For
-- subsequent requests, include both @dnsname@ and @hostedzoneid@
-- parameters. For @dnsname@, specify the value of @NextDNSName@ from the
-- previous response.
listHostedZonesByName_dNSName :: Lens.Lens' ListHostedZonesByName (Prelude.Maybe Prelude.Text)
listHostedZonesByName_dNSName = Lens.lens (\ListHostedZonesByName' {dNSName} -> dNSName) (\s@ListHostedZonesByName' {} a -> s {dNSName = a} :: ListHostedZonesByName)

-- | (Optional) For your first request to @ListHostedZonesByName@, do not
-- include the @hostedzoneid@ parameter.
--
-- If you have more hosted zones than the value of @maxitems@,
-- @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones.
-- To get the next group of @maxitems@ hosted zones, submit another request
-- to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@
-- parameters. For the value of @hostedzoneid@, specify the value of the
-- @NextHostedZoneId@ element from the previous response.
listHostedZonesByName_hostedZoneId :: Lens.Lens' ListHostedZonesByName (Prelude.Maybe ResourceId)
listHostedZonesByName_hostedZoneId = Lens.lens (\ListHostedZonesByName' {hostedZoneId} -> hostedZoneId) (\s@ListHostedZonesByName' {} a -> s {hostedZoneId = a} :: ListHostedZonesByName)

-- | The maximum number of hosted zones to be included in the response body
-- for this request. If you have more than @maxitems@ hosted zones, then
-- the value of the @IsTruncated@ element in the response is true, and the
-- values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted
-- zone in the next group of @maxitems@ hosted zones.
listHostedZonesByName_maxItems :: Lens.Lens' ListHostedZonesByName (Prelude.Maybe Prelude.Text)
listHostedZonesByName_maxItems = Lens.lens (\ListHostedZonesByName' {maxItems} -> maxItems) (\s@ListHostedZonesByName' {} a -> s {maxItems = a} :: ListHostedZonesByName)

instance Core.AWSRequest ListHostedZonesByName where
  type
    AWSResponse ListHostedZonesByName =
      ListHostedZonesByNameResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListHostedZonesByNameResponse'
            Prelude.<$> (x Data..@? "DNSName")
            Prelude.<*> (x Data..@? "HostedZoneId")
            Prelude.<*> (x Data..@? "NextDNSName")
            Prelude.<*> (x Data..@? "NextHostedZoneId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "HostedZones" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "HostedZone"
                        )
            Prelude.<*> (x Data..@ "IsTruncated")
            Prelude.<*> (x Data..@ "MaxItems")
      )

instance Prelude.Hashable ListHostedZonesByName where
  hashWithSalt _salt ListHostedZonesByName' {..} =
    _salt `Prelude.hashWithSalt` dNSName
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListHostedZonesByName where
  rnf ListHostedZonesByName' {..} =
    Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf maxItems

instance Data.ToHeaders ListHostedZonesByName where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListHostedZonesByName where
  toPath =
    Prelude.const "/2013-04-01/hostedzonesbyname"

instance Data.ToQuery ListHostedZonesByName where
  toQuery ListHostedZonesByName' {..} =
    Prelude.mconcat
      [ "dnsname" Data.=: dNSName,
        "hostedzoneid" Data.=: hostedZoneId,
        "maxitems" Data.=: maxItems
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'newListHostedZonesByNameResponse' smart constructor.
data ListHostedZonesByNameResponse = ListHostedZonesByNameResponse'
  { -- | For the second and subsequent calls to @ListHostedZonesByName@,
    -- @DNSName@ is the value that you specified for the @dnsname@ parameter in
    -- the request that produced the current response.
    dNSName :: Prelude.Maybe Prelude.Text,
    -- | The ID that Amazon Route 53 assigned to the hosted zone when you created
    -- it.
    hostedZoneId :: Prelude.Maybe ResourceId,
    -- | If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the
    -- first hosted zone in the next group of @maxitems@ hosted zones. Call
    -- @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and
    -- @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters,
    -- respectively.
    --
    -- This element is present only if @IsTruncated@ is @true@.
    nextDNSName :: Prelude.Maybe Prelude.Text,
    -- | If @IsTruncated@ is @true@, the value of @NextHostedZoneId@ identifies
    -- the first hosted zone in the next group of @maxitems@ hosted zones. Call
    -- @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and
    -- @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters,
    -- respectively.
    --
    -- This element is present only if @IsTruncated@ is @true@.
    nextHostedZoneId :: Prelude.Maybe ResourceId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains general information about the hosted zone.
    hostedZones :: [HostedZone],
    -- | A flag that indicates whether there are more hosted zones to be listed.
    -- If the response was truncated, you can get the next group of @maxitems@
    -- hosted zones by calling @ListHostedZonesByName@ again and specifying the
    -- values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@
    -- and @hostedzoneid@ parameters.
    isTruncated :: Prelude.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to
    -- @ListHostedZonesByName@ that produced the current response.
    maxItems :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHostedZonesByNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dNSName', 'listHostedZonesByNameResponse_dNSName' - For the second and subsequent calls to @ListHostedZonesByName@,
-- @DNSName@ is the value that you specified for the @dnsname@ parameter in
-- the request that produced the current response.
--
-- 'hostedZoneId', 'listHostedZonesByNameResponse_hostedZoneId' - The ID that Amazon Route 53 assigned to the hosted zone when you created
-- it.
--
-- 'nextDNSName', 'listHostedZonesByNameResponse_nextDNSName' - If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the
-- first hosted zone in the next group of @maxitems@ hosted zones. Call
-- @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and
-- @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters,
-- respectively.
--
-- This element is present only if @IsTruncated@ is @true@.
--
-- 'nextHostedZoneId', 'listHostedZonesByNameResponse_nextHostedZoneId' - If @IsTruncated@ is @true@, the value of @NextHostedZoneId@ identifies
-- the first hosted zone in the next group of @maxitems@ hosted zones. Call
-- @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and
-- @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters,
-- respectively.
--
-- This element is present only if @IsTruncated@ is @true@.
--
-- 'httpStatus', 'listHostedZonesByNameResponse_httpStatus' - The response's http status code.
--
-- 'hostedZones', 'listHostedZonesByNameResponse_hostedZones' - A complex type that contains general information about the hosted zone.
--
-- 'isTruncated', 'listHostedZonesByNameResponse_isTruncated' - A flag that indicates whether there are more hosted zones to be listed.
-- If the response was truncated, you can get the next group of @maxitems@
-- hosted zones by calling @ListHostedZonesByName@ again and specifying the
-- values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@
-- and @hostedzoneid@ parameters.
--
-- 'maxItems', 'listHostedZonesByNameResponse_maxItems' - The value that you specified for the @maxitems@ parameter in the call to
-- @ListHostedZonesByName@ that produced the current response.
newListHostedZonesByNameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isTruncated'
  Prelude.Bool ->
  -- | 'maxItems'
  Prelude.Text ->
  ListHostedZonesByNameResponse
newListHostedZonesByNameResponse
  pHttpStatus_
  pIsTruncated_
  pMaxItems_ =
    ListHostedZonesByNameResponse'
      { dNSName =
          Prelude.Nothing,
        hostedZoneId = Prelude.Nothing,
        nextDNSName = Prelude.Nothing,
        nextHostedZoneId = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        hostedZones = Prelude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | For the second and subsequent calls to @ListHostedZonesByName@,
-- @DNSName@ is the value that you specified for the @dnsname@ parameter in
-- the request that produced the current response.
listHostedZonesByNameResponse_dNSName :: Lens.Lens' ListHostedZonesByNameResponse (Prelude.Maybe Prelude.Text)
listHostedZonesByNameResponse_dNSName = Lens.lens (\ListHostedZonesByNameResponse' {dNSName} -> dNSName) (\s@ListHostedZonesByNameResponse' {} a -> s {dNSName = a} :: ListHostedZonesByNameResponse)

-- | The ID that Amazon Route 53 assigned to the hosted zone when you created
-- it.
listHostedZonesByNameResponse_hostedZoneId :: Lens.Lens' ListHostedZonesByNameResponse (Prelude.Maybe ResourceId)
listHostedZonesByNameResponse_hostedZoneId = Lens.lens (\ListHostedZonesByNameResponse' {hostedZoneId} -> hostedZoneId) (\s@ListHostedZonesByNameResponse' {} a -> s {hostedZoneId = a} :: ListHostedZonesByNameResponse)

-- | If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the
-- first hosted zone in the next group of @maxitems@ hosted zones. Call
-- @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and
-- @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters,
-- respectively.
--
-- This element is present only if @IsTruncated@ is @true@.
listHostedZonesByNameResponse_nextDNSName :: Lens.Lens' ListHostedZonesByNameResponse (Prelude.Maybe Prelude.Text)
listHostedZonesByNameResponse_nextDNSName = Lens.lens (\ListHostedZonesByNameResponse' {nextDNSName} -> nextDNSName) (\s@ListHostedZonesByNameResponse' {} a -> s {nextDNSName = a} :: ListHostedZonesByNameResponse)

-- | If @IsTruncated@ is @true@, the value of @NextHostedZoneId@ identifies
-- the first hosted zone in the next group of @maxitems@ hosted zones. Call
-- @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and
-- @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters,
-- respectively.
--
-- This element is present only if @IsTruncated@ is @true@.
listHostedZonesByNameResponse_nextHostedZoneId :: Lens.Lens' ListHostedZonesByNameResponse (Prelude.Maybe ResourceId)
listHostedZonesByNameResponse_nextHostedZoneId = Lens.lens (\ListHostedZonesByNameResponse' {nextHostedZoneId} -> nextHostedZoneId) (\s@ListHostedZonesByNameResponse' {} a -> s {nextHostedZoneId = a} :: ListHostedZonesByNameResponse)

-- | The response's http status code.
listHostedZonesByNameResponse_httpStatus :: Lens.Lens' ListHostedZonesByNameResponse Prelude.Int
listHostedZonesByNameResponse_httpStatus = Lens.lens (\ListHostedZonesByNameResponse' {httpStatus} -> httpStatus) (\s@ListHostedZonesByNameResponse' {} a -> s {httpStatus = a} :: ListHostedZonesByNameResponse)

-- | A complex type that contains general information about the hosted zone.
listHostedZonesByNameResponse_hostedZones :: Lens.Lens' ListHostedZonesByNameResponse [HostedZone]
listHostedZonesByNameResponse_hostedZones = Lens.lens (\ListHostedZonesByNameResponse' {hostedZones} -> hostedZones) (\s@ListHostedZonesByNameResponse' {} a -> s {hostedZones = a} :: ListHostedZonesByNameResponse) Prelude.. Lens.coerced

-- | A flag that indicates whether there are more hosted zones to be listed.
-- If the response was truncated, you can get the next group of @maxitems@
-- hosted zones by calling @ListHostedZonesByName@ again and specifying the
-- values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@
-- and @hostedzoneid@ parameters.
listHostedZonesByNameResponse_isTruncated :: Lens.Lens' ListHostedZonesByNameResponse Prelude.Bool
listHostedZonesByNameResponse_isTruncated = Lens.lens (\ListHostedZonesByNameResponse' {isTruncated} -> isTruncated) (\s@ListHostedZonesByNameResponse' {} a -> s {isTruncated = a} :: ListHostedZonesByNameResponse)

-- | The value that you specified for the @maxitems@ parameter in the call to
-- @ListHostedZonesByName@ that produced the current response.
listHostedZonesByNameResponse_maxItems :: Lens.Lens' ListHostedZonesByNameResponse Prelude.Text
listHostedZonesByNameResponse_maxItems = Lens.lens (\ListHostedZonesByNameResponse' {maxItems} -> maxItems) (\s@ListHostedZonesByNameResponse' {} a -> s {maxItems = a} :: ListHostedZonesByNameResponse)

instance Prelude.NFData ListHostedZonesByNameResponse where
  rnf ListHostedZonesByNameResponse' {..} =
    Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf nextDNSName
      `Prelude.seq` Prelude.rnf nextHostedZoneId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hostedZones
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf maxItems

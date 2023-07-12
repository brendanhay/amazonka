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
-- Module      : Amazonka.Route53.ChangeResourceRecordSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates, changes, or deletes a resource record set, which contains
-- authoritative DNS information for a specified domain name or subdomain
-- name. For example, you can use @ChangeResourceRecordSets@ to create a
-- resource record set that routes traffic for test.example.com to a web
-- server that has an IP address of 192.0.2.44.
--
-- __Deleting Resource Record Sets__
--
-- To delete a resource record set, you must specify all the same values
-- that you specified when you created it.
--
-- __Change Batches and Transactional Changes__
--
-- The request body must include a document with a
-- @ChangeResourceRecordSetsRequest@ element. The request body contains a
-- list of change items, known as a change batch. Change batches are
-- considered transactional changes. Route 53 validates the changes in the
-- request and then either makes all or none of the changes in the change
-- batch request. This ensures that DNS routing isn\'t adversely affected
-- by partial changes to the resource record sets in a hosted zone.
--
-- For example, suppose a change batch request contains two changes: it
-- deletes the @CNAME@ resource record set for www.example.com and creates
-- an alias resource record set for www.example.com. If validation for both
-- records succeeds, Route 53 deletes the first resource record set and
-- creates the second resource record set in a single operation. If
-- validation for either the @DELETE@ or the @CREATE@ action fails, then
-- the request is canceled, and the original @CNAME@ record continues to
-- exist.
--
-- If you try to delete the same resource record set more than once in a
-- single change batch, Route 53 returns an @InvalidChangeBatch@ error.
--
-- __Traffic Flow__
--
-- To create resource record sets for complex routing configurations, use
-- either the traffic flow visual editor in the Route 53 console or the API
-- actions for traffic policies and traffic policy instances. Save the
-- configuration as a traffic policy, then associate the traffic policy
-- with one or more domain names (such as example.com) or subdomain names
-- (such as www.example.com), in the same hosted zone or in multiple hosted
-- zones. You can roll back the updates if the new configuration isn\'t
-- performing as expected. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/traffic-flow.html Using Traffic Flow to Route DNS Traffic>
-- in the /Amazon Route 53 Developer Guide/.
--
-- __Create, Delete, and Upsert__
--
-- Use @ChangeResourceRecordsSetsRequest@ to perform the following actions:
--
-- -   @CREATE@: Creates a resource record set that has the specified
--     values.
--
-- -   @DELETE@: Deletes an existing resource record set that has the
--     specified values.
--
-- -   @UPSERT@: If a resource set exists Route 53 updates it with the
--     values in the request.
--
-- __Syntaxes for Creating, Updating, and Deleting Resource Record Sets__
--
-- The syntax for a request depends on the type of resource record set that
-- you want to create, delete, or update, such as weighted, alias, or
-- failover. The XML elements in your request must appear in the order
-- listed in the syntax.
--
-- For an example for each type of resource record set, see \"Examples.\"
--
-- Don\'t refer to the syntax in the \"Parameter Syntax\" section, which
-- includes all of the elements for every kind of resource record set that
-- you can create, delete, or update by using @ChangeResourceRecordSets@.
--
-- __Change Propagation to Route 53 DNS Servers__
--
-- When you submit a @ChangeResourceRecordSets@ request, Route 53
-- propagates your changes to all of the Route 53 authoritative DNS
-- servers. While your changes are propagating, @GetChange@ returns a
-- status of @PENDING@. When propagation is complete, @GetChange@ returns a
-- status of @INSYNC@. Changes generally propagate to all Route 53 name
-- servers within 60 seconds. For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange>.
--
-- __Limits on ChangeResourceRecordSets Requests__
--
-- For information about the limits on a @ChangeResourceRecordSets@
-- request, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits>
-- in the /Amazon Route 53 Developer Guide/.
module Amazonka.Route53.ChangeResourceRecordSets
  ( -- * Creating a Request
    ChangeResourceRecordSets (..),
    newChangeResourceRecordSets,

    -- * Request Lenses
    changeResourceRecordSets_hostedZoneId,
    changeResourceRecordSets_changeBatch,

    -- * Destructuring the Response
    ChangeResourceRecordSetsResponse (..),
    newChangeResourceRecordSetsResponse,

    -- * Response Lenses
    changeResourceRecordSetsResponse_httpStatus,
    changeResourceRecordSetsResponse_changeInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains change information for the resource record
-- set.
--
-- /See:/ 'newChangeResourceRecordSets' smart constructor.
data ChangeResourceRecordSets = ChangeResourceRecordSets'
  { -- | The ID of the hosted zone that contains the resource record sets that
    -- you want to change.
    hostedZoneId :: ResourceId,
    -- | A complex type that contains an optional comment and the @Changes@
    -- element.
    changeBatch :: ChangeBatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeResourceRecordSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostedZoneId', 'changeResourceRecordSets_hostedZoneId' - The ID of the hosted zone that contains the resource record sets that
-- you want to change.
--
-- 'changeBatch', 'changeResourceRecordSets_changeBatch' - A complex type that contains an optional comment and the @Changes@
-- element.
newChangeResourceRecordSets ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'changeBatch'
  ChangeBatch ->
  ChangeResourceRecordSets
newChangeResourceRecordSets
  pHostedZoneId_
  pChangeBatch_ =
    ChangeResourceRecordSets'
      { hostedZoneId =
          pHostedZoneId_,
        changeBatch = pChangeBatch_
      }

-- | The ID of the hosted zone that contains the resource record sets that
-- you want to change.
changeResourceRecordSets_hostedZoneId :: Lens.Lens' ChangeResourceRecordSets ResourceId
changeResourceRecordSets_hostedZoneId = Lens.lens (\ChangeResourceRecordSets' {hostedZoneId} -> hostedZoneId) (\s@ChangeResourceRecordSets' {} a -> s {hostedZoneId = a} :: ChangeResourceRecordSets)

-- | A complex type that contains an optional comment and the @Changes@
-- element.
changeResourceRecordSets_changeBatch :: Lens.Lens' ChangeResourceRecordSets ChangeBatch
changeResourceRecordSets_changeBatch = Lens.lens (\ChangeResourceRecordSets' {changeBatch} -> changeBatch) (\s@ChangeResourceRecordSets' {} a -> s {changeBatch = a} :: ChangeResourceRecordSets)

instance Core.AWSRequest ChangeResourceRecordSets where
  type
    AWSResponse ChangeResourceRecordSets =
      ChangeResourceRecordSetsResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ChangeResourceRecordSetsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable ChangeResourceRecordSets where
  hashWithSalt _salt ChangeResourceRecordSets' {..} =
    _salt
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` changeBatch

instance Prelude.NFData ChangeResourceRecordSets where
  rnf ChangeResourceRecordSets' {..} =
    Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf changeBatch

instance Data.ToElement ChangeResourceRecordSets where
  toElement =
    Data.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeResourceRecordSetsRequest"

instance Data.ToHeaders ChangeResourceRecordSets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ChangeResourceRecordSets where
  toPath ChangeResourceRecordSets' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzone/",
        Data.toBS hostedZoneId,
        "/rrset/"
      ]

instance Data.ToQuery ChangeResourceRecordSets where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML ChangeResourceRecordSets where
  toXML ChangeResourceRecordSets' {..} =
    Prelude.mconcat ["ChangeBatch" Data.@= changeBatch]

-- | A complex type containing the response for the request.
--
-- /See:/ 'newChangeResourceRecordSetsResponse' smart constructor.
data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains information about changes made to your
    -- hosted zone.
    --
    -- This element contains an ID that you use when performing a
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange>
    -- action to get detailed information about the change.
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeResourceRecordSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'changeResourceRecordSetsResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'changeResourceRecordSetsResponse_changeInfo' - A complex type that contains information about changes made to your
-- hosted zone.
--
-- This element contains an ID that you use when performing a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange>
-- action to get detailed information about the change.
newChangeResourceRecordSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  ChangeResourceRecordSetsResponse
newChangeResourceRecordSetsResponse
  pHttpStatus_
  pChangeInfo_ =
    ChangeResourceRecordSetsResponse'
      { httpStatus =
          pHttpStatus_,
        changeInfo = pChangeInfo_
      }

-- | The response's http status code.
changeResourceRecordSetsResponse_httpStatus :: Lens.Lens' ChangeResourceRecordSetsResponse Prelude.Int
changeResourceRecordSetsResponse_httpStatus = Lens.lens (\ChangeResourceRecordSetsResponse' {httpStatus} -> httpStatus) (\s@ChangeResourceRecordSetsResponse' {} a -> s {httpStatus = a} :: ChangeResourceRecordSetsResponse)

-- | A complex type that contains information about changes made to your
-- hosted zone.
--
-- This element contains an ID that you use when performing a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange>
-- action to get detailed information about the change.
changeResourceRecordSetsResponse_changeInfo :: Lens.Lens' ChangeResourceRecordSetsResponse ChangeInfo
changeResourceRecordSetsResponse_changeInfo = Lens.lens (\ChangeResourceRecordSetsResponse' {changeInfo} -> changeInfo) (\s@ChangeResourceRecordSetsResponse' {} a -> s {changeInfo = a} :: ChangeResourceRecordSetsResponse)

instance
  Prelude.NFData
    ChangeResourceRecordSetsResponse
  where
  rnf ChangeResourceRecordSetsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo

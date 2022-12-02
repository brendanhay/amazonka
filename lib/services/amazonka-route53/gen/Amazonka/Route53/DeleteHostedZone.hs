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
-- Module      : Amazonka.Route53.DeleteHostedZone
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a hosted zone.
--
-- If the hosted zone was created by another service, such as Cloud Map,
-- see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DeleteHostedZone.html#delete-public-hosted-zone-created-by-another-service Deleting Public Hosted Zones That Were Created by Another Service>
-- in the /Amazon Route 53 Developer Guide/ for information about how to
-- delete it. (The process is the same for public and private hosted zones
-- that were created by another service.)
--
-- If you want to keep your domain registration but you want to stop
-- routing internet traffic to your website or web application, we
-- recommend that you delete resource record sets in the hosted zone
-- instead of deleting the hosted zone.
--
-- If you delete a hosted zone, you can\'t undelete it. You must create a
-- new hosted zone and update the name servers for your domain
-- registration, which can require up to 48 hours to take effect. (If you
-- delegated responsibility for a subdomain to a hosted zone and you delete
-- the child hosted zone, you must update the name servers in the parent
-- hosted zone.) In addition, if you delete a hosted zone, someone could
-- hijack the domain and route traffic to their own resources using your
-- domain name.
--
-- If you want to avoid the monthly charge for the hosted zone, you can
-- transfer DNS service for the domain to a free DNS service. When you
-- transfer DNS service, you have to update the name servers for the domain
-- registration. If the domain is registered with Route 53, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_UpdateDomainNameservers.html UpdateDomainNameservers>
-- for information about how to replace Route 53 name servers with name
-- servers for the new DNS service. If the domain is registered with
-- another registrar, use the method provided by the registrar to update
-- name servers for the domain registration. For more information, perform
-- an internet search on \"free DNS service.\"
--
-- You can delete a hosted zone only if it contains only the default SOA
-- record and NS resource record sets. If the hosted zone contains other
-- resource record sets, you must delete them before you can delete the
-- hosted zone. If you try to delete a hosted zone that contains other
-- resource record sets, the request fails, and Route 53 returns a
-- @HostedZoneNotEmpty@ error. For information about deleting records from
-- your hosted zone, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html ChangeResourceRecordSets>.
--
-- To verify that the hosted zone has been deleted, do one of the
-- following:
--
-- -   Use the @GetHostedZone@ action to request information about the
--     hosted zone.
--
-- -   Use the @ListHostedZones@ action to get a list of the hosted zones
--     associated with the current Amazon Web Services account.
module Amazonka.Route53.DeleteHostedZone
  ( -- * Creating a Request
    DeleteHostedZone (..),
    newDeleteHostedZone,

    -- * Request Lenses
    deleteHostedZone_id,

    -- * Destructuring the Response
    DeleteHostedZoneResponse (..),
    newDeleteHostedZoneResponse,

    -- * Response Lenses
    deleteHostedZoneResponse_httpStatus,
    deleteHostedZoneResponse_changeInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A request to delete a hosted zone.
--
-- /See:/ 'newDeleteHostedZone' smart constructor.
data DeleteHostedZone = DeleteHostedZone'
  { -- | The ID of the hosted zone you want to delete.
    id :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHostedZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteHostedZone_id' - The ID of the hosted zone you want to delete.
newDeleteHostedZone ::
  -- | 'id'
  ResourceId ->
  DeleteHostedZone
newDeleteHostedZone pId_ =
  DeleteHostedZone' {id = pId_}

-- | The ID of the hosted zone you want to delete.
deleteHostedZone_id :: Lens.Lens' DeleteHostedZone ResourceId
deleteHostedZone_id = Lens.lens (\DeleteHostedZone' {id} -> id) (\s@DeleteHostedZone' {} a -> s {id = a} :: DeleteHostedZone)

instance Core.AWSRequest DeleteHostedZone where
  type
    AWSResponse DeleteHostedZone =
      DeleteHostedZoneResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteHostedZoneResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable DeleteHostedZone where
  hashWithSalt _salt DeleteHostedZone' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteHostedZone where
  rnf DeleteHostedZone' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteHostedZone where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteHostedZone where
  toPath DeleteHostedZone' {..} =
    Prelude.mconcat
      ["/2013-04-01/hostedzone/", Data.toBS id]

instance Data.ToQuery DeleteHostedZone where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the response to a @DeleteHostedZone@
-- request.
--
-- /See:/ 'newDeleteHostedZoneResponse' smart constructor.
data DeleteHostedZoneResponse = DeleteHostedZoneResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains the ID, the status, and the date and time
    -- of a request to delete a hosted zone.
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHostedZoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteHostedZoneResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'deleteHostedZoneResponse_changeInfo' - A complex type that contains the ID, the status, and the date and time
-- of a request to delete a hosted zone.
newDeleteHostedZoneResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  DeleteHostedZoneResponse
newDeleteHostedZoneResponse pHttpStatus_ pChangeInfo_ =
  DeleteHostedZoneResponse'
    { httpStatus =
        pHttpStatus_,
      changeInfo = pChangeInfo_
    }

-- | The response's http status code.
deleteHostedZoneResponse_httpStatus :: Lens.Lens' DeleteHostedZoneResponse Prelude.Int
deleteHostedZoneResponse_httpStatus = Lens.lens (\DeleteHostedZoneResponse' {httpStatus} -> httpStatus) (\s@DeleteHostedZoneResponse' {} a -> s {httpStatus = a} :: DeleteHostedZoneResponse)

-- | A complex type that contains the ID, the status, and the date and time
-- of a request to delete a hosted zone.
deleteHostedZoneResponse_changeInfo :: Lens.Lens' DeleteHostedZoneResponse ChangeInfo
deleteHostedZoneResponse_changeInfo = Lens.lens (\DeleteHostedZoneResponse' {changeInfo} -> changeInfo) (\s@DeleteHostedZoneResponse' {} a -> s {changeInfo = a} :: DeleteHostedZoneResponse)

instance Prelude.NFData DeleteHostedZoneResponse where
  rnf DeleteHostedZoneResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo

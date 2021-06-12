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
-- Module      : Network.AWS.RDS.DeleteCustomAvailabilityZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom Availability Zone (AZ).
--
-- A custom AZ is an on-premises AZ that is integrated with a VMware
-- vSphere cluster.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
module Network.AWS.RDS.DeleteCustomAvailabilityZone
  ( -- * Creating a Request
    DeleteCustomAvailabilityZone (..),
    newDeleteCustomAvailabilityZone,

    -- * Request Lenses
    deleteCustomAvailabilityZone_customAvailabilityZoneId,

    -- * Destructuring the Response
    DeleteCustomAvailabilityZoneResponse (..),
    newDeleteCustomAvailabilityZoneResponse,

    -- * Response Lenses
    deleteCustomAvailabilityZoneResponse_customAvailabilityZone,
    deleteCustomAvailabilityZoneResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCustomAvailabilityZone' smart constructor.
data DeleteCustomAvailabilityZone = DeleteCustomAvailabilityZone'
  { -- | The custom AZ identifier.
    customAvailabilityZoneId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCustomAvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAvailabilityZoneId', 'deleteCustomAvailabilityZone_customAvailabilityZoneId' - The custom AZ identifier.
newDeleteCustomAvailabilityZone ::
  -- | 'customAvailabilityZoneId'
  Core.Text ->
  DeleteCustomAvailabilityZone
newDeleteCustomAvailabilityZone
  pCustomAvailabilityZoneId_ =
    DeleteCustomAvailabilityZone'
      { customAvailabilityZoneId =
          pCustomAvailabilityZoneId_
      }

-- | The custom AZ identifier.
deleteCustomAvailabilityZone_customAvailabilityZoneId :: Lens.Lens' DeleteCustomAvailabilityZone Core.Text
deleteCustomAvailabilityZone_customAvailabilityZoneId = Lens.lens (\DeleteCustomAvailabilityZone' {customAvailabilityZoneId} -> customAvailabilityZoneId) (\s@DeleteCustomAvailabilityZone' {} a -> s {customAvailabilityZoneId = a} :: DeleteCustomAvailabilityZone)

instance Core.AWSRequest DeleteCustomAvailabilityZone where
  type
    AWSResponse DeleteCustomAvailabilityZone =
      DeleteCustomAvailabilityZoneResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteCustomAvailabilityZoneResult"
      ( \s h x ->
          DeleteCustomAvailabilityZoneResponse'
            Core.<$> (x Core..@? "CustomAvailabilityZone")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCustomAvailabilityZone

instance Core.NFData DeleteCustomAvailabilityZone

instance Core.ToHeaders DeleteCustomAvailabilityZone where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteCustomAvailabilityZone where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCustomAvailabilityZone where
  toQuery DeleteCustomAvailabilityZone' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteCustomAvailabilityZone" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "CustomAvailabilityZoneId"
          Core.=: customAvailabilityZoneId
      ]

-- | /See:/ 'newDeleteCustomAvailabilityZoneResponse' smart constructor.
data DeleteCustomAvailabilityZoneResponse = DeleteCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Core.Maybe CustomAvailabilityZone,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCustomAvailabilityZoneResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAvailabilityZone', 'deleteCustomAvailabilityZoneResponse_customAvailabilityZone' - Undocumented member.
--
-- 'httpStatus', 'deleteCustomAvailabilityZoneResponse_httpStatus' - The response's http status code.
newDeleteCustomAvailabilityZoneResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteCustomAvailabilityZoneResponse
newDeleteCustomAvailabilityZoneResponse pHttpStatus_ =
  DeleteCustomAvailabilityZoneResponse'
    { customAvailabilityZone =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteCustomAvailabilityZoneResponse_customAvailabilityZone :: Lens.Lens' DeleteCustomAvailabilityZoneResponse (Core.Maybe CustomAvailabilityZone)
deleteCustomAvailabilityZoneResponse_customAvailabilityZone = Lens.lens (\DeleteCustomAvailabilityZoneResponse' {customAvailabilityZone} -> customAvailabilityZone) (\s@DeleteCustomAvailabilityZoneResponse' {} a -> s {customAvailabilityZone = a} :: DeleteCustomAvailabilityZoneResponse)

-- | The response's http status code.
deleteCustomAvailabilityZoneResponse_httpStatus :: Lens.Lens' DeleteCustomAvailabilityZoneResponse Core.Int
deleteCustomAvailabilityZoneResponse_httpStatus = Lens.lens (\DeleteCustomAvailabilityZoneResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomAvailabilityZoneResponse' {} a -> s {httpStatus = a} :: DeleteCustomAvailabilityZoneResponse)

instance
  Core.NFData
    DeleteCustomAvailabilityZoneResponse

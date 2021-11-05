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
-- Module      : Amazonka.RDS.DeleteCustomAvailabilityZone
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
module Amazonka.RDS.DeleteCustomAvailabilityZone
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomAvailabilityZone' smart constructor.
data DeleteCustomAvailabilityZone = DeleteCustomAvailabilityZone'
  { -- | The custom AZ identifier.
    customAvailabilityZoneId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteCustomAvailabilityZone
newDeleteCustomAvailabilityZone
  pCustomAvailabilityZoneId_ =
    DeleteCustomAvailabilityZone'
      { customAvailabilityZoneId =
          pCustomAvailabilityZoneId_
      }

-- | The custom AZ identifier.
deleteCustomAvailabilityZone_customAvailabilityZoneId :: Lens.Lens' DeleteCustomAvailabilityZone Prelude.Text
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
            Prelude.<$> (x Core..@? "CustomAvailabilityZone")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteCustomAvailabilityZone

instance Prelude.NFData DeleteCustomAvailabilityZone

instance Core.ToHeaders DeleteCustomAvailabilityZone where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteCustomAvailabilityZone where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCustomAvailabilityZone where
  toQuery DeleteCustomAvailabilityZone' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteCustomAvailabilityZone" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "CustomAvailabilityZoneId"
          Core.=: customAvailabilityZoneId
      ]

-- | /See:/ 'newDeleteCustomAvailabilityZoneResponse' smart constructor.
data DeleteCustomAvailabilityZoneResponse = DeleteCustomAvailabilityZoneResponse'
  { customAvailabilityZone :: Prelude.Maybe CustomAvailabilityZone,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteCustomAvailabilityZoneResponse
newDeleteCustomAvailabilityZoneResponse pHttpStatus_ =
  DeleteCustomAvailabilityZoneResponse'
    { customAvailabilityZone =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteCustomAvailabilityZoneResponse_customAvailabilityZone :: Lens.Lens' DeleteCustomAvailabilityZoneResponse (Prelude.Maybe CustomAvailabilityZone)
deleteCustomAvailabilityZoneResponse_customAvailabilityZone = Lens.lens (\DeleteCustomAvailabilityZoneResponse' {customAvailabilityZone} -> customAvailabilityZone) (\s@DeleteCustomAvailabilityZoneResponse' {} a -> s {customAvailabilityZone = a} :: DeleteCustomAvailabilityZoneResponse)

-- | The response's http status code.
deleteCustomAvailabilityZoneResponse_httpStatus :: Lens.Lens' DeleteCustomAvailabilityZoneResponse Prelude.Int
deleteCustomAvailabilityZoneResponse_httpStatus = Lens.lens (\DeleteCustomAvailabilityZoneResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomAvailabilityZoneResponse' {} a -> s {httpStatus = a} :: DeleteCustomAvailabilityZoneResponse)

instance
  Prelude.NFData
    DeleteCustomAvailabilityZoneResponse

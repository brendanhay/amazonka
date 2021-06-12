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
-- Module      : Network.AWS.AppStream.DeleteFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified fleet.
module Network.AWS.AppStream.DeleteFleet
  ( -- * Creating a Request
    DeleteFleet (..),
    newDeleteFleet,

    -- * Request Lenses
    deleteFleet_name,

    -- * Destructuring the Response
    DeleteFleetResponse (..),
    newDeleteFleetResponse,

    -- * Response Lenses
    deleteFleetResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFleet' smart constructor.
data DeleteFleet = DeleteFleet'
  { -- | The name of the fleet.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteFleet_name' - The name of the fleet.
newDeleteFleet ::
  -- | 'name'
  Core.Text ->
  DeleteFleet
newDeleteFleet pName_ = DeleteFleet' {name = pName_}

-- | The name of the fleet.
deleteFleet_name :: Lens.Lens' DeleteFleet Core.Text
deleteFleet_name = Lens.lens (\DeleteFleet' {name} -> name) (\s@DeleteFleet' {} a -> s {name = a} :: DeleteFleet)

instance Core.AWSRequest DeleteFleet where
  type AWSResponse DeleteFleet = DeleteFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFleetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteFleet

instance Core.NFData DeleteFleet

instance Core.ToHeaders DeleteFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteFleet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteFleet where
  toJSON DeleteFleet' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteFleet where
  toPath = Core.const "/"

instance Core.ToQuery DeleteFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteFleetResponse' smart constructor.
data DeleteFleetResponse = DeleteFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFleetResponse_httpStatus' - The response's http status code.
newDeleteFleetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteFleetResponse
newDeleteFleetResponse pHttpStatus_ =
  DeleteFleetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFleetResponse_httpStatus :: Lens.Lens' DeleteFleetResponse Core.Int
deleteFleetResponse_httpStatus = Lens.lens (\DeleteFleetResponse' {httpStatus} -> httpStatus) (\s@DeleteFleetResponse' {} a -> s {httpStatus = a} :: DeleteFleetResponse)

instance Core.NFData DeleteFleetResponse

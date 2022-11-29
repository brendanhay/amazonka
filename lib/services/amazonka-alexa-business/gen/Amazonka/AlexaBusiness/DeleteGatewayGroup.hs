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
-- Module      : Amazonka.AlexaBusiness.DeleteGatewayGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway group.
module Amazonka.AlexaBusiness.DeleteGatewayGroup
  ( -- * Creating a Request
    DeleteGatewayGroup (..),
    newDeleteGatewayGroup,

    -- * Request Lenses
    deleteGatewayGroup_gatewayGroupArn,

    -- * Destructuring the Response
    DeleteGatewayGroupResponse (..),
    newDeleteGatewayGroupResponse,

    -- * Response Lenses
    deleteGatewayGroupResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGatewayGroup' smart constructor.
data DeleteGatewayGroup = DeleteGatewayGroup'
  { -- | The ARN of the gateway group to delete.
    gatewayGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayGroupArn', 'deleteGatewayGroup_gatewayGroupArn' - The ARN of the gateway group to delete.
newDeleteGatewayGroup ::
  -- | 'gatewayGroupArn'
  Prelude.Text ->
  DeleteGatewayGroup
newDeleteGatewayGroup pGatewayGroupArn_ =
  DeleteGatewayGroup'
    { gatewayGroupArn =
        pGatewayGroupArn_
    }

-- | The ARN of the gateway group to delete.
deleteGatewayGroup_gatewayGroupArn :: Lens.Lens' DeleteGatewayGroup Prelude.Text
deleteGatewayGroup_gatewayGroupArn = Lens.lens (\DeleteGatewayGroup' {gatewayGroupArn} -> gatewayGroupArn) (\s@DeleteGatewayGroup' {} a -> s {gatewayGroupArn = a} :: DeleteGatewayGroup)

instance Core.AWSRequest DeleteGatewayGroup where
  type
    AWSResponse DeleteGatewayGroup =
      DeleteGatewayGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGatewayGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGatewayGroup where
  hashWithSalt _salt DeleteGatewayGroup' {..} =
    _salt `Prelude.hashWithSalt` gatewayGroupArn

instance Prelude.NFData DeleteGatewayGroup where
  rnf DeleteGatewayGroup' {..} =
    Prelude.rnf gatewayGroupArn

instance Core.ToHeaders DeleteGatewayGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DeleteGatewayGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteGatewayGroup where
  toJSON DeleteGatewayGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GatewayGroupArn" Core..= gatewayGroupArn)
          ]
      )

instance Core.ToPath DeleteGatewayGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteGatewayGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGatewayGroupResponse' smart constructor.
data DeleteGatewayGroupResponse = DeleteGatewayGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGatewayGroupResponse_httpStatus' - The response's http status code.
newDeleteGatewayGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGatewayGroupResponse
newDeleteGatewayGroupResponse pHttpStatus_ =
  DeleteGatewayGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteGatewayGroupResponse_httpStatus :: Lens.Lens' DeleteGatewayGroupResponse Prelude.Int
deleteGatewayGroupResponse_httpStatus = Lens.lens (\DeleteGatewayGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGatewayGroupResponse' {} a -> s {httpStatus = a} :: DeleteGatewayGroupResponse)

instance Prelude.NFData DeleteGatewayGroupResponse where
  rnf DeleteGatewayGroupResponse' {..} =
    Prelude.rnf httpStatus

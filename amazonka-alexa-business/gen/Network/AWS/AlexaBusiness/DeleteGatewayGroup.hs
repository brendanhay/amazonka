{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.DeleteGatewayGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway group.
module Network.AWS.AlexaBusiness.DeleteGatewayGroup
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGatewayGroup' smart constructor.
data DeleteGatewayGroup = DeleteGatewayGroup'
  { -- | The ARN of the gateway group to delete.
    gatewayGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteGatewayGroup where
  type
    Rs DeleteGatewayGroup =
      DeleteGatewayGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGatewayGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGatewayGroup

instance Prelude.NFData DeleteGatewayGroup

instance Prelude.ToHeaders DeleteGatewayGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteGatewayGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteGatewayGroup where
  toJSON DeleteGatewayGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GatewayGroupArn" Prelude..= gatewayGroupArn)
          ]
      )

instance Prelude.ToPath DeleteGatewayGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteGatewayGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGatewayGroupResponse' smart constructor.
data DeleteGatewayGroupResponse = DeleteGatewayGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteGatewayGroupResponse

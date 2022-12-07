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
-- Module      : Amazonka.ApiGatewayV2.DeleteVpcLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a VPC link.
module Amazonka.ApiGatewayV2.DeleteVpcLink
  ( -- * Creating a Request
    DeleteVpcLink (..),
    newDeleteVpcLink,

    -- * Request Lenses
    deleteVpcLink_vpcLinkId,

    -- * Destructuring the Response
    DeleteVpcLinkResponse (..),
    newDeleteVpcLinkResponse,

    -- * Response Lenses
    deleteVpcLinkResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcLink' smart constructor.
data DeleteVpcLink = DeleteVpcLink'
  { -- | The ID of the VPC link.
    vpcLinkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcLinkId', 'deleteVpcLink_vpcLinkId' - The ID of the VPC link.
newDeleteVpcLink ::
  -- | 'vpcLinkId'
  Prelude.Text ->
  DeleteVpcLink
newDeleteVpcLink pVpcLinkId_ =
  DeleteVpcLink' {vpcLinkId = pVpcLinkId_}

-- | The ID of the VPC link.
deleteVpcLink_vpcLinkId :: Lens.Lens' DeleteVpcLink Prelude.Text
deleteVpcLink_vpcLinkId = Lens.lens (\DeleteVpcLink' {vpcLinkId} -> vpcLinkId) (\s@DeleteVpcLink' {} a -> s {vpcLinkId = a} :: DeleteVpcLink)

instance Core.AWSRequest DeleteVpcLink where
  type
    AWSResponse DeleteVpcLink =
      DeleteVpcLinkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVpcLinkResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVpcLink where
  hashWithSalt _salt DeleteVpcLink' {..} =
    _salt `Prelude.hashWithSalt` vpcLinkId

instance Prelude.NFData DeleteVpcLink where
  rnf DeleteVpcLink' {..} = Prelude.rnf vpcLinkId

instance Data.ToHeaders DeleteVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVpcLink where
  toPath DeleteVpcLink' {..} =
    Prelude.mconcat
      ["/v2/vpclinks/", Data.toBS vpcLinkId]

instance Data.ToQuery DeleteVpcLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVpcLinkResponse' smart constructor.
data DeleteVpcLinkResponse = DeleteVpcLinkResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVpcLinkResponse_httpStatus' - The response's http status code.
newDeleteVpcLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVpcLinkResponse
newDeleteVpcLinkResponse pHttpStatus_ =
  DeleteVpcLinkResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteVpcLinkResponse_httpStatus :: Lens.Lens' DeleteVpcLinkResponse Prelude.Int
deleteVpcLinkResponse_httpStatus = Lens.lens (\DeleteVpcLinkResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcLinkResponse' {} a -> s {httpStatus = a} :: DeleteVpcLinkResponse)

instance Prelude.NFData DeleteVpcLinkResponse where
  rnf DeleteVpcLinkResponse' {..} =
    Prelude.rnf httpStatus

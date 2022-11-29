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
-- Module      : Amazonka.APIGateway.DeleteVpcLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing VpcLink of a specified identifier.
module Amazonka.APIGateway.DeleteVpcLink
  ( -- * Creating a Request
    DeleteVpcLink (..),
    newDeleteVpcLink,

    -- * Request Lenses
    deleteVpcLink_vpcLinkId,

    -- * Destructuring the Response
    DeleteVpcLinkResponse (..),
    newDeleteVpcLinkResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes an existing VpcLink of a specified identifier.
--
-- /See:/ 'newDeleteVpcLink' smart constructor.
data DeleteVpcLink = DeleteVpcLink'
  { -- | The identifier of the VpcLink. It is used in an Integration to reference
    -- this VpcLink.
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
-- 'vpcLinkId', 'deleteVpcLink_vpcLinkId' - The identifier of the VpcLink. It is used in an Integration to reference
-- this VpcLink.
newDeleteVpcLink ::
  -- | 'vpcLinkId'
  Prelude.Text ->
  DeleteVpcLink
newDeleteVpcLink pVpcLinkId_ =
  DeleteVpcLink' {vpcLinkId = pVpcLinkId_}

-- | The identifier of the VpcLink. It is used in an Integration to reference
-- this VpcLink.
deleteVpcLink_vpcLinkId :: Lens.Lens' DeleteVpcLink Prelude.Text
deleteVpcLink_vpcLinkId = Lens.lens (\DeleteVpcLink' {vpcLinkId} -> vpcLinkId) (\s@DeleteVpcLink' {} a -> s {vpcLinkId = a} :: DeleteVpcLink)

instance Core.AWSRequest DeleteVpcLink where
  type
    AWSResponse DeleteVpcLink =
      DeleteVpcLinkResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteVpcLinkResponse'

instance Prelude.Hashable DeleteVpcLink where
  hashWithSalt _salt DeleteVpcLink' {..} =
    _salt `Prelude.hashWithSalt` vpcLinkId

instance Prelude.NFData DeleteVpcLink where
  rnf DeleteVpcLink' {..} = Prelude.rnf vpcLinkId

instance Core.ToHeaders DeleteVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath DeleteVpcLink where
  toPath DeleteVpcLink' {..} =
    Prelude.mconcat ["/vpclinks/", Core.toBS vpcLinkId]

instance Core.ToQuery DeleteVpcLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVpcLinkResponse' smart constructor.
data DeleteVpcLinkResponse = DeleteVpcLinkResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVpcLinkResponse ::
  DeleteVpcLinkResponse
newDeleteVpcLinkResponse = DeleteVpcLinkResponse'

instance Prelude.NFData DeleteVpcLinkResponse where
  rnf _ = ()

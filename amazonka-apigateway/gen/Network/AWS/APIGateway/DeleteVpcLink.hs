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
-- Module      : Network.AWS.APIGateway.DeleteVpcLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing VpcLink of a specified identifier.
module Network.AWS.APIGateway.DeleteVpcLink
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes an existing VpcLink of a specified identifier.
--
-- /See:/ 'newDeleteVpcLink' smart constructor.
data DeleteVpcLink = DeleteVpcLink'
  { -- | [Required] The identifier of the VpcLink. It is used in an Integration
    -- to reference this VpcLink.
    vpcLinkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcLinkId', 'deleteVpcLink_vpcLinkId' - [Required] The identifier of the VpcLink. It is used in an Integration
-- to reference this VpcLink.
newDeleteVpcLink ::
  -- | 'vpcLinkId'
  Prelude.Text ->
  DeleteVpcLink
newDeleteVpcLink pVpcLinkId_ =
  DeleteVpcLink' {vpcLinkId = pVpcLinkId_}

-- | [Required] The identifier of the VpcLink. It is used in an Integration
-- to reference this VpcLink.
deleteVpcLink_vpcLinkId :: Lens.Lens' DeleteVpcLink Prelude.Text
deleteVpcLink_vpcLinkId = Lens.lens (\DeleteVpcLink' {vpcLinkId} -> vpcLinkId) (\s@DeleteVpcLink' {} a -> s {vpcLinkId = a} :: DeleteVpcLink)

instance Prelude.AWSRequest DeleteVpcLink where
  type Rs DeleteVpcLink = DeleteVpcLinkResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteVpcLinkResponse'

instance Prelude.Hashable DeleteVpcLink

instance Prelude.NFData DeleteVpcLink

instance Prelude.ToHeaders DeleteVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteVpcLink where
  toPath DeleteVpcLink' {..} =
    Prelude.mconcat
      ["/vpclinks/", Prelude.toBS vpcLinkId]

instance Prelude.ToQuery DeleteVpcLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVpcLinkResponse' smart constructor.
data DeleteVpcLinkResponse = DeleteVpcLinkResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteVpcLinkResponse ::
  DeleteVpcLinkResponse
newDeleteVpcLinkResponse = DeleteVpcLinkResponse'

instance Prelude.NFData DeleteVpcLinkResponse

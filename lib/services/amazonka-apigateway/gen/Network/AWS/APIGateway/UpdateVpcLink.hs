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
-- Module      : Network.AWS.APIGateway.UpdateVpcLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing VpcLink of a specified identifier.
module Network.AWS.APIGateway.UpdateVpcLink
  ( -- * Creating a Request
    UpdateVpcLink (..),
    newUpdateVpcLink,

    -- * Request Lenses
    updateVpcLink_patchOperations,
    updateVpcLink_vpcLinkId,

    -- * Destructuring the Response
    VpcLink (..),
    newVpcLink,

    -- * Response Lenses
    vpcLink_status,
    vpcLink_targetArns,
    vpcLink_name,
    vpcLink_statusMessage,
    vpcLink_id,
    vpcLink_description,
    vpcLink_tags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates an existing VpcLink of a specified identifier.
--
-- /See:/ 'newUpdateVpcLink' smart constructor.
data UpdateVpcLink = UpdateVpcLink'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The identifier of the VpcLink. It is used in an Integration
    -- to reference this VpcLink.
    vpcLinkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateVpcLink_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'vpcLinkId', 'updateVpcLink_vpcLinkId' - [Required] The identifier of the VpcLink. It is used in an Integration
-- to reference this VpcLink.
newUpdateVpcLink ::
  -- | 'vpcLinkId'
  Prelude.Text ->
  UpdateVpcLink
newUpdateVpcLink pVpcLinkId_ =
  UpdateVpcLink'
    { patchOperations = Prelude.Nothing,
      vpcLinkId = pVpcLinkId_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateVpcLink_patchOperations :: Lens.Lens' UpdateVpcLink (Prelude.Maybe [PatchOperation])
updateVpcLink_patchOperations = Lens.lens (\UpdateVpcLink' {patchOperations} -> patchOperations) (\s@UpdateVpcLink' {} a -> s {patchOperations = a} :: UpdateVpcLink) Prelude.. Lens.mapping Lens.coerced

-- | [Required] The identifier of the VpcLink. It is used in an Integration
-- to reference this VpcLink.
updateVpcLink_vpcLinkId :: Lens.Lens' UpdateVpcLink Prelude.Text
updateVpcLink_vpcLinkId = Lens.lens (\UpdateVpcLink' {vpcLinkId} -> vpcLinkId) (\s@UpdateVpcLink' {} a -> s {vpcLinkId = a} :: UpdateVpcLink)

instance Core.AWSRequest UpdateVpcLink where
  type AWSResponse UpdateVpcLink = VpcLink
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateVpcLink

instance Prelude.NFData UpdateVpcLink

instance Core.ToHeaders UpdateVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateVpcLink where
  toJSON UpdateVpcLink' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateVpcLink where
  toPath UpdateVpcLink' {..} =
    Prelude.mconcat ["/vpclinks/", Core.toBS vpcLinkId]

instance Core.ToQuery UpdateVpcLink where
  toQuery = Prelude.const Prelude.mempty

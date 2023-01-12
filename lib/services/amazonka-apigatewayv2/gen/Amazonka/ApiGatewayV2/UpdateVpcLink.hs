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
-- Module      : Amazonka.ApiGatewayV2.UpdateVpcLink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a VPC link.
module Amazonka.ApiGatewayV2.UpdateVpcLink
  ( -- * Creating a Request
    UpdateVpcLink (..),
    newUpdateVpcLink,

    -- * Request Lenses
    updateVpcLink_name,
    updateVpcLink_vpcLinkId,

    -- * Destructuring the Response
    UpdateVpcLinkResponse (..),
    newUpdateVpcLinkResponse,

    -- * Response Lenses
    updateVpcLinkResponse_createdDate,
    updateVpcLinkResponse_name,
    updateVpcLinkResponse_securityGroupIds,
    updateVpcLinkResponse_subnetIds,
    updateVpcLinkResponse_tags,
    updateVpcLinkResponse_vpcLinkId,
    updateVpcLinkResponse_vpcLinkStatus,
    updateVpcLinkResponse_vpcLinkStatusMessage,
    updateVpcLinkResponse_vpcLinkVersion,
    updateVpcLinkResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates a VPC link.
--
-- /See:/ 'newUpdateVpcLink' smart constructor.
data UpdateVpcLink = UpdateVpcLink'
  { -- | The name of the VPC link.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC link.
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
-- 'name', 'updateVpcLink_name' - The name of the VPC link.
--
-- 'vpcLinkId', 'updateVpcLink_vpcLinkId' - The ID of the VPC link.
newUpdateVpcLink ::
  -- | 'vpcLinkId'
  Prelude.Text ->
  UpdateVpcLink
newUpdateVpcLink pVpcLinkId_ =
  UpdateVpcLink'
    { name = Prelude.Nothing,
      vpcLinkId = pVpcLinkId_
    }

-- | The name of the VPC link.
updateVpcLink_name :: Lens.Lens' UpdateVpcLink (Prelude.Maybe Prelude.Text)
updateVpcLink_name = Lens.lens (\UpdateVpcLink' {name} -> name) (\s@UpdateVpcLink' {} a -> s {name = a} :: UpdateVpcLink)

-- | The ID of the VPC link.
updateVpcLink_vpcLinkId :: Lens.Lens' UpdateVpcLink Prelude.Text
updateVpcLink_vpcLinkId = Lens.lens (\UpdateVpcLink' {vpcLinkId} -> vpcLinkId) (\s@UpdateVpcLink' {} a -> s {vpcLinkId = a} :: UpdateVpcLink)

instance Core.AWSRequest UpdateVpcLink where
  type
    AWSResponse UpdateVpcLink =
      UpdateVpcLinkResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVpcLinkResponse'
            Prelude.<$> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> ( x Data..?> "securityGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "subnetIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vpcLinkId")
            Prelude.<*> (x Data..?> "vpcLinkStatus")
            Prelude.<*> (x Data..?> "vpcLinkStatusMessage")
            Prelude.<*> (x Data..?> "vpcLinkVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVpcLink where
  hashWithSalt _salt UpdateVpcLink' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` vpcLinkId

instance Prelude.NFData UpdateVpcLink where
  rnf UpdateVpcLink' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf vpcLinkId

instance Data.ToHeaders UpdateVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVpcLink where
  toJSON UpdateVpcLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [("name" Data..=) Prelude.<$> name]
      )

instance Data.ToPath UpdateVpcLink where
  toPath UpdateVpcLink' {..} =
    Prelude.mconcat
      ["/v2/vpclinks/", Data.toBS vpcLinkId]

instance Data.ToQuery UpdateVpcLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVpcLinkResponse' smart constructor.
data UpdateVpcLinkResponse = UpdateVpcLinkResponse'
  { -- | The timestamp when the VPC link was created.
    createdDate :: Prelude.Maybe Data.ISO8601,
    -- | The name of the VPC link.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of security group IDs for the VPC link.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of subnet IDs to include in the VPC link.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | Tags for the VPC link.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the VPC link.
    vpcLinkId :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPC link.
    vpcLinkStatus :: Prelude.Maybe VpcLinkStatus,
    -- | A message summarizing the cause of the status of the VPC link.
    vpcLinkStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The version of the VPC link.
    vpcLinkVersion :: Prelude.Maybe VpcLinkVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVpcLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'updateVpcLinkResponse_createdDate' - The timestamp when the VPC link was created.
--
-- 'name', 'updateVpcLinkResponse_name' - The name of the VPC link.
--
-- 'securityGroupIds', 'updateVpcLinkResponse_securityGroupIds' - A list of security group IDs for the VPC link.
--
-- 'subnetIds', 'updateVpcLinkResponse_subnetIds' - A list of subnet IDs to include in the VPC link.
--
-- 'tags', 'updateVpcLinkResponse_tags' - Tags for the VPC link.
--
-- 'vpcLinkId', 'updateVpcLinkResponse_vpcLinkId' - The ID of the VPC link.
--
-- 'vpcLinkStatus', 'updateVpcLinkResponse_vpcLinkStatus' - The status of the VPC link.
--
-- 'vpcLinkStatusMessage', 'updateVpcLinkResponse_vpcLinkStatusMessage' - A message summarizing the cause of the status of the VPC link.
--
-- 'vpcLinkVersion', 'updateVpcLinkResponse_vpcLinkVersion' - The version of the VPC link.
--
-- 'httpStatus', 'updateVpcLinkResponse_httpStatus' - The response's http status code.
newUpdateVpcLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVpcLinkResponse
newUpdateVpcLinkResponse pHttpStatus_ =
  UpdateVpcLinkResponse'
    { createdDate =
        Prelude.Nothing,
      name = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcLinkId = Prelude.Nothing,
      vpcLinkStatus = Prelude.Nothing,
      vpcLinkStatusMessage = Prelude.Nothing,
      vpcLinkVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp when the VPC link was created.
updateVpcLinkResponse_createdDate :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe Prelude.UTCTime)
updateVpcLinkResponse_createdDate = Lens.lens (\UpdateVpcLinkResponse' {createdDate} -> createdDate) (\s@UpdateVpcLinkResponse' {} a -> s {createdDate = a} :: UpdateVpcLinkResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the VPC link.
updateVpcLinkResponse_name :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe Prelude.Text)
updateVpcLinkResponse_name = Lens.lens (\UpdateVpcLinkResponse' {name} -> name) (\s@UpdateVpcLinkResponse' {} a -> s {name = a} :: UpdateVpcLinkResponse)

-- | A list of security group IDs for the VPC link.
updateVpcLinkResponse_securityGroupIds :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe [Prelude.Text])
updateVpcLinkResponse_securityGroupIds = Lens.lens (\UpdateVpcLinkResponse' {securityGroupIds} -> securityGroupIds) (\s@UpdateVpcLinkResponse' {} a -> s {securityGroupIds = a} :: UpdateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of subnet IDs to include in the VPC link.
updateVpcLinkResponse_subnetIds :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe [Prelude.Text])
updateVpcLinkResponse_subnetIds = Lens.lens (\UpdateVpcLinkResponse' {subnetIds} -> subnetIds) (\s@UpdateVpcLinkResponse' {} a -> s {subnetIds = a} :: UpdateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | Tags for the VPC link.
updateVpcLinkResponse_tags :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateVpcLinkResponse_tags = Lens.lens (\UpdateVpcLinkResponse' {tags} -> tags) (\s@UpdateVpcLinkResponse' {} a -> s {tags = a} :: UpdateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC link.
updateVpcLinkResponse_vpcLinkId :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe Prelude.Text)
updateVpcLinkResponse_vpcLinkId = Lens.lens (\UpdateVpcLinkResponse' {vpcLinkId} -> vpcLinkId) (\s@UpdateVpcLinkResponse' {} a -> s {vpcLinkId = a} :: UpdateVpcLinkResponse)

-- | The status of the VPC link.
updateVpcLinkResponse_vpcLinkStatus :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe VpcLinkStatus)
updateVpcLinkResponse_vpcLinkStatus = Lens.lens (\UpdateVpcLinkResponse' {vpcLinkStatus} -> vpcLinkStatus) (\s@UpdateVpcLinkResponse' {} a -> s {vpcLinkStatus = a} :: UpdateVpcLinkResponse)

-- | A message summarizing the cause of the status of the VPC link.
updateVpcLinkResponse_vpcLinkStatusMessage :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe Prelude.Text)
updateVpcLinkResponse_vpcLinkStatusMessage = Lens.lens (\UpdateVpcLinkResponse' {vpcLinkStatusMessage} -> vpcLinkStatusMessage) (\s@UpdateVpcLinkResponse' {} a -> s {vpcLinkStatusMessage = a} :: UpdateVpcLinkResponse)

-- | The version of the VPC link.
updateVpcLinkResponse_vpcLinkVersion :: Lens.Lens' UpdateVpcLinkResponse (Prelude.Maybe VpcLinkVersion)
updateVpcLinkResponse_vpcLinkVersion = Lens.lens (\UpdateVpcLinkResponse' {vpcLinkVersion} -> vpcLinkVersion) (\s@UpdateVpcLinkResponse' {} a -> s {vpcLinkVersion = a} :: UpdateVpcLinkResponse)

-- | The response's http status code.
updateVpcLinkResponse_httpStatus :: Lens.Lens' UpdateVpcLinkResponse Prelude.Int
updateVpcLinkResponse_httpStatus = Lens.lens (\UpdateVpcLinkResponse' {httpStatus} -> httpStatus) (\s@UpdateVpcLinkResponse' {} a -> s {httpStatus = a} :: UpdateVpcLinkResponse)

instance Prelude.NFData UpdateVpcLinkResponse where
  rnf UpdateVpcLinkResponse' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcLinkId
      `Prelude.seq` Prelude.rnf vpcLinkStatus
      `Prelude.seq` Prelude.rnf vpcLinkStatusMessage
      `Prelude.seq` Prelude.rnf vpcLinkVersion
      `Prelude.seq` Prelude.rnf httpStatus

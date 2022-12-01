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
-- Module      : Amazonka.ApiGatewayV2.GetVpcLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a VPC link.
module Amazonka.ApiGatewayV2.GetVpcLink
  ( -- * Creating a Request
    GetVpcLink (..),
    newGetVpcLink,

    -- * Request Lenses
    getVpcLink_vpcLinkId,

    -- * Destructuring the Response
    GetVpcLinkResponse (..),
    newGetVpcLinkResponse,

    -- * Response Lenses
    getVpcLinkResponse_tags,
    getVpcLinkResponse_vpcLinkStatusMessage,
    getVpcLinkResponse_name,
    getVpcLinkResponse_securityGroupIds,
    getVpcLinkResponse_vpcLinkVersion,
    getVpcLinkResponse_vpcLinkStatus,
    getVpcLinkResponse_vpcLinkId,
    getVpcLinkResponse_createdDate,
    getVpcLinkResponse_subnetIds,
    getVpcLinkResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVpcLink' smart constructor.
data GetVpcLink = GetVpcLink'
  { -- | The ID of the VPC link.
    vpcLinkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcLinkId', 'getVpcLink_vpcLinkId' - The ID of the VPC link.
newGetVpcLink ::
  -- | 'vpcLinkId'
  Prelude.Text ->
  GetVpcLink
newGetVpcLink pVpcLinkId_ =
  GetVpcLink' {vpcLinkId = pVpcLinkId_}

-- | The ID of the VPC link.
getVpcLink_vpcLinkId :: Lens.Lens' GetVpcLink Prelude.Text
getVpcLink_vpcLinkId = Lens.lens (\GetVpcLink' {vpcLinkId} -> vpcLinkId) (\s@GetVpcLink' {} a -> s {vpcLinkId = a} :: GetVpcLink)

instance Core.AWSRequest GetVpcLink where
  type AWSResponse GetVpcLink = GetVpcLinkResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVpcLinkResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "vpcLinkStatusMessage")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> ( x Core..?> "securityGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "vpcLinkVersion")
            Prelude.<*> (x Core..?> "vpcLinkStatus")
            Prelude.<*> (x Core..?> "vpcLinkId")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "subnetIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVpcLink where
  hashWithSalt _salt GetVpcLink' {..} =
    _salt `Prelude.hashWithSalt` vpcLinkId

instance Prelude.NFData GetVpcLink where
  rnf GetVpcLink' {..} = Prelude.rnf vpcLinkId

instance Core.ToHeaders GetVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetVpcLink where
  toPath GetVpcLink' {..} =
    Prelude.mconcat
      ["/v2/vpclinks/", Core.toBS vpcLinkId]

instance Core.ToQuery GetVpcLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVpcLinkResponse' smart constructor.
data GetVpcLinkResponse = GetVpcLinkResponse'
  { -- | Tags for the VPC link.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A message summarizing the cause of the status of the VPC link.
    vpcLinkStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC link.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of security group IDs for the VPC link.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The version of the VPC link.
    vpcLinkVersion :: Prelude.Maybe VpcLinkVersion,
    -- | The status of the VPC link.
    vpcLinkStatus :: Prelude.Maybe VpcLinkStatus,
    -- | The ID of the VPC link.
    vpcLinkId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the VPC link was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | A list of subnet IDs to include in the VPC link.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVpcLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getVpcLinkResponse_tags' - Tags for the VPC link.
--
-- 'vpcLinkStatusMessage', 'getVpcLinkResponse_vpcLinkStatusMessage' - A message summarizing the cause of the status of the VPC link.
--
-- 'name', 'getVpcLinkResponse_name' - The name of the VPC link.
--
-- 'securityGroupIds', 'getVpcLinkResponse_securityGroupIds' - A list of security group IDs for the VPC link.
--
-- 'vpcLinkVersion', 'getVpcLinkResponse_vpcLinkVersion' - The version of the VPC link.
--
-- 'vpcLinkStatus', 'getVpcLinkResponse_vpcLinkStatus' - The status of the VPC link.
--
-- 'vpcLinkId', 'getVpcLinkResponse_vpcLinkId' - The ID of the VPC link.
--
-- 'createdDate', 'getVpcLinkResponse_createdDate' - The timestamp when the VPC link was created.
--
-- 'subnetIds', 'getVpcLinkResponse_subnetIds' - A list of subnet IDs to include in the VPC link.
--
-- 'httpStatus', 'getVpcLinkResponse_httpStatus' - The response's http status code.
newGetVpcLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVpcLinkResponse
newGetVpcLinkResponse pHttpStatus_ =
  GetVpcLinkResponse'
    { tags = Prelude.Nothing,
      vpcLinkStatusMessage = Prelude.Nothing,
      name = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      vpcLinkVersion = Prelude.Nothing,
      vpcLinkStatus = Prelude.Nothing,
      vpcLinkId = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Tags for the VPC link.
getVpcLinkResponse_tags :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getVpcLinkResponse_tags = Lens.lens (\GetVpcLinkResponse' {tags} -> tags) (\s@GetVpcLinkResponse' {} a -> s {tags = a} :: GetVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | A message summarizing the cause of the status of the VPC link.
getVpcLinkResponse_vpcLinkStatusMessage :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe Prelude.Text)
getVpcLinkResponse_vpcLinkStatusMessage = Lens.lens (\GetVpcLinkResponse' {vpcLinkStatusMessage} -> vpcLinkStatusMessage) (\s@GetVpcLinkResponse' {} a -> s {vpcLinkStatusMessage = a} :: GetVpcLinkResponse)

-- | The name of the VPC link.
getVpcLinkResponse_name :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe Prelude.Text)
getVpcLinkResponse_name = Lens.lens (\GetVpcLinkResponse' {name} -> name) (\s@GetVpcLinkResponse' {} a -> s {name = a} :: GetVpcLinkResponse)

-- | A list of security group IDs for the VPC link.
getVpcLinkResponse_securityGroupIds :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe [Prelude.Text])
getVpcLinkResponse_securityGroupIds = Lens.lens (\GetVpcLinkResponse' {securityGroupIds} -> securityGroupIds) (\s@GetVpcLinkResponse' {} a -> s {securityGroupIds = a} :: GetVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the VPC link.
getVpcLinkResponse_vpcLinkVersion :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe VpcLinkVersion)
getVpcLinkResponse_vpcLinkVersion = Lens.lens (\GetVpcLinkResponse' {vpcLinkVersion} -> vpcLinkVersion) (\s@GetVpcLinkResponse' {} a -> s {vpcLinkVersion = a} :: GetVpcLinkResponse)

-- | The status of the VPC link.
getVpcLinkResponse_vpcLinkStatus :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe VpcLinkStatus)
getVpcLinkResponse_vpcLinkStatus = Lens.lens (\GetVpcLinkResponse' {vpcLinkStatus} -> vpcLinkStatus) (\s@GetVpcLinkResponse' {} a -> s {vpcLinkStatus = a} :: GetVpcLinkResponse)

-- | The ID of the VPC link.
getVpcLinkResponse_vpcLinkId :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe Prelude.Text)
getVpcLinkResponse_vpcLinkId = Lens.lens (\GetVpcLinkResponse' {vpcLinkId} -> vpcLinkId) (\s@GetVpcLinkResponse' {} a -> s {vpcLinkId = a} :: GetVpcLinkResponse)

-- | The timestamp when the VPC link was created.
getVpcLinkResponse_createdDate :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe Prelude.UTCTime)
getVpcLinkResponse_createdDate = Lens.lens (\GetVpcLinkResponse' {createdDate} -> createdDate) (\s@GetVpcLinkResponse' {} a -> s {createdDate = a} :: GetVpcLinkResponse) Prelude.. Lens.mapping Core._Time

-- | A list of subnet IDs to include in the VPC link.
getVpcLinkResponse_subnetIds :: Lens.Lens' GetVpcLinkResponse (Prelude.Maybe [Prelude.Text])
getVpcLinkResponse_subnetIds = Lens.lens (\GetVpcLinkResponse' {subnetIds} -> subnetIds) (\s@GetVpcLinkResponse' {} a -> s {subnetIds = a} :: GetVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getVpcLinkResponse_httpStatus :: Lens.Lens' GetVpcLinkResponse Prelude.Int
getVpcLinkResponse_httpStatus = Lens.lens (\GetVpcLinkResponse' {httpStatus} -> httpStatus) (\s@GetVpcLinkResponse' {} a -> s {httpStatus = a} :: GetVpcLinkResponse)

instance Prelude.NFData GetVpcLinkResponse where
  rnf GetVpcLinkResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcLinkStatusMessage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf vpcLinkVersion
      `Prelude.seq` Prelude.rnf vpcLinkStatus
      `Prelude.seq` Prelude.rnf vpcLinkId
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf httpStatus

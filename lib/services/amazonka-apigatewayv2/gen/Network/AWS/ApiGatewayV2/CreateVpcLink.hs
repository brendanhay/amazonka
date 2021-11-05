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
-- Module      : Amazonka.ApiGatewayV2.CreateVpcLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC link.
module Amazonka.ApiGatewayV2.CreateVpcLink
  ( -- * Creating a Request
    CreateVpcLink (..),
    newCreateVpcLink,

    -- * Request Lenses
    createVpcLink_securityGroupIds,
    createVpcLink_tags,
    createVpcLink_subnetIds,
    createVpcLink_name,

    -- * Destructuring the Response
    CreateVpcLinkResponse (..),
    newCreateVpcLinkResponse,

    -- * Response Lenses
    createVpcLinkResponse_securityGroupIds,
    createVpcLinkResponse_subnetIds,
    createVpcLinkResponse_vpcLinkId,
    createVpcLinkResponse_createdDate,
    createVpcLinkResponse_vpcLinkVersion,
    createVpcLinkResponse_name,
    createVpcLinkResponse_vpcLinkStatusMessage,
    createVpcLinkResponse_tags,
    createVpcLinkResponse_vpcLinkStatus,
    createVpcLinkResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a VPC link
--
-- /See:/ 'newCreateVpcLink' smart constructor.
data CreateVpcLink = CreateVpcLink'
  { -- | A list of security group IDs for the VPC link.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of subnet IDs to include in the VPC link.
    subnetIds :: [Prelude.Text],
    -- | The name of the VPC link.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'createVpcLink_securityGroupIds' - A list of security group IDs for the VPC link.
--
-- 'tags', 'createVpcLink_tags' - A list of tags.
--
-- 'subnetIds', 'createVpcLink_subnetIds' - A list of subnet IDs to include in the VPC link.
--
-- 'name', 'createVpcLink_name' - The name of the VPC link.
newCreateVpcLink ::
  -- | 'name'
  Prelude.Text ->
  CreateVpcLink
newCreateVpcLink pName_ =
  CreateVpcLink'
    { securityGroupIds = Prelude.Nothing,
      tags = Prelude.Nothing,
      subnetIds = Prelude.mempty,
      name = pName_
    }

-- | A list of security group IDs for the VPC link.
createVpcLink_securityGroupIds :: Lens.Lens' CreateVpcLink (Prelude.Maybe [Prelude.Text])
createVpcLink_securityGroupIds = Lens.lens (\CreateVpcLink' {securityGroupIds} -> securityGroupIds) (\s@CreateVpcLink' {} a -> s {securityGroupIds = a} :: CreateVpcLink) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags.
createVpcLink_tags :: Lens.Lens' CreateVpcLink (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVpcLink_tags = Lens.lens (\CreateVpcLink' {tags} -> tags) (\s@CreateVpcLink' {} a -> s {tags = a} :: CreateVpcLink) Prelude.. Lens.mapping Lens.coerced

-- | A list of subnet IDs to include in the VPC link.
createVpcLink_subnetIds :: Lens.Lens' CreateVpcLink [Prelude.Text]
createVpcLink_subnetIds = Lens.lens (\CreateVpcLink' {subnetIds} -> subnetIds) (\s@CreateVpcLink' {} a -> s {subnetIds = a} :: CreateVpcLink) Prelude.. Lens.coerced

-- | The name of the VPC link.
createVpcLink_name :: Lens.Lens' CreateVpcLink Prelude.Text
createVpcLink_name = Lens.lens (\CreateVpcLink' {name} -> name) (\s@CreateVpcLink' {} a -> s {name = a} :: CreateVpcLink)

instance Core.AWSRequest CreateVpcLink where
  type
    AWSResponse CreateVpcLink =
      CreateVpcLinkResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcLinkResponse'
            Prelude.<$> ( x Core..?> "securityGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "subnetIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "vpcLinkId")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "vpcLinkVersion")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "vpcLinkStatusMessage")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "vpcLinkStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpcLink

instance Prelude.NFData CreateVpcLink

instance Core.ToHeaders CreateVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateVpcLink where
  toJSON CreateVpcLink' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("securityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("subnetIds" Core..= subnetIds),
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateVpcLink where
  toPath = Prelude.const "/v2/vpclinks"

instance Core.ToQuery CreateVpcLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcLinkResponse' smart constructor.
data CreateVpcLinkResponse = CreateVpcLinkResponse'
  { -- | A list of security group IDs for the VPC link.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of subnet IDs to include in the VPC link.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC link.
    vpcLinkId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the VPC link was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The version of the VPC link.
    vpcLinkVersion :: Prelude.Maybe VpcLinkVersion,
    -- | The name of the VPC link.
    name :: Prelude.Maybe Prelude.Text,
    -- | A message summarizing the cause of the status of the VPC link.
    vpcLinkStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | Tags for the VPC link.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The status of the VPC link.
    vpcLinkStatus :: Prelude.Maybe VpcLinkStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'createVpcLinkResponse_securityGroupIds' - A list of security group IDs for the VPC link.
--
-- 'subnetIds', 'createVpcLinkResponse_subnetIds' - A list of subnet IDs to include in the VPC link.
--
-- 'vpcLinkId', 'createVpcLinkResponse_vpcLinkId' - The ID of the VPC link.
--
-- 'createdDate', 'createVpcLinkResponse_createdDate' - The timestamp when the VPC link was created.
--
-- 'vpcLinkVersion', 'createVpcLinkResponse_vpcLinkVersion' - The version of the VPC link.
--
-- 'name', 'createVpcLinkResponse_name' - The name of the VPC link.
--
-- 'vpcLinkStatusMessage', 'createVpcLinkResponse_vpcLinkStatusMessage' - A message summarizing the cause of the status of the VPC link.
--
-- 'tags', 'createVpcLinkResponse_tags' - Tags for the VPC link.
--
-- 'vpcLinkStatus', 'createVpcLinkResponse_vpcLinkStatus' - The status of the VPC link.
--
-- 'httpStatus', 'createVpcLinkResponse_httpStatus' - The response's http status code.
newCreateVpcLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcLinkResponse
newCreateVpcLinkResponse pHttpStatus_ =
  CreateVpcLinkResponse'
    { securityGroupIds =
        Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcLinkId = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      vpcLinkVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      vpcLinkStatusMessage = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcLinkStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of security group IDs for the VPC link.
createVpcLinkResponse_securityGroupIds :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe [Prelude.Text])
createVpcLinkResponse_securityGroupIds = Lens.lens (\CreateVpcLinkResponse' {securityGroupIds} -> securityGroupIds) (\s@CreateVpcLinkResponse' {} a -> s {securityGroupIds = a} :: CreateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of subnet IDs to include in the VPC link.
createVpcLinkResponse_subnetIds :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe [Prelude.Text])
createVpcLinkResponse_subnetIds = Lens.lens (\CreateVpcLinkResponse' {subnetIds} -> subnetIds) (\s@CreateVpcLinkResponse' {} a -> s {subnetIds = a} :: CreateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC link.
createVpcLinkResponse_vpcLinkId :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe Prelude.Text)
createVpcLinkResponse_vpcLinkId = Lens.lens (\CreateVpcLinkResponse' {vpcLinkId} -> vpcLinkId) (\s@CreateVpcLinkResponse' {} a -> s {vpcLinkId = a} :: CreateVpcLinkResponse)

-- | The timestamp when the VPC link was created.
createVpcLinkResponse_createdDate :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe Prelude.UTCTime)
createVpcLinkResponse_createdDate = Lens.lens (\CreateVpcLinkResponse' {createdDate} -> createdDate) (\s@CreateVpcLinkResponse' {} a -> s {createdDate = a} :: CreateVpcLinkResponse) Prelude.. Lens.mapping Core._Time

-- | The version of the VPC link.
createVpcLinkResponse_vpcLinkVersion :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe VpcLinkVersion)
createVpcLinkResponse_vpcLinkVersion = Lens.lens (\CreateVpcLinkResponse' {vpcLinkVersion} -> vpcLinkVersion) (\s@CreateVpcLinkResponse' {} a -> s {vpcLinkVersion = a} :: CreateVpcLinkResponse)

-- | The name of the VPC link.
createVpcLinkResponse_name :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe Prelude.Text)
createVpcLinkResponse_name = Lens.lens (\CreateVpcLinkResponse' {name} -> name) (\s@CreateVpcLinkResponse' {} a -> s {name = a} :: CreateVpcLinkResponse)

-- | A message summarizing the cause of the status of the VPC link.
createVpcLinkResponse_vpcLinkStatusMessage :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe Prelude.Text)
createVpcLinkResponse_vpcLinkStatusMessage = Lens.lens (\CreateVpcLinkResponse' {vpcLinkStatusMessage} -> vpcLinkStatusMessage) (\s@CreateVpcLinkResponse' {} a -> s {vpcLinkStatusMessage = a} :: CreateVpcLinkResponse)

-- | Tags for the VPC link.
createVpcLinkResponse_tags :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVpcLinkResponse_tags = Lens.lens (\CreateVpcLinkResponse' {tags} -> tags) (\s@CreateVpcLinkResponse' {} a -> s {tags = a} :: CreateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the VPC link.
createVpcLinkResponse_vpcLinkStatus :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe VpcLinkStatus)
createVpcLinkResponse_vpcLinkStatus = Lens.lens (\CreateVpcLinkResponse' {vpcLinkStatus} -> vpcLinkStatus) (\s@CreateVpcLinkResponse' {} a -> s {vpcLinkStatus = a} :: CreateVpcLinkResponse)

-- | The response's http status code.
createVpcLinkResponse_httpStatus :: Lens.Lens' CreateVpcLinkResponse Prelude.Int
createVpcLinkResponse_httpStatus = Lens.lens (\CreateVpcLinkResponse' {httpStatus} -> httpStatus) (\s@CreateVpcLinkResponse' {} a -> s {httpStatus = a} :: CreateVpcLinkResponse)

instance Prelude.NFData CreateVpcLinkResponse

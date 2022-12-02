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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createVpcLink_tags,
    createVpcLink_securityGroupIds,
    createVpcLink_subnetIds,
    createVpcLink_name,

    -- * Destructuring the Response
    CreateVpcLinkResponse (..),
    newCreateVpcLinkResponse,

    -- * Response Lenses
    createVpcLinkResponse_tags,
    createVpcLinkResponse_vpcLinkStatusMessage,
    createVpcLinkResponse_name,
    createVpcLinkResponse_securityGroupIds,
    createVpcLinkResponse_vpcLinkVersion,
    createVpcLinkResponse_vpcLinkStatus,
    createVpcLinkResponse_vpcLinkId,
    createVpcLinkResponse_createdDate,
    createVpcLinkResponse_subnetIds,
    createVpcLinkResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a VPC link
--
-- /See:/ 'newCreateVpcLink' smart constructor.
data CreateVpcLink = CreateVpcLink'
  { -- | A list of tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of security group IDs for the VPC link.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
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
-- 'tags', 'createVpcLink_tags' - A list of tags.
--
-- 'securityGroupIds', 'createVpcLink_securityGroupIds' - A list of security group IDs for the VPC link.
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
    { tags = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      subnetIds = Prelude.mempty,
      name = pName_
    }

-- | A list of tags.
createVpcLink_tags :: Lens.Lens' CreateVpcLink (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVpcLink_tags = Lens.lens (\CreateVpcLink' {tags} -> tags) (\s@CreateVpcLink' {} a -> s {tags = a} :: CreateVpcLink) Prelude.. Lens.mapping Lens.coerced

-- | A list of security group IDs for the VPC link.
createVpcLink_securityGroupIds :: Lens.Lens' CreateVpcLink (Prelude.Maybe [Prelude.Text])
createVpcLink_securityGroupIds = Lens.lens (\CreateVpcLink' {securityGroupIds} -> securityGroupIds) (\s@CreateVpcLink' {} a -> s {securityGroupIds = a} :: CreateVpcLink) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcLinkResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vpcLinkStatusMessage")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> ( x Data..?> "securityGroupIds"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "vpcLinkVersion")
            Prelude.<*> (x Data..?> "vpcLinkStatus")
            Prelude.<*> (x Data..?> "vpcLinkId")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "subnetIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpcLink where
  hashWithSalt _salt CreateVpcLink' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateVpcLink where
  rnf CreateVpcLink' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateVpcLink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVpcLink where
  toJSON CreateVpcLink' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("securityGroupIds" Data..=)
              Prelude.<$> securityGroupIds,
            Prelude.Just ("subnetIds" Data..= subnetIds),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateVpcLink where
  toPath = Prelude.const "/v2/vpclinks"

instance Data.ToQuery CreateVpcLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcLinkResponse' smart constructor.
data CreateVpcLinkResponse = CreateVpcLinkResponse'
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
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | A list of subnet IDs to include in the VPC link.
    subnetIds :: Prelude.Maybe [Prelude.Text],
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
-- 'tags', 'createVpcLinkResponse_tags' - Tags for the VPC link.
--
-- 'vpcLinkStatusMessage', 'createVpcLinkResponse_vpcLinkStatusMessage' - A message summarizing the cause of the status of the VPC link.
--
-- 'name', 'createVpcLinkResponse_name' - The name of the VPC link.
--
-- 'securityGroupIds', 'createVpcLinkResponse_securityGroupIds' - A list of security group IDs for the VPC link.
--
-- 'vpcLinkVersion', 'createVpcLinkResponse_vpcLinkVersion' - The version of the VPC link.
--
-- 'vpcLinkStatus', 'createVpcLinkResponse_vpcLinkStatus' - The status of the VPC link.
--
-- 'vpcLinkId', 'createVpcLinkResponse_vpcLinkId' - The ID of the VPC link.
--
-- 'createdDate', 'createVpcLinkResponse_createdDate' - The timestamp when the VPC link was created.
--
-- 'subnetIds', 'createVpcLinkResponse_subnetIds' - A list of subnet IDs to include in the VPC link.
--
-- 'httpStatus', 'createVpcLinkResponse_httpStatus' - The response's http status code.
newCreateVpcLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcLinkResponse
newCreateVpcLinkResponse pHttpStatus_ =
  CreateVpcLinkResponse'
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
createVpcLinkResponse_tags :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVpcLinkResponse_tags = Lens.lens (\CreateVpcLinkResponse' {tags} -> tags) (\s@CreateVpcLinkResponse' {} a -> s {tags = a} :: CreateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | A message summarizing the cause of the status of the VPC link.
createVpcLinkResponse_vpcLinkStatusMessage :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe Prelude.Text)
createVpcLinkResponse_vpcLinkStatusMessage = Lens.lens (\CreateVpcLinkResponse' {vpcLinkStatusMessage} -> vpcLinkStatusMessage) (\s@CreateVpcLinkResponse' {} a -> s {vpcLinkStatusMessage = a} :: CreateVpcLinkResponse)

-- | The name of the VPC link.
createVpcLinkResponse_name :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe Prelude.Text)
createVpcLinkResponse_name = Lens.lens (\CreateVpcLinkResponse' {name} -> name) (\s@CreateVpcLinkResponse' {} a -> s {name = a} :: CreateVpcLinkResponse)

-- | A list of security group IDs for the VPC link.
createVpcLinkResponse_securityGroupIds :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe [Prelude.Text])
createVpcLinkResponse_securityGroupIds = Lens.lens (\CreateVpcLinkResponse' {securityGroupIds} -> securityGroupIds) (\s@CreateVpcLinkResponse' {} a -> s {securityGroupIds = a} :: CreateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the VPC link.
createVpcLinkResponse_vpcLinkVersion :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe VpcLinkVersion)
createVpcLinkResponse_vpcLinkVersion = Lens.lens (\CreateVpcLinkResponse' {vpcLinkVersion} -> vpcLinkVersion) (\s@CreateVpcLinkResponse' {} a -> s {vpcLinkVersion = a} :: CreateVpcLinkResponse)

-- | The status of the VPC link.
createVpcLinkResponse_vpcLinkStatus :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe VpcLinkStatus)
createVpcLinkResponse_vpcLinkStatus = Lens.lens (\CreateVpcLinkResponse' {vpcLinkStatus} -> vpcLinkStatus) (\s@CreateVpcLinkResponse' {} a -> s {vpcLinkStatus = a} :: CreateVpcLinkResponse)

-- | The ID of the VPC link.
createVpcLinkResponse_vpcLinkId :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe Prelude.Text)
createVpcLinkResponse_vpcLinkId = Lens.lens (\CreateVpcLinkResponse' {vpcLinkId} -> vpcLinkId) (\s@CreateVpcLinkResponse' {} a -> s {vpcLinkId = a} :: CreateVpcLinkResponse)

-- | The timestamp when the VPC link was created.
createVpcLinkResponse_createdDate :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe Prelude.UTCTime)
createVpcLinkResponse_createdDate = Lens.lens (\CreateVpcLinkResponse' {createdDate} -> createdDate) (\s@CreateVpcLinkResponse' {} a -> s {createdDate = a} :: CreateVpcLinkResponse) Prelude.. Lens.mapping Data._Time

-- | A list of subnet IDs to include in the VPC link.
createVpcLinkResponse_subnetIds :: Lens.Lens' CreateVpcLinkResponse (Prelude.Maybe [Prelude.Text])
createVpcLinkResponse_subnetIds = Lens.lens (\CreateVpcLinkResponse' {subnetIds} -> subnetIds) (\s@CreateVpcLinkResponse' {} a -> s {subnetIds = a} :: CreateVpcLinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createVpcLinkResponse_httpStatus :: Lens.Lens' CreateVpcLinkResponse Prelude.Int
createVpcLinkResponse_httpStatus = Lens.lens (\CreateVpcLinkResponse' {httpStatus} -> httpStatus) (\s@CreateVpcLinkResponse' {} a -> s {httpStatus = a} :: CreateVpcLinkResponse)

instance Prelude.NFData CreateVpcLinkResponse where
  rnf CreateVpcLinkResponse' {..} =
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

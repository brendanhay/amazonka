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
-- Module      : Amazonka.NetworkManager.CreateVpcAttachment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC attachment on an edge location of a core network.
module Amazonka.NetworkManager.CreateVpcAttachment
  ( -- * Creating a Request
    CreateVpcAttachment (..),
    newCreateVpcAttachment,

    -- * Request Lenses
    createVpcAttachment_tags,
    createVpcAttachment_clientToken,
    createVpcAttachment_options,
    createVpcAttachment_coreNetworkId,
    createVpcAttachment_vpcArn,
    createVpcAttachment_subnetArns,

    -- * Destructuring the Response
    CreateVpcAttachmentResponse (..),
    newCreateVpcAttachmentResponse,

    -- * Response Lenses
    createVpcAttachmentResponse_vpcAttachment,
    createVpcAttachmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVpcAttachment' smart constructor.
data CreateVpcAttachment = CreateVpcAttachment'
  { -- | The key-value tags associated with the request.
    tags :: Prelude.Maybe [Tag],
    -- | The client token associated with the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Options for the VPC attachment.
    options :: Prelude.Maybe VpcOptions,
    -- | The ID of a core network for the VPC attachment.
    coreNetworkId :: Prelude.Text,
    -- | The ARN of the VPC.
    vpcArn :: Prelude.Text,
    -- | The subnet ARN of the VPC attachment.
    subnetArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVpcAttachment_tags' - The key-value tags associated with the request.
--
-- 'clientToken', 'createVpcAttachment_clientToken' - The client token associated with the request.
--
-- 'options', 'createVpcAttachment_options' - Options for the VPC attachment.
--
-- 'coreNetworkId', 'createVpcAttachment_coreNetworkId' - The ID of a core network for the VPC attachment.
--
-- 'vpcArn', 'createVpcAttachment_vpcArn' - The ARN of the VPC.
--
-- 'subnetArns', 'createVpcAttachment_subnetArns' - The subnet ARN of the VPC attachment.
newCreateVpcAttachment ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'vpcArn'
  Prelude.Text ->
  CreateVpcAttachment
newCreateVpcAttachment pCoreNetworkId_ pVpcArn_ =
  CreateVpcAttachment'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      options = Prelude.Nothing,
      coreNetworkId = pCoreNetworkId_,
      vpcArn = pVpcArn_,
      subnetArns = Prelude.mempty
    }

-- | The key-value tags associated with the request.
createVpcAttachment_tags :: Lens.Lens' CreateVpcAttachment (Prelude.Maybe [Tag])
createVpcAttachment_tags = Lens.lens (\CreateVpcAttachment' {tags} -> tags) (\s@CreateVpcAttachment' {} a -> s {tags = a} :: CreateVpcAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The client token associated with the request.
createVpcAttachment_clientToken :: Lens.Lens' CreateVpcAttachment (Prelude.Maybe Prelude.Text)
createVpcAttachment_clientToken = Lens.lens (\CreateVpcAttachment' {clientToken} -> clientToken) (\s@CreateVpcAttachment' {} a -> s {clientToken = a} :: CreateVpcAttachment)

-- | Options for the VPC attachment.
createVpcAttachment_options :: Lens.Lens' CreateVpcAttachment (Prelude.Maybe VpcOptions)
createVpcAttachment_options = Lens.lens (\CreateVpcAttachment' {options} -> options) (\s@CreateVpcAttachment' {} a -> s {options = a} :: CreateVpcAttachment)

-- | The ID of a core network for the VPC attachment.
createVpcAttachment_coreNetworkId :: Lens.Lens' CreateVpcAttachment Prelude.Text
createVpcAttachment_coreNetworkId = Lens.lens (\CreateVpcAttachment' {coreNetworkId} -> coreNetworkId) (\s@CreateVpcAttachment' {} a -> s {coreNetworkId = a} :: CreateVpcAttachment)

-- | The ARN of the VPC.
createVpcAttachment_vpcArn :: Lens.Lens' CreateVpcAttachment Prelude.Text
createVpcAttachment_vpcArn = Lens.lens (\CreateVpcAttachment' {vpcArn} -> vpcArn) (\s@CreateVpcAttachment' {} a -> s {vpcArn = a} :: CreateVpcAttachment)

-- | The subnet ARN of the VPC attachment.
createVpcAttachment_subnetArns :: Lens.Lens' CreateVpcAttachment [Prelude.Text]
createVpcAttachment_subnetArns = Lens.lens (\CreateVpcAttachment' {subnetArns} -> subnetArns) (\s@CreateVpcAttachment' {} a -> s {subnetArns = a} :: CreateVpcAttachment) Prelude.. Lens.coerced

instance Core.AWSRequest CreateVpcAttachment where
  type
    AWSResponse CreateVpcAttachment =
      CreateVpcAttachmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVpcAttachmentResponse'
            Prelude.<$> (x Data..?> "VpcAttachment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVpcAttachment where
  hashWithSalt _salt CreateVpcAttachment' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` vpcArn
      `Prelude.hashWithSalt` subnetArns

instance Prelude.NFData CreateVpcAttachment where
  rnf CreateVpcAttachment' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf vpcArn
      `Prelude.seq` Prelude.rnf subnetArns

instance Data.ToHeaders CreateVpcAttachment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVpcAttachment where
  toJSON CreateVpcAttachment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Options" Data..=) Prelude.<$> options,
            Prelude.Just ("CoreNetworkId" Data..= coreNetworkId),
            Prelude.Just ("VpcArn" Data..= vpcArn),
            Prelude.Just ("SubnetArns" Data..= subnetArns)
          ]
      )

instance Data.ToPath CreateVpcAttachment where
  toPath = Prelude.const "/vpc-attachments"

instance Data.ToQuery CreateVpcAttachment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVpcAttachmentResponse' smart constructor.
data CreateVpcAttachmentResponse = CreateVpcAttachmentResponse'
  { -- | Provides details about the VPC attachment.
    vpcAttachment :: Prelude.Maybe VpcAttachment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcAttachmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcAttachment', 'createVpcAttachmentResponse_vpcAttachment' - Provides details about the VPC attachment.
--
-- 'httpStatus', 'createVpcAttachmentResponse_httpStatus' - The response's http status code.
newCreateVpcAttachmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcAttachmentResponse
newCreateVpcAttachmentResponse pHttpStatus_ =
  CreateVpcAttachmentResponse'
    { vpcAttachment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides details about the VPC attachment.
createVpcAttachmentResponse_vpcAttachment :: Lens.Lens' CreateVpcAttachmentResponse (Prelude.Maybe VpcAttachment)
createVpcAttachmentResponse_vpcAttachment = Lens.lens (\CreateVpcAttachmentResponse' {vpcAttachment} -> vpcAttachment) (\s@CreateVpcAttachmentResponse' {} a -> s {vpcAttachment = a} :: CreateVpcAttachmentResponse)

-- | The response's http status code.
createVpcAttachmentResponse_httpStatus :: Lens.Lens' CreateVpcAttachmentResponse Prelude.Int
createVpcAttachmentResponse_httpStatus = Lens.lens (\CreateVpcAttachmentResponse' {httpStatus} -> httpStatus) (\s@CreateVpcAttachmentResponse' {} a -> s {httpStatus = a} :: CreateVpcAttachmentResponse)

instance Prelude.NFData CreateVpcAttachmentResponse where
  rnf CreateVpcAttachmentResponse' {..} =
    Prelude.rnf vpcAttachment
      `Prelude.seq` Prelude.rnf httpStatus

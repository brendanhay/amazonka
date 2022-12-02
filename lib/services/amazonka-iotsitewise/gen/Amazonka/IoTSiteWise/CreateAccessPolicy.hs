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
-- Module      : Amazonka.IoTSiteWise.CreateAccessPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an access policy that grants the specified identity (IAM
-- Identity Center user, IAM Identity Center group, or IAM user) access to
-- the specified IoT SiteWise Monitor portal or project resource.
module Amazonka.IoTSiteWise.CreateAccessPolicy
  ( -- * Creating a Request
    CreateAccessPolicy (..),
    newCreateAccessPolicy,

    -- * Request Lenses
    createAccessPolicy_tags,
    createAccessPolicy_clientToken,
    createAccessPolicy_accessPolicyIdentity,
    createAccessPolicy_accessPolicyResource,
    createAccessPolicy_accessPolicyPermission,

    -- * Destructuring the Response
    CreateAccessPolicyResponse (..),
    newCreateAccessPolicyResponse,

    -- * Response Lenses
    createAccessPolicyResponse_httpStatus,
    createAccessPolicyResponse_accessPolicyId,
    createAccessPolicyResponse_accessPolicyArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccessPolicy' smart constructor.
data CreateAccessPolicy = CreateAccessPolicy'
  { -- | A list of key-value pairs that contain metadata for the access policy.
    -- For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
    -- in the /IoT SiteWise User Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The identity for this access policy. Choose an IAM Identity Center user,
    -- an IAM Identity Center group, or an IAM user.
    accessPolicyIdentity :: Identity,
    -- | The IoT SiteWise Monitor resource for this access policy. Choose either
    -- a portal or a project.
    accessPolicyResource :: Resource,
    -- | The permission level for this access policy. Note that a project
    -- @ADMINISTRATOR@ is also known as a project owner.
    accessPolicyPermission :: Permission
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAccessPolicy_tags' - A list of key-value pairs that contain metadata for the access policy.
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
--
-- 'clientToken', 'createAccessPolicy_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'accessPolicyIdentity', 'createAccessPolicy_accessPolicyIdentity' - The identity for this access policy. Choose an IAM Identity Center user,
-- an IAM Identity Center group, or an IAM user.
--
-- 'accessPolicyResource', 'createAccessPolicy_accessPolicyResource' - The IoT SiteWise Monitor resource for this access policy. Choose either
-- a portal or a project.
--
-- 'accessPolicyPermission', 'createAccessPolicy_accessPolicyPermission' - The permission level for this access policy. Note that a project
-- @ADMINISTRATOR@ is also known as a project owner.
newCreateAccessPolicy ::
  -- | 'accessPolicyIdentity'
  Identity ->
  -- | 'accessPolicyResource'
  Resource ->
  -- | 'accessPolicyPermission'
  Permission ->
  CreateAccessPolicy
newCreateAccessPolicy
  pAccessPolicyIdentity_
  pAccessPolicyResource_
  pAccessPolicyPermission_ =
    CreateAccessPolicy'
      { tags = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        accessPolicyIdentity = pAccessPolicyIdentity_,
        accessPolicyResource = pAccessPolicyResource_,
        accessPolicyPermission = pAccessPolicyPermission_
      }

-- | A list of key-value pairs that contain metadata for the access policy.
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/tag-resources.html Tagging your IoT SiteWise resources>
-- in the /IoT SiteWise User Guide/.
createAccessPolicy_tags :: Lens.Lens' CreateAccessPolicy (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAccessPolicy_tags = Lens.lens (\CreateAccessPolicy' {tags} -> tags) (\s@CreateAccessPolicy' {} a -> s {tags = a} :: CreateAccessPolicy) Prelude.. Lens.mapping Lens.coerced

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
createAccessPolicy_clientToken :: Lens.Lens' CreateAccessPolicy (Prelude.Maybe Prelude.Text)
createAccessPolicy_clientToken = Lens.lens (\CreateAccessPolicy' {clientToken} -> clientToken) (\s@CreateAccessPolicy' {} a -> s {clientToken = a} :: CreateAccessPolicy)

-- | The identity for this access policy. Choose an IAM Identity Center user,
-- an IAM Identity Center group, or an IAM user.
createAccessPolicy_accessPolicyIdentity :: Lens.Lens' CreateAccessPolicy Identity
createAccessPolicy_accessPolicyIdentity = Lens.lens (\CreateAccessPolicy' {accessPolicyIdentity} -> accessPolicyIdentity) (\s@CreateAccessPolicy' {} a -> s {accessPolicyIdentity = a} :: CreateAccessPolicy)

-- | The IoT SiteWise Monitor resource for this access policy. Choose either
-- a portal or a project.
createAccessPolicy_accessPolicyResource :: Lens.Lens' CreateAccessPolicy Resource
createAccessPolicy_accessPolicyResource = Lens.lens (\CreateAccessPolicy' {accessPolicyResource} -> accessPolicyResource) (\s@CreateAccessPolicy' {} a -> s {accessPolicyResource = a} :: CreateAccessPolicy)

-- | The permission level for this access policy. Note that a project
-- @ADMINISTRATOR@ is also known as a project owner.
createAccessPolicy_accessPolicyPermission :: Lens.Lens' CreateAccessPolicy Permission
createAccessPolicy_accessPolicyPermission = Lens.lens (\CreateAccessPolicy' {accessPolicyPermission} -> accessPolicyPermission) (\s@CreateAccessPolicy' {} a -> s {accessPolicyPermission = a} :: CreateAccessPolicy)

instance Core.AWSRequest CreateAccessPolicy where
  type
    AWSResponse CreateAccessPolicy =
      CreateAccessPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccessPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "accessPolicyId")
            Prelude.<*> (x Data..:> "accessPolicyArn")
      )

instance Prelude.Hashable CreateAccessPolicy where
  hashWithSalt _salt CreateAccessPolicy' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` accessPolicyIdentity
      `Prelude.hashWithSalt` accessPolicyResource
      `Prelude.hashWithSalt` accessPolicyPermission

instance Prelude.NFData CreateAccessPolicy where
  rnf CreateAccessPolicy' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf accessPolicyIdentity
      `Prelude.seq` Prelude.rnf accessPolicyResource
      `Prelude.seq` Prelude.rnf accessPolicyPermission

instance Data.ToHeaders CreateAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccessPolicy where
  toJSON CreateAccessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "accessPolicyIdentity"
                  Data..= accessPolicyIdentity
              ),
            Prelude.Just
              ( "accessPolicyResource"
                  Data..= accessPolicyResource
              ),
            Prelude.Just
              ( "accessPolicyPermission"
                  Data..= accessPolicyPermission
              )
          ]
      )

instance Data.ToPath CreateAccessPolicy where
  toPath = Prelude.const "/access-policies"

instance Data.ToQuery CreateAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccessPolicyResponse' smart constructor.
data CreateAccessPolicyResponse = CreateAccessPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the access policy.
    accessPolicyId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the access policy, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:access-policy\/${AccessPolicyId}@
    accessPolicyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAccessPolicyResponse_httpStatus' - The response's http status code.
--
-- 'accessPolicyId', 'createAccessPolicyResponse_accessPolicyId' - The ID of the access policy.
--
-- 'accessPolicyArn', 'createAccessPolicyResponse_accessPolicyArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the access policy, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:access-policy\/${AccessPolicyId}@
newCreateAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accessPolicyId'
  Prelude.Text ->
  -- | 'accessPolicyArn'
  Prelude.Text ->
  CreateAccessPolicyResponse
newCreateAccessPolicyResponse
  pHttpStatus_
  pAccessPolicyId_
  pAccessPolicyArn_ =
    CreateAccessPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        accessPolicyId = pAccessPolicyId_,
        accessPolicyArn = pAccessPolicyArn_
      }

-- | The response's http status code.
createAccessPolicyResponse_httpStatus :: Lens.Lens' CreateAccessPolicyResponse Prelude.Int
createAccessPolicyResponse_httpStatus = Lens.lens (\CreateAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateAccessPolicyResponse' {} a -> s {httpStatus = a} :: CreateAccessPolicyResponse)

-- | The ID of the access policy.
createAccessPolicyResponse_accessPolicyId :: Lens.Lens' CreateAccessPolicyResponse Prelude.Text
createAccessPolicyResponse_accessPolicyId = Lens.lens (\CreateAccessPolicyResponse' {accessPolicyId} -> accessPolicyId) (\s@CreateAccessPolicyResponse' {} a -> s {accessPolicyId = a} :: CreateAccessPolicyResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the access policy, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:access-policy\/${AccessPolicyId}@
createAccessPolicyResponse_accessPolicyArn :: Lens.Lens' CreateAccessPolicyResponse Prelude.Text
createAccessPolicyResponse_accessPolicyArn = Lens.lens (\CreateAccessPolicyResponse' {accessPolicyArn} -> accessPolicyArn) (\s@CreateAccessPolicyResponse' {} a -> s {accessPolicyArn = a} :: CreateAccessPolicyResponse)

instance Prelude.NFData CreateAccessPolicyResponse where
  rnf CreateAccessPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accessPolicyId
      `Prelude.seq` Prelude.rnf accessPolicyArn

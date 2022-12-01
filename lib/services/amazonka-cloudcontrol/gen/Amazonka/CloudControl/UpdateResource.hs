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
-- Module      : Amazonka.CloudControl.UpdateResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified property values in the resource.
--
-- You specify your resource property updates as a list of patch operations
-- contained in a JSON patch document that adheres to the
-- <https://datatracker.ietf.org/doc/html/rfc6902 RFC 6902 - JavaScript Object Notation (JSON) Patch>
-- standard.
--
-- For details on how Cloud Control API performs resource update
-- operations, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-update.html Updating a resource>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- After you have initiated a resource update request, you can monitor the
-- progress of your request by calling
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- using the @RequestToken@ of the @ProgressEvent@ returned by
-- @UpdateResource@.
--
-- For more information about the properties of a specific resource, refer
-- to the related topic for the resource in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Resource and property types reference>
-- in the /CloudFormation Users Guide/.
module Amazonka.CloudControl.UpdateResource
  ( -- * Creating a Request
    UpdateResource (..),
    newUpdateResource,

    -- * Request Lenses
    updateResource_clientToken,
    updateResource_roleArn,
    updateResource_typeVersionId,
    updateResource_typeName,
    updateResource_identifier,
    updateResource_patchDocument,

    -- * Destructuring the Response
    UpdateResourceResponse (..),
    newUpdateResourceResponse,

    -- * Response Lenses
    updateResourceResponse_progressEvent,
    updateResourceResponse_httpStatus,
  )
where

import Amazonka.CloudControl.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResource' smart constructor.
data UpdateResource = UpdateResource'
  { -- | A unique identifier to ensure the idempotency of the resource request.
    -- As a best practice, specify this token to ensure idempotency, so that
    -- Amazon Web Services Cloud Control API can accurately distinguish between
    -- request retries and new resource requests. You might retry a resource
    -- request to ensure that it was successfully received.
    --
    -- A client token is valid for 36 hours once used. After that, a resource
    -- request with the same client token is treated as a new request.
    --
    -- If you do not specify a client token, one is generated for inclusion in
    -- the request.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations.html#resource-operations-idempotency Ensuring resource operation requests are unique>
    -- in the /Amazon Web Services Cloud Control API User Guide/.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role for Cloud Control API to use when performing this resource
    -- operation. The role specified must have the permissions required for
    -- this operation. The necessary permissions for each event handler are
    -- defined in the @ handlers @ section of the
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html resource type definition schema>.
    --
    -- If you do not specify a role, Cloud Control API uses a temporary session
    -- created using your Amazon Web Services user credentials.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations.html#resource-operations-permissions Specifying credentials>
    -- in the /Amazon Web Services Cloud Control API User Guide/.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | For private resource types, the type version to use in this resource
    -- operation. If you do not specify a resource version, CloudFormation uses
    -- the default version.
    typeVersionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource type.
    typeName :: Prelude.Text,
    -- | The identifier for the resource.
    --
    -- You can specify the primary identifier, or any secondary identifier
    -- defined for the resource type in its resource schema. You can only
    -- specify one identifier. Primary identifiers can be specified as a string
    -- or JSON; secondary identifiers must be specified as JSON.
    --
    -- For compound primary identifiers (that is, one that consists of multiple
    -- resource properties strung together), to specify the primary identifier
    -- as a string, list the property values /in the order they are specified/
    -- in the primary identifier definition, separated by @|@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-identifier.html Identifying resources>
    -- in the /Amazon Web Services Cloud Control API User Guide/.
    identifier :: Prelude.Text,
    -- | A JavaScript Object Notation (JSON) document listing the patch
    -- operations that represent the updates to apply to the current resource
    -- properties. For details, see
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-update.html#resource-operations-update-patch Composing the patch document>
    -- in the /Amazon Web Services Cloud Control API User Guide/.
    patchDocument :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateResource_clientToken' - A unique identifier to ensure the idempotency of the resource request.
-- As a best practice, specify this token to ensure idempotency, so that
-- Amazon Web Services Cloud Control API can accurately distinguish between
-- request retries and new resource requests. You might retry a resource
-- request to ensure that it was successfully received.
--
-- A client token is valid for 36 hours once used. After that, a resource
-- request with the same client token is treated as a new request.
--
-- If you do not specify a client token, one is generated for inclusion in
-- the request.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations.html#resource-operations-idempotency Ensuring resource operation requests are unique>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- 'roleArn', 'updateResource_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role for Cloud Control API to use when performing this resource
-- operation. The role specified must have the permissions required for
-- this operation. The necessary permissions for each event handler are
-- defined in the @ handlers @ section of the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html resource type definition schema>.
--
-- If you do not specify a role, Cloud Control API uses a temporary session
-- created using your Amazon Web Services user credentials.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations.html#resource-operations-permissions Specifying credentials>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- 'typeVersionId', 'updateResource_typeVersionId' - For private resource types, the type version to use in this resource
-- operation. If you do not specify a resource version, CloudFormation uses
-- the default version.
--
-- 'typeName', 'updateResource_typeName' - The name of the resource type.
--
-- 'identifier', 'updateResource_identifier' - The identifier for the resource.
--
-- You can specify the primary identifier, or any secondary identifier
-- defined for the resource type in its resource schema. You can only
-- specify one identifier. Primary identifiers can be specified as a string
-- or JSON; secondary identifiers must be specified as JSON.
--
-- For compound primary identifiers (that is, one that consists of multiple
-- resource properties strung together), to specify the primary identifier
-- as a string, list the property values /in the order they are specified/
-- in the primary identifier definition, separated by @|@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-identifier.html Identifying resources>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- 'patchDocument', 'updateResource_patchDocument' - A JavaScript Object Notation (JSON) document listing the patch
-- operations that represent the updates to apply to the current resource
-- properties. For details, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-update.html#resource-operations-update-patch Composing the patch document>
-- in the /Amazon Web Services Cloud Control API User Guide/.
newUpdateResource ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'identifier'
  Prelude.Text ->
  -- | 'patchDocument'
  Prelude.Text ->
  UpdateResource
newUpdateResource
  pTypeName_
  pIdentifier_
  pPatchDocument_ =
    UpdateResource'
      { clientToken = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        typeVersionId = Prelude.Nothing,
        typeName = pTypeName_,
        identifier = pIdentifier_,
        patchDocument =
          Core._Sensitive Lens.# pPatchDocument_
      }

-- | A unique identifier to ensure the idempotency of the resource request.
-- As a best practice, specify this token to ensure idempotency, so that
-- Amazon Web Services Cloud Control API can accurately distinguish between
-- request retries and new resource requests. You might retry a resource
-- request to ensure that it was successfully received.
--
-- A client token is valid for 36 hours once used. After that, a resource
-- request with the same client token is treated as a new request.
--
-- If you do not specify a client token, one is generated for inclusion in
-- the request.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations.html#resource-operations-idempotency Ensuring resource operation requests are unique>
-- in the /Amazon Web Services Cloud Control API User Guide/.
updateResource_clientToken :: Lens.Lens' UpdateResource (Prelude.Maybe Prelude.Text)
updateResource_clientToken = Lens.lens (\UpdateResource' {clientToken} -> clientToken) (\s@UpdateResource' {} a -> s {clientToken = a} :: UpdateResource)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role for Cloud Control API to use when performing this resource
-- operation. The role specified must have the permissions required for
-- this operation. The necessary permissions for each event handler are
-- defined in the @ handlers @ section of the
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html resource type definition schema>.
--
-- If you do not specify a role, Cloud Control API uses a temporary session
-- created using your Amazon Web Services user credentials.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations.html#resource-operations-permissions Specifying credentials>
-- in the /Amazon Web Services Cloud Control API User Guide/.
updateResource_roleArn :: Lens.Lens' UpdateResource (Prelude.Maybe Prelude.Text)
updateResource_roleArn = Lens.lens (\UpdateResource' {roleArn} -> roleArn) (\s@UpdateResource' {} a -> s {roleArn = a} :: UpdateResource)

-- | For private resource types, the type version to use in this resource
-- operation. If you do not specify a resource version, CloudFormation uses
-- the default version.
updateResource_typeVersionId :: Lens.Lens' UpdateResource (Prelude.Maybe Prelude.Text)
updateResource_typeVersionId = Lens.lens (\UpdateResource' {typeVersionId} -> typeVersionId) (\s@UpdateResource' {} a -> s {typeVersionId = a} :: UpdateResource)

-- | The name of the resource type.
updateResource_typeName :: Lens.Lens' UpdateResource Prelude.Text
updateResource_typeName = Lens.lens (\UpdateResource' {typeName} -> typeName) (\s@UpdateResource' {} a -> s {typeName = a} :: UpdateResource)

-- | The identifier for the resource.
--
-- You can specify the primary identifier, or any secondary identifier
-- defined for the resource type in its resource schema. You can only
-- specify one identifier. Primary identifiers can be specified as a string
-- or JSON; secondary identifiers must be specified as JSON.
--
-- For compound primary identifiers (that is, one that consists of multiple
-- resource properties strung together), to specify the primary identifier
-- as a string, list the property values /in the order they are specified/
-- in the primary identifier definition, separated by @|@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-identifier.html Identifying resources>
-- in the /Amazon Web Services Cloud Control API User Guide/.
updateResource_identifier :: Lens.Lens' UpdateResource Prelude.Text
updateResource_identifier = Lens.lens (\UpdateResource' {identifier} -> identifier) (\s@UpdateResource' {} a -> s {identifier = a} :: UpdateResource)

-- | A JavaScript Object Notation (JSON) document listing the patch
-- operations that represent the updates to apply to the current resource
-- properties. For details, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-update.html#resource-operations-update-patch Composing the patch document>
-- in the /Amazon Web Services Cloud Control API User Guide/.
updateResource_patchDocument :: Lens.Lens' UpdateResource Prelude.Text
updateResource_patchDocument = Lens.lens (\UpdateResource' {patchDocument} -> patchDocument) (\s@UpdateResource' {} a -> s {patchDocument = a} :: UpdateResource) Prelude.. Core._Sensitive

instance Core.AWSRequest UpdateResource where
  type
    AWSResponse UpdateResource =
      UpdateResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourceResponse'
            Prelude.<$> (x Core..?> "ProgressEvent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResource where
  hashWithSalt _salt UpdateResource' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` typeVersionId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` patchDocument

instance Prelude.NFData UpdateResource where
  rnf UpdateResource' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf typeVersionId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf patchDocument

instance Core.ToHeaders UpdateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudApiService.UpdateResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateResource where
  toJSON UpdateResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("TypeVersionId" Core..=) Prelude.<$> typeVersionId,
            Prelude.Just ("TypeName" Core..= typeName),
            Prelude.Just ("Identifier" Core..= identifier),
            Prelude.Just
              ("PatchDocument" Core..= patchDocument)
          ]
      )

instance Core.ToPath UpdateResource where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceResponse' smart constructor.
data UpdateResourceResponse = UpdateResourceResponse'
  { -- | Represents the current status of the resource update request.
    --
    -- Use the @RequestToken@ of the @ProgressEvent@ with
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
    -- to return the current status of a resource operation request.
    progressEvent :: Prelude.Maybe ProgressEvent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressEvent', 'updateResourceResponse_progressEvent' - Represents the current status of the resource update request.
--
-- Use the @RequestToken@ of the @ProgressEvent@ with
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- to return the current status of a resource operation request.
--
-- 'httpStatus', 'updateResourceResponse_httpStatus' - The response's http status code.
newUpdateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceResponse
newUpdateResourceResponse pHttpStatus_ =
  UpdateResourceResponse'
    { progressEvent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the current status of the resource update request.
--
-- Use the @RequestToken@ of the @ProgressEvent@ with
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- to return the current status of a resource operation request.
updateResourceResponse_progressEvent :: Lens.Lens' UpdateResourceResponse (Prelude.Maybe ProgressEvent)
updateResourceResponse_progressEvent = Lens.lens (\UpdateResourceResponse' {progressEvent} -> progressEvent) (\s@UpdateResourceResponse' {} a -> s {progressEvent = a} :: UpdateResourceResponse)

-- | The response's http status code.
updateResourceResponse_httpStatus :: Lens.Lens' UpdateResourceResponse Prelude.Int
updateResourceResponse_httpStatus = Lens.lens (\UpdateResourceResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceResponse' {} a -> s {httpStatus = a} :: UpdateResourceResponse)

instance Prelude.NFData UpdateResourceResponse where
  rnf UpdateResourceResponse' {..} =
    Prelude.rnf progressEvent
      `Prelude.seq` Prelude.rnf httpStatus

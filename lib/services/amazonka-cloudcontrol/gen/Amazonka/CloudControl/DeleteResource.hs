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
-- Module      : Amazonka.CloudControl.DeleteResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource. For details, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-delete.html Deleting a resource>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- After you have initiated a resource deletion request, you can monitor
-- the progress of your request by calling
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- using the @RequestToken@ of the @ProgressEvent@ returned by
-- @DeleteResource@.
module Amazonka.CloudControl.DeleteResource
  ( -- * Creating a Request
    DeleteResource (..),
    newDeleteResource,

    -- * Request Lenses
    deleteResource_clientToken,
    deleteResource_roleArn,
    deleteResource_typeVersionId,
    deleteResource_typeName,
    deleteResource_identifier,

    -- * Destructuring the Response
    DeleteResourceResponse (..),
    newDeleteResourceResponse,

    -- * Response Lenses
    deleteResourceResponse_progressEvent,
    deleteResourceResponse_httpStatus,
  )
where

import Amazonka.CloudControl.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
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
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteResource_clientToken' - A unique identifier to ensure the idempotency of the resource request.
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
-- 'roleArn', 'deleteResource_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
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
-- 'typeVersionId', 'deleteResource_typeVersionId' - For private resource types, the type version to use in this resource
-- operation. If you do not specify a resource version, CloudFormation uses
-- the default version.
--
-- 'typeName', 'deleteResource_typeName' - The name of the resource type.
--
-- 'identifier', 'deleteResource_identifier' - The identifier for the resource.
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
newDeleteResource ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'identifier'
  Prelude.Text ->
  DeleteResource
newDeleteResource pTypeName_ pIdentifier_ =
  DeleteResource'
    { clientToken = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      typeVersionId = Prelude.Nothing,
      typeName = pTypeName_,
      identifier = pIdentifier_
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
deleteResource_clientToken :: Lens.Lens' DeleteResource (Prelude.Maybe Prelude.Text)
deleteResource_clientToken = Lens.lens (\DeleteResource' {clientToken} -> clientToken) (\s@DeleteResource' {} a -> s {clientToken = a} :: DeleteResource)

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
deleteResource_roleArn :: Lens.Lens' DeleteResource (Prelude.Maybe Prelude.Text)
deleteResource_roleArn = Lens.lens (\DeleteResource' {roleArn} -> roleArn) (\s@DeleteResource' {} a -> s {roleArn = a} :: DeleteResource)

-- | For private resource types, the type version to use in this resource
-- operation. If you do not specify a resource version, CloudFormation uses
-- the default version.
deleteResource_typeVersionId :: Lens.Lens' DeleteResource (Prelude.Maybe Prelude.Text)
deleteResource_typeVersionId = Lens.lens (\DeleteResource' {typeVersionId} -> typeVersionId) (\s@DeleteResource' {} a -> s {typeVersionId = a} :: DeleteResource)

-- | The name of the resource type.
deleteResource_typeName :: Lens.Lens' DeleteResource Prelude.Text
deleteResource_typeName = Lens.lens (\DeleteResource' {typeName} -> typeName) (\s@DeleteResource' {} a -> s {typeName = a} :: DeleteResource)

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
deleteResource_identifier :: Lens.Lens' DeleteResource Prelude.Text
deleteResource_identifier = Lens.lens (\DeleteResource' {identifier} -> identifier) (\s@DeleteResource' {} a -> s {identifier = a} :: DeleteResource)

instance Core.AWSRequest DeleteResource where
  type
    AWSResponse DeleteResource =
      DeleteResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResourceResponse'
            Prelude.<$> (x Data..?> "ProgressEvent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResource where
  hashWithSalt _salt DeleteResource' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` typeVersionId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeleteResource where
  rnf DeleteResource' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf typeVersionId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToHeaders DeleteResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudApiService.DeleteResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResource where
  toJSON DeleteResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("TypeVersionId" Data..=) Prelude.<$> typeVersionId,
            Prelude.Just ("TypeName" Data..= typeName),
            Prelude.Just ("Identifier" Data..= identifier)
          ]
      )

instance Data.ToPath DeleteResource where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceResponse' smart constructor.
data DeleteResourceResponse = DeleteResourceResponse'
  { -- | Represents the current status of the resource deletion request.
    --
    -- After you have initiated a resource deletion request, you can monitor
    -- the progress of your request by calling
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
    -- using the @RequestToken@ of the @ProgressEvent@ returned by
    -- @DeleteResource@.
    progressEvent :: Prelude.Maybe ProgressEvent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressEvent', 'deleteResourceResponse_progressEvent' - Represents the current status of the resource deletion request.
--
-- After you have initiated a resource deletion request, you can monitor
-- the progress of your request by calling
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- using the @RequestToken@ of the @ProgressEvent@ returned by
-- @DeleteResource@.
--
-- 'httpStatus', 'deleteResourceResponse_httpStatus' - The response's http status code.
newDeleteResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourceResponse
newDeleteResourceResponse pHttpStatus_ =
  DeleteResourceResponse'
    { progressEvent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the current status of the resource deletion request.
--
-- After you have initiated a resource deletion request, you can monitor
-- the progress of your request by calling
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- using the @RequestToken@ of the @ProgressEvent@ returned by
-- @DeleteResource@.
deleteResourceResponse_progressEvent :: Lens.Lens' DeleteResourceResponse (Prelude.Maybe ProgressEvent)
deleteResourceResponse_progressEvent = Lens.lens (\DeleteResourceResponse' {progressEvent} -> progressEvent) (\s@DeleteResourceResponse' {} a -> s {progressEvent = a} :: DeleteResourceResponse)

-- | The response's http status code.
deleteResourceResponse_httpStatus :: Lens.Lens' DeleteResourceResponse Prelude.Int
deleteResourceResponse_httpStatus = Lens.lens (\DeleteResourceResponse' {httpStatus} -> httpStatus) (\s@DeleteResourceResponse' {} a -> s {httpStatus = a} :: DeleteResourceResponse)

instance Prelude.NFData DeleteResourceResponse where
  rnf DeleteResourceResponse' {..} =
    Prelude.rnf progressEvent
      `Prelude.seq` Prelude.rnf httpStatus

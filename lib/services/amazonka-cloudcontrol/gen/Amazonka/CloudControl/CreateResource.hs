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
-- Module      : Amazonka.CloudControl.CreateResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified resource. For more information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-create.html Creating a resource>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- After you have initiated a resource creation request, you can monitor
-- the progress of your request by calling
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- using the @RequestToken@ of the @ProgressEvent@ type returned by
-- @CreateResource@.
module Amazonka.CloudControl.CreateResource
  ( -- * Creating a Request
    CreateResource (..),
    newCreateResource,

    -- * Request Lenses
    createResource_clientToken,
    createResource_roleArn,
    createResource_typeVersionId,
    createResource_typeName,
    createResource_desiredState,

    -- * Destructuring the Response
    CreateResourceResponse (..),
    newCreateResourceResponse,

    -- * Response Lenses
    createResourceResponse_progressEvent,
    createResourceResponse_httpStatus,
  )
where

import Amazonka.CloudControl.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateResource' smart constructor.
data CreateResource = CreateResource'
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
    -- | Structured data format representing the desired state of the resource,
    -- consisting of that resource\'s properties and their desired values.
    --
    -- Cloud Control API currently supports JSON as a structured data format.
    --
    -- >  <p>Specify the desired state as one of the following:</p> <ul> <li> <p>A JSON blob</p> </li> <li> <p>A local path containing the desired state in JSON data format</p> </li> </ul> <p>For more information, see <a href="https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-create.html#resource-operations-create-desiredstate">Composing the desired state of the resource</a> in the <i>Amazon Web Services Cloud Control API User Guide</i>.</p> <p>For more information about the properties of a specific resource, refer to the related topic for the resource in the <a href="https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html">Resource and property types reference</a> in the <i>CloudFormation Users Guide</i>.</p>
    desiredState :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createResource_clientToken' - A unique identifier to ensure the idempotency of the resource request.
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
-- 'roleArn', 'createResource_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
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
-- 'typeVersionId', 'createResource_typeVersionId' - For private resource types, the type version to use in this resource
-- operation. If you do not specify a resource version, CloudFormation uses
-- the default version.
--
-- 'typeName', 'createResource_typeName' - The name of the resource type.
--
-- 'desiredState', 'createResource_desiredState' - Structured data format representing the desired state of the resource,
-- consisting of that resource\'s properties and their desired values.
--
-- Cloud Control API currently supports JSON as a structured data format.
--
-- >  <p>Specify the desired state as one of the following:</p> <ul> <li> <p>A JSON blob</p> </li> <li> <p>A local path containing the desired state in JSON data format</p> </li> </ul> <p>For more information, see <a href="https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-create.html#resource-operations-create-desiredstate">Composing the desired state of the resource</a> in the <i>Amazon Web Services Cloud Control API User Guide</i>.</p> <p>For more information about the properties of a specific resource, refer to the related topic for the resource in the <a href="https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html">Resource and property types reference</a> in the <i>CloudFormation Users Guide</i>.</p>
newCreateResource ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'desiredState'
  Prelude.Text ->
  CreateResource
newCreateResource pTypeName_ pDesiredState_ =
  CreateResource'
    { clientToken = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      typeVersionId = Prelude.Nothing,
      typeName = pTypeName_,
      desiredState = Data._Sensitive Lens.# pDesiredState_
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
createResource_clientToken :: Lens.Lens' CreateResource (Prelude.Maybe Prelude.Text)
createResource_clientToken = Lens.lens (\CreateResource' {clientToken} -> clientToken) (\s@CreateResource' {} a -> s {clientToken = a} :: CreateResource)

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
createResource_roleArn :: Lens.Lens' CreateResource (Prelude.Maybe Prelude.Text)
createResource_roleArn = Lens.lens (\CreateResource' {roleArn} -> roleArn) (\s@CreateResource' {} a -> s {roleArn = a} :: CreateResource)

-- | For private resource types, the type version to use in this resource
-- operation. If you do not specify a resource version, CloudFormation uses
-- the default version.
createResource_typeVersionId :: Lens.Lens' CreateResource (Prelude.Maybe Prelude.Text)
createResource_typeVersionId = Lens.lens (\CreateResource' {typeVersionId} -> typeVersionId) (\s@CreateResource' {} a -> s {typeVersionId = a} :: CreateResource)

-- | The name of the resource type.
createResource_typeName :: Lens.Lens' CreateResource Prelude.Text
createResource_typeName = Lens.lens (\CreateResource' {typeName} -> typeName) (\s@CreateResource' {} a -> s {typeName = a} :: CreateResource)

-- | Structured data format representing the desired state of the resource,
-- consisting of that resource\'s properties and their desired values.
--
-- Cloud Control API currently supports JSON as a structured data format.
--
-- >  <p>Specify the desired state as one of the following:</p> <ul> <li> <p>A JSON blob</p> </li> <li> <p>A local path containing the desired state in JSON data format</p> </li> </ul> <p>For more information, see <a href="https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-create.html#resource-operations-create-desiredstate">Composing the desired state of the resource</a> in the <i>Amazon Web Services Cloud Control API User Guide</i>.</p> <p>For more information about the properties of a specific resource, refer to the related topic for the resource in the <a href="https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html">Resource and property types reference</a> in the <i>CloudFormation Users Guide</i>.</p>
createResource_desiredState :: Lens.Lens' CreateResource Prelude.Text
createResource_desiredState = Lens.lens (\CreateResource' {desiredState} -> desiredState) (\s@CreateResource' {} a -> s {desiredState = a} :: CreateResource) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateResource where
  type
    AWSResponse CreateResource =
      CreateResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceResponse'
            Prelude.<$> (x Data..?> "ProgressEvent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResource where
  hashWithSalt _salt CreateResource' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` typeVersionId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` desiredState

instance Prelude.NFData CreateResource where
  rnf CreateResource' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf typeVersionId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf desiredState

instance Data.ToHeaders CreateResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudApiService.CreateResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResource where
  toJSON CreateResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("TypeVersionId" Data..=) Prelude.<$> typeVersionId,
            Prelude.Just ("TypeName" Data..= typeName),
            Prelude.Just ("DesiredState" Data..= desiredState)
          ]
      )

instance Data.ToPath CreateResource where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceResponse' smart constructor.
data CreateResourceResponse = CreateResourceResponse'
  { -- | Represents the current status of the resource creation request.
    --
    -- After you have initiated a resource creation request, you can monitor
    -- the progress of your request by calling
    -- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
    -- using the @RequestToken@ of the @ProgressEvent@ returned by
    -- @CreateResource@.
    progressEvent :: Prelude.Maybe ProgressEvent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressEvent', 'createResourceResponse_progressEvent' - Represents the current status of the resource creation request.
--
-- After you have initiated a resource creation request, you can monitor
-- the progress of your request by calling
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- using the @RequestToken@ of the @ProgressEvent@ returned by
-- @CreateResource@.
--
-- 'httpStatus', 'createResourceResponse_httpStatus' - The response's http status code.
newCreateResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourceResponse
newCreateResourceResponse pHttpStatus_ =
  CreateResourceResponse'
    { progressEvent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the current status of the resource creation request.
--
-- After you have initiated a resource creation request, you can monitor
-- the progress of your request by calling
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/APIReference/API_GetResourceRequestStatus.html GetResourceRequestStatus>
-- using the @RequestToken@ of the @ProgressEvent@ returned by
-- @CreateResource@.
createResourceResponse_progressEvent :: Lens.Lens' CreateResourceResponse (Prelude.Maybe ProgressEvent)
createResourceResponse_progressEvent = Lens.lens (\CreateResourceResponse' {progressEvent} -> progressEvent) (\s@CreateResourceResponse' {} a -> s {progressEvent = a} :: CreateResourceResponse)

-- | The response's http status code.
createResourceResponse_httpStatus :: Lens.Lens' CreateResourceResponse Prelude.Int
createResourceResponse_httpStatus = Lens.lens (\CreateResourceResponse' {httpStatus} -> httpStatus) (\s@CreateResourceResponse' {} a -> s {httpStatus = a} :: CreateResourceResponse)

instance Prelude.NFData CreateResourceResponse where
  rnf CreateResourceResponse' {..} =
    Prelude.rnf progressEvent
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.CloudControl.GetResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the current state of the specified resource.
-- For details, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-read.html Reading a resource\'s current state>.
--
-- You can use this action to return information about an existing resource
-- in your account and Amazon Web Services Region, whether those resources
-- were provisioned using Cloud Control API.
module Amazonka.CloudControl.GetResource
  ( -- * Creating a Request
    GetResource (..),
    newGetResource,

    -- * Request Lenses
    getResource_roleArn,
    getResource_typeVersionId,
    getResource_typeName,
    getResource_identifier,

    -- * Destructuring the Response
    GetResourceResponse (..),
    newGetResourceResponse,

    -- * Response Lenses
    getResourceResponse_resourceDescription,
    getResourceResponse_typeName,
    getResourceResponse_httpStatus,
  )
where

import Amazonka.CloudControl.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResource' smart constructor.
data GetResource = GetResource'
  { -- | The Amazon Resource Name (ARN) of the Identity and Access Management
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
-- Create a value of 'GetResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'getResource_roleArn' - The Amazon Resource Name (ARN) of the Identity and Access Management
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
-- 'typeVersionId', 'getResource_typeVersionId' - For private resource types, the type version to use in this resource
-- operation. If you do not specify a resource version, CloudFormation uses
-- the default version.
--
-- 'typeName', 'getResource_typeName' - The name of the resource type.
--
-- 'identifier', 'getResource_identifier' - The identifier for the resource.
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
newGetResource ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'identifier'
  Prelude.Text ->
  GetResource
newGetResource pTypeName_ pIdentifier_ =
  GetResource'
    { roleArn = Prelude.Nothing,
      typeVersionId = Prelude.Nothing,
      typeName = pTypeName_,
      identifier = pIdentifier_
    }

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
getResource_roleArn :: Lens.Lens' GetResource (Prelude.Maybe Prelude.Text)
getResource_roleArn = Lens.lens (\GetResource' {roleArn} -> roleArn) (\s@GetResource' {} a -> s {roleArn = a} :: GetResource)

-- | For private resource types, the type version to use in this resource
-- operation. If you do not specify a resource version, CloudFormation uses
-- the default version.
getResource_typeVersionId :: Lens.Lens' GetResource (Prelude.Maybe Prelude.Text)
getResource_typeVersionId = Lens.lens (\GetResource' {typeVersionId} -> typeVersionId) (\s@GetResource' {} a -> s {typeVersionId = a} :: GetResource)

-- | The name of the resource type.
getResource_typeName :: Lens.Lens' GetResource Prelude.Text
getResource_typeName = Lens.lens (\GetResource' {typeName} -> typeName) (\s@GetResource' {} a -> s {typeName = a} :: GetResource)

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
getResource_identifier :: Lens.Lens' GetResource Prelude.Text
getResource_identifier = Lens.lens (\GetResource' {identifier} -> identifier) (\s@GetResource' {} a -> s {identifier = a} :: GetResource)

instance Core.AWSRequest GetResource where
  type AWSResponse GetResource = GetResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceResponse'
            Prelude.<$> (x Data..?> "ResourceDescription")
            Prelude.<*> (x Data..?> "TypeName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResource where
  hashWithSalt _salt GetResource' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` typeVersionId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetResource where
  rnf GetResource' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf typeVersionId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToHeaders GetResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudApiService.GetResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResource where
  toJSON GetResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("TypeVersionId" Data..=) Prelude.<$> typeVersionId,
            Prelude.Just ("TypeName" Data..= typeName),
            Prelude.Just ("Identifier" Data..= identifier)
          ]
      )

instance Data.ToPath GetResource where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceResponse' smart constructor.
data GetResourceResponse = GetResourceResponse'
  { resourceDescription :: Prelude.Maybe ResourceDescription,
    -- | The name of the resource type.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDescription', 'getResourceResponse_resourceDescription' - Undocumented member.
--
-- 'typeName', 'getResourceResponse_typeName' - The name of the resource type.
--
-- 'httpStatus', 'getResourceResponse_httpStatus' - The response's http status code.
newGetResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceResponse
newGetResourceResponse pHttpStatus_ =
  GetResourceResponse'
    { resourceDescription =
        Prelude.Nothing,
      typeName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getResourceResponse_resourceDescription :: Lens.Lens' GetResourceResponse (Prelude.Maybe ResourceDescription)
getResourceResponse_resourceDescription = Lens.lens (\GetResourceResponse' {resourceDescription} -> resourceDescription) (\s@GetResourceResponse' {} a -> s {resourceDescription = a} :: GetResourceResponse)

-- | The name of the resource type.
getResourceResponse_typeName :: Lens.Lens' GetResourceResponse (Prelude.Maybe Prelude.Text)
getResourceResponse_typeName = Lens.lens (\GetResourceResponse' {typeName} -> typeName) (\s@GetResourceResponse' {} a -> s {typeName = a} :: GetResourceResponse)

-- | The response's http status code.
getResourceResponse_httpStatus :: Lens.Lens' GetResourceResponse Prelude.Int
getResourceResponse_httpStatus = Lens.lens (\GetResourceResponse' {httpStatus} -> httpStatus) (\s@GetResourceResponse' {} a -> s {httpStatus = a} :: GetResourceResponse)

instance Prelude.NFData GetResourceResponse where
  rnf GetResourceResponse' {..} =
    Prelude.rnf resourceDescription
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf httpStatus

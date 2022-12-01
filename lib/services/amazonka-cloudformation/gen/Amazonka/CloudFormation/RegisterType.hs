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
-- Module      : Amazonka.CloudFormation.RegisterType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an extension with the CloudFormation service. Registering an
-- extension makes it available for use in CloudFormation templates in your
-- Amazon Web Services account, and includes:
--
-- -   Validating the extension schema.
--
-- -   Determining which handlers, if any, have been specified for the
--     extension.
--
-- -   Making the extension available for use in your account.
--
-- For more information about how to develop extensions and ready them for
-- registration, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-types.html Creating Resource Providers>
-- in the /CloudFormation CLI User Guide/.
--
-- You can have a maximum of 50 resource extension versions registered at a
-- time. This maximum is per account and per region. Use
-- <AWSCloudFormation/latest/APIReference/API_DeregisterType.html DeregisterType>
-- to deregister specific extension versions if necessary.
--
-- Once you have initiated a registration request using @ RegisterType @,
-- you can use @ DescribeTypeRegistration @ to monitor the progress of the
-- registration request.
--
-- Once you have registered a private extension in your account and region,
-- use
-- <AWSCloudFormation/latest/APIReference/API_SetTypeConfiguration.html SetTypeConfiguration>
-- to specify configuration properties for the extension. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-register.html#registry-set-configuration Configuring extensions at the account level>
-- in the /CloudFormation User Guide/.
module Amazonka.CloudFormation.RegisterType
  ( -- * Creating a Request
    RegisterType (..),
    newRegisterType,

    -- * Request Lenses
    registerType_type,
    registerType_clientRequestToken,
    registerType_executionRoleArn,
    registerType_loggingConfig,
    registerType_typeName,
    registerType_schemaHandlerPackage,

    -- * Destructuring the Response
    RegisterTypeResponse (..),
    newRegisterTypeResponse,

    -- * Response Lenses
    registerTypeResponse_registrationToken,
    registerTypeResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterType' smart constructor.
data RegisterType = RegisterType'
  { -- | The kind of extension.
    type' :: Prelude.Maybe RegistryType,
    -- | A unique identifier that acts as an idempotency key for this
    -- registration request. Specifying a client request token prevents
    -- CloudFormation from generating more than one version of an extension
    -- from the same registration request, even if the request is submitted
    -- multiple times.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to
    -- assume when invoking the extension.
    --
    -- For CloudFormation to assume the specified execution role, the role must
    -- contain a trust relationship with the CloudFormation service principle
    -- (@resources.cloudformation.amazonaws.com@). For more information about
    -- adding trust relationships, see
    -- <IAM/latest/UserGuide/roles-managingrole-editing-console.html#roles-managingrole_edit-trust-policy Modifying a role trust policy>
    -- in the /Identity and Access Management User Guide/.
    --
    -- If your extension calls Amazon Web Services APIs in any of its handlers,
    -- you must create an
    -- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
    -- that includes the necessary permissions to call those Amazon Web
    -- Services APIs, and provision that execution role in your account. When
    -- CloudFormation needs to invoke the resource type handler, CloudFormation
    -- assumes this execution role to create a temporary session token, which
    -- it then passes to the resource type handler, thereby supplying your
    -- resource type with the appropriate credentials.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies logging configuration information for an extension.
    loggingConfig :: Prelude.Maybe LoggingConfig,
    -- | The name of the extension being registered.
    --
    -- We suggest that extension names adhere to the following patterns:
    --
    -- -   For resource types, /company_or_organization/::/service/::/type/.
    --
    -- -   For modules, /company_or_organization/::/service/::/type/::MODULE.
    --
    -- -   For hooks, /MyCompany/::/Testing/::/MyTestHook/.
    --
    -- The following organization namespaces are reserved and can\'t be used in
    -- your extension names:
    --
    -- -   @Alexa@
    --
    -- -   @AMZN@
    --
    -- -   @Amazon@
    --
    -- -   @AWS@
    --
    -- -   @Custom@
    --
    -- -   @Dev@
    typeName :: Prelude.Text,
    -- | A URL to the S3 bucket containing the extension project package that
    -- contains the necessary files for the extension you want to register.
    --
    -- For information about generating a schema handler package for the
    -- extension you want to register, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit>
    -- in the /CloudFormation CLI User Guide/.
    --
    -- The user registering the extension must be able to access the package in
    -- the S3 bucket. That\'s, the user needs to have
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
    -- permissions for the schema handler package. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
    -- in the /Identity and Access Management User Guide/.
    schemaHandlerPackage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'registerType_type' - The kind of extension.
--
-- 'clientRequestToken', 'registerType_clientRequestToken' - A unique identifier that acts as an idempotency key for this
-- registration request. Specifying a client request token prevents
-- CloudFormation from generating more than one version of an extension
-- from the same registration request, even if the request is submitted
-- multiple times.
--
-- 'executionRoleArn', 'registerType_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role for CloudFormation to
-- assume when invoking the extension.
--
-- For CloudFormation to assume the specified execution role, the role must
-- contain a trust relationship with the CloudFormation service principle
-- (@resources.cloudformation.amazonaws.com@). For more information about
-- adding trust relationships, see
-- <IAM/latest/UserGuide/roles-managingrole-editing-console.html#roles-managingrole_edit-trust-policy Modifying a role trust policy>
-- in the /Identity and Access Management User Guide/.
--
-- If your extension calls Amazon Web Services APIs in any of its handlers,
-- you must create an
-- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
-- that includes the necessary permissions to call those Amazon Web
-- Services APIs, and provision that execution role in your account. When
-- CloudFormation needs to invoke the resource type handler, CloudFormation
-- assumes this execution role to create a temporary session token, which
-- it then passes to the resource type handler, thereby supplying your
-- resource type with the appropriate credentials.
--
-- 'loggingConfig', 'registerType_loggingConfig' - Specifies logging configuration information for an extension.
--
-- 'typeName', 'registerType_typeName' - The name of the extension being registered.
--
-- We suggest that extension names adhere to the following patterns:
--
-- -   For resource types, /company_or_organization/::/service/::/type/.
--
-- -   For modules, /company_or_organization/::/service/::/type/::MODULE.
--
-- -   For hooks, /MyCompany/::/Testing/::/MyTestHook/.
--
-- The following organization namespaces are reserved and can\'t be used in
-- your extension names:
--
-- -   @Alexa@
--
-- -   @AMZN@
--
-- -   @Amazon@
--
-- -   @AWS@
--
-- -   @Custom@
--
-- -   @Dev@
--
-- 'schemaHandlerPackage', 'registerType_schemaHandlerPackage' - A URL to the S3 bucket containing the extension project package that
-- contains the necessary files for the extension you want to register.
--
-- For information about generating a schema handler package for the
-- extension you want to register, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit>
-- in the /CloudFormation CLI User Guide/.
--
-- The user registering the extension must be able to access the package in
-- the S3 bucket. That\'s, the user needs to have
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
-- permissions for the schema handler package. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
-- in the /Identity and Access Management User Guide/.
newRegisterType ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'schemaHandlerPackage'
  Prelude.Text ->
  RegisterType
newRegisterType pTypeName_ pSchemaHandlerPackage_ =
  RegisterType'
    { type' = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      loggingConfig = Prelude.Nothing,
      typeName = pTypeName_,
      schemaHandlerPackage = pSchemaHandlerPackage_
    }

-- | The kind of extension.
registerType_type :: Lens.Lens' RegisterType (Prelude.Maybe RegistryType)
registerType_type = Lens.lens (\RegisterType' {type'} -> type') (\s@RegisterType' {} a -> s {type' = a} :: RegisterType)

-- | A unique identifier that acts as an idempotency key for this
-- registration request. Specifying a client request token prevents
-- CloudFormation from generating more than one version of an extension
-- from the same registration request, even if the request is submitted
-- multiple times.
registerType_clientRequestToken :: Lens.Lens' RegisterType (Prelude.Maybe Prelude.Text)
registerType_clientRequestToken = Lens.lens (\RegisterType' {clientRequestToken} -> clientRequestToken) (\s@RegisterType' {} a -> s {clientRequestToken = a} :: RegisterType)

-- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to
-- assume when invoking the extension.
--
-- For CloudFormation to assume the specified execution role, the role must
-- contain a trust relationship with the CloudFormation service principle
-- (@resources.cloudformation.amazonaws.com@). For more information about
-- adding trust relationships, see
-- <IAM/latest/UserGuide/roles-managingrole-editing-console.html#roles-managingrole_edit-trust-policy Modifying a role trust policy>
-- in the /Identity and Access Management User Guide/.
--
-- If your extension calls Amazon Web Services APIs in any of its handlers,
-- you must create an
-- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
-- that includes the necessary permissions to call those Amazon Web
-- Services APIs, and provision that execution role in your account. When
-- CloudFormation needs to invoke the resource type handler, CloudFormation
-- assumes this execution role to create a temporary session token, which
-- it then passes to the resource type handler, thereby supplying your
-- resource type with the appropriate credentials.
registerType_executionRoleArn :: Lens.Lens' RegisterType (Prelude.Maybe Prelude.Text)
registerType_executionRoleArn = Lens.lens (\RegisterType' {executionRoleArn} -> executionRoleArn) (\s@RegisterType' {} a -> s {executionRoleArn = a} :: RegisterType)

-- | Specifies logging configuration information for an extension.
registerType_loggingConfig :: Lens.Lens' RegisterType (Prelude.Maybe LoggingConfig)
registerType_loggingConfig = Lens.lens (\RegisterType' {loggingConfig} -> loggingConfig) (\s@RegisterType' {} a -> s {loggingConfig = a} :: RegisterType)

-- | The name of the extension being registered.
--
-- We suggest that extension names adhere to the following patterns:
--
-- -   For resource types, /company_or_organization/::/service/::/type/.
--
-- -   For modules, /company_or_organization/::/service/::/type/::MODULE.
--
-- -   For hooks, /MyCompany/::/Testing/::/MyTestHook/.
--
-- The following organization namespaces are reserved and can\'t be used in
-- your extension names:
--
-- -   @Alexa@
--
-- -   @AMZN@
--
-- -   @Amazon@
--
-- -   @AWS@
--
-- -   @Custom@
--
-- -   @Dev@
registerType_typeName :: Lens.Lens' RegisterType Prelude.Text
registerType_typeName = Lens.lens (\RegisterType' {typeName} -> typeName) (\s@RegisterType' {} a -> s {typeName = a} :: RegisterType)

-- | A URL to the S3 bucket containing the extension project package that
-- contains the necessary files for the extension you want to register.
--
-- For information about generating a schema handler package for the
-- extension you want to register, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit>
-- in the /CloudFormation CLI User Guide/.
--
-- The user registering the extension must be able to access the package in
-- the S3 bucket. That\'s, the user needs to have
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
-- permissions for the schema handler package. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
-- in the /Identity and Access Management User Guide/.
registerType_schemaHandlerPackage :: Lens.Lens' RegisterType Prelude.Text
registerType_schemaHandlerPackage = Lens.lens (\RegisterType' {schemaHandlerPackage} -> schemaHandlerPackage) (\s@RegisterType' {} a -> s {schemaHandlerPackage = a} :: RegisterType)

instance Core.AWSRequest RegisterType where
  type AWSResponse RegisterType = RegisterTypeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RegisterTypeResult"
      ( \s h x ->
          RegisterTypeResponse'
            Prelude.<$> (x Core..@? "RegistrationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterType where
  hashWithSalt _salt RegisterType' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` loggingConfig
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` schemaHandlerPackage

instance Prelude.NFData RegisterType where
  rnf RegisterType' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf loggingConfig
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf schemaHandlerPackage

instance Core.ToHeaders RegisterType where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RegisterType where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterType where
  toQuery RegisterType' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RegisterType" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "Type" Core.=: type',
        "ClientRequestToken" Core.=: clientRequestToken,
        "ExecutionRoleArn" Core.=: executionRoleArn,
        "LoggingConfig" Core.=: loggingConfig,
        "TypeName" Core.=: typeName,
        "SchemaHandlerPackage" Core.=: schemaHandlerPackage
      ]

-- | /See:/ 'newRegisterTypeResponse' smart constructor.
data RegisterTypeResponse = RegisterTypeResponse'
  { -- | The identifier for this registration request.
    --
    -- Use this registration token when calling @ DescribeTypeRegistration @,
    -- which returns information about the status and IDs of the extension
    -- registration.
    registrationToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registrationToken', 'registerTypeResponse_registrationToken' - The identifier for this registration request.
--
-- Use this registration token when calling @ DescribeTypeRegistration @,
-- which returns information about the status and IDs of the extension
-- registration.
--
-- 'httpStatus', 'registerTypeResponse_httpStatus' - The response's http status code.
newRegisterTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterTypeResponse
newRegisterTypeResponse pHttpStatus_ =
  RegisterTypeResponse'
    { registrationToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this registration request.
--
-- Use this registration token when calling @ DescribeTypeRegistration @,
-- which returns information about the status and IDs of the extension
-- registration.
registerTypeResponse_registrationToken :: Lens.Lens' RegisterTypeResponse (Prelude.Maybe Prelude.Text)
registerTypeResponse_registrationToken = Lens.lens (\RegisterTypeResponse' {registrationToken} -> registrationToken) (\s@RegisterTypeResponse' {} a -> s {registrationToken = a} :: RegisterTypeResponse)

-- | The response's http status code.
registerTypeResponse_httpStatus :: Lens.Lens' RegisterTypeResponse Prelude.Int
registerTypeResponse_httpStatus = Lens.lens (\RegisterTypeResponse' {httpStatus} -> httpStatus) (\s@RegisterTypeResponse' {} a -> s {httpStatus = a} :: RegisterTypeResponse)

instance Prelude.NFData RegisterTypeResponse where
  rnf RegisterTypeResponse' {..} =
    Prelude.rnf registrationToken
      `Prelude.seq` Prelude.rnf httpStatus

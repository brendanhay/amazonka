{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFormation.RegisterType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an extension with the CloudFormation service. Registering an
-- extension makes it available for use in CloudFormation templates in your
-- AWS account, and includes:
--
-- -   Validating the extension schema
--
-- -   Determining which handlers, if any, have been specified for the
--     extension
--
-- -   Making the extension available for use in your account
--
-- For more information on how to develop extensions and ready them for
-- registeration, see
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
module Network.AWS.CloudFormation.RegisterType
  ( -- * Creating a Request
    RegisterType (..),
    newRegisterType,

    -- * Request Lenses
    registerType_loggingConfig,
    registerType_executionRoleArn,
    registerType_clientRequestToken,
    registerType_type,
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

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterType' smart constructor.
data RegisterType = RegisterType'
  { -- | Specifies logging configuration information for an extension.
    loggingConfig :: Prelude.Maybe LoggingConfig,
    -- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to
    -- assume when invoking the extension. If your extension calls AWS APIs in
    -- any of its handlers, you must create an
    -- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
    -- that includes the necessary permissions to call those AWS APIs, and
    -- provision that execution role in your account. When CloudFormation needs
    -- to invoke the extension handler, CloudFormation assumes this execution
    -- role to create a temporary session token, which it then passes to the
    -- extension handler, thereby supplying your extension with the appropriate
    -- credentials.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that acts as an idempotency key for this
    -- registration request. Specifying a client request token prevents
    -- CloudFormation from generating more than one version of an extension
    -- from the same registeration request, even if the request is submitted
    -- multiple times.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The kind of extension.
    type' :: Prelude.Maybe RegistryType,
    -- | The name of the extension being registered.
    --
    -- We recommend that extension names adhere to the following pattern:
    -- /company_or_organization/::/service/::/type/.
    --
    -- The following organization namespaces are reserved and cannot be used in
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
    -- | A url to the S3 bucket containing the extension project package that
    -- contains the neccessary files for the extension you want to register.
    --
    -- For information on generating a schema handler package for the extension
    -- you want to register, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit>
    -- in the /CloudFormation CLI User Guide/.
    --
    -- The user registering the extension must be able to access the package in
    -- the S3 bucket. That is, the user needs to have
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
    -- permissions for the schema handler package. For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
    -- in the /AWS Identity and Access Management User Guide/.
    schemaHandlerPackage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfig', 'registerType_loggingConfig' - Specifies logging configuration information for an extension.
--
-- 'executionRoleArn', 'registerType_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role for CloudFormation to
-- assume when invoking the extension. If your extension calls AWS APIs in
-- any of its handlers, you must create an
-- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
-- that includes the necessary permissions to call those AWS APIs, and
-- provision that execution role in your account. When CloudFormation needs
-- to invoke the extension handler, CloudFormation assumes this execution
-- role to create a temporary session token, which it then passes to the
-- extension handler, thereby supplying your extension with the appropriate
-- credentials.
--
-- 'clientRequestToken', 'registerType_clientRequestToken' - A unique identifier that acts as an idempotency key for this
-- registration request. Specifying a client request token prevents
-- CloudFormation from generating more than one version of an extension
-- from the same registeration request, even if the request is submitted
-- multiple times.
--
-- 'type'', 'registerType_type' - The kind of extension.
--
-- 'typeName', 'registerType_typeName' - The name of the extension being registered.
--
-- We recommend that extension names adhere to the following pattern:
-- /company_or_organization/::/service/::/type/.
--
-- The following organization namespaces are reserved and cannot be used in
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
-- 'schemaHandlerPackage', 'registerType_schemaHandlerPackage' - A url to the S3 bucket containing the extension project package that
-- contains the neccessary files for the extension you want to register.
--
-- For information on generating a schema handler package for the extension
-- you want to register, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit>
-- in the /CloudFormation CLI User Guide/.
--
-- The user registering the extension must be able to access the package in
-- the S3 bucket. That is, the user needs to have
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
-- permissions for the schema handler package. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
-- in the /AWS Identity and Access Management User Guide/.
newRegisterType ::
  -- | 'typeName'
  Prelude.Text ->
  -- | 'schemaHandlerPackage'
  Prelude.Text ->
  RegisterType
newRegisterType pTypeName_ pSchemaHandlerPackage_ =
  RegisterType'
    { loggingConfig = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      typeName = pTypeName_,
      schemaHandlerPackage = pSchemaHandlerPackage_
    }

-- | Specifies logging configuration information for an extension.
registerType_loggingConfig :: Lens.Lens' RegisterType (Prelude.Maybe LoggingConfig)
registerType_loggingConfig = Lens.lens (\RegisterType' {loggingConfig} -> loggingConfig) (\s@RegisterType' {} a -> s {loggingConfig = a} :: RegisterType)

-- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to
-- assume when invoking the extension. If your extension calls AWS APIs in
-- any of its handlers, you must create an
-- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
-- that includes the necessary permissions to call those AWS APIs, and
-- provision that execution role in your account. When CloudFormation needs
-- to invoke the extension handler, CloudFormation assumes this execution
-- role to create a temporary session token, which it then passes to the
-- extension handler, thereby supplying your extension with the appropriate
-- credentials.
registerType_executionRoleArn :: Lens.Lens' RegisterType (Prelude.Maybe Prelude.Text)
registerType_executionRoleArn = Lens.lens (\RegisterType' {executionRoleArn} -> executionRoleArn) (\s@RegisterType' {} a -> s {executionRoleArn = a} :: RegisterType)

-- | A unique identifier that acts as an idempotency key for this
-- registration request. Specifying a client request token prevents
-- CloudFormation from generating more than one version of an extension
-- from the same registeration request, even if the request is submitted
-- multiple times.
registerType_clientRequestToken :: Lens.Lens' RegisterType (Prelude.Maybe Prelude.Text)
registerType_clientRequestToken = Lens.lens (\RegisterType' {clientRequestToken} -> clientRequestToken) (\s@RegisterType' {} a -> s {clientRequestToken = a} :: RegisterType)

-- | The kind of extension.
registerType_type :: Lens.Lens' RegisterType (Prelude.Maybe RegistryType)
registerType_type = Lens.lens (\RegisterType' {type'} -> type') (\s@RegisterType' {} a -> s {type' = a} :: RegisterType)

-- | The name of the extension being registered.
--
-- We recommend that extension names adhere to the following pattern:
-- /company_or_organization/::/service/::/type/.
--
-- The following organization namespaces are reserved and cannot be used in
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

-- | A url to the S3 bucket containing the extension project package that
-- contains the neccessary files for the extension you want to register.
--
-- For information on generating a schema handler package for the extension
-- you want to register, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit>
-- in the /CloudFormation CLI User Guide/.
--
-- The user registering the extension must be able to access the package in
-- the S3 bucket. That is, the user needs to have
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
-- permissions for the schema handler package. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/list_amazons3.html Actions, Resources, and Condition Keys for Amazon S3>
-- in the /AWS Identity and Access Management User Guide/.
registerType_schemaHandlerPackage :: Lens.Lens' RegisterType Prelude.Text
registerType_schemaHandlerPackage = Lens.lens (\RegisterType' {schemaHandlerPackage} -> schemaHandlerPackage) (\s@RegisterType' {} a -> s {schemaHandlerPackage = a} :: RegisterType)

instance Prelude.AWSRequest RegisterType where
  type Rs RegisterType = RegisterTypeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RegisterTypeResult"
      ( \s h x ->
          RegisterTypeResponse'
            Prelude.<$> (x Prelude..@? "RegistrationToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterType

instance Prelude.NFData RegisterType

instance Prelude.ToHeaders RegisterType where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RegisterType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterType where
  toQuery RegisterType' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RegisterType" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-15" :: Prelude.ByteString),
        "LoggingConfig" Prelude.=: loggingConfig,
        "ExecutionRoleArn" Prelude.=: executionRoleArn,
        "ClientRequestToken" Prelude.=: clientRequestToken,
        "Type" Prelude.=: type',
        "TypeName" Prelude.=: typeName,
        "SchemaHandlerPackage"
          Prelude.=: schemaHandlerPackage
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData RegisterTypeResponse

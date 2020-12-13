{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.RegisterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a type with the CloudFormation service. Registering a type makes it available for use in CloudFormation templates in your AWS account, and includes:
--
--
--     * Validating the resource schema
--
--
--     * Determining which handlers have been specified for the resource
--
--
--     * Making the resource type available for use in your account
--
--
-- For more information on how to develop types and ready them for registeration, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-types.html Creating Resource Providers> in the /CloudFormation CLI User Guide/ .
-- You can have a maximum of 50 resource type versions registered at a time. This maximum is per account and per region. Use <AWSCloudFormation/latest/APIReference/API_DeregisterType.html DeregisterType> to deregister specific resource type versions if necessary.
-- Once you have initiated a registration request using @'RegisterType' @ , you can use @'DescribeTypeRegistration' @ to monitor the progress of the registration request.
module Network.AWS.CloudFormation.RegisterType
  ( -- * Creating a request
    RegisterType (..),
    mkRegisterType,

    -- ** Request lenses
    rTypeName,
    rSchemaHandlerPackage,
    rExecutionRoleARN,
    rType,
    rClientRequestToken,
    rLoggingConfig,

    -- * Destructuring the response
    RegisterTypeResponse (..),
    mkRegisterTypeResponse,

    -- ** Response lenses
    rtrsRegistrationToken,
    rtrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterType' smart constructor.
data RegisterType = RegisterType'
  { -- | The name of the type being registered.
    --
    -- We recommend that type names adhere to the following pattern: /company_or_organization/ ::/service/ ::/type/ .
    typeName :: Lude.Text,
    -- | A url to the S3 bucket containing the schema handler package that contains the schema, event handlers, and associated files for the type you want to register.
    --
    -- For information on generating a schema handler package for the type you want to register, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit> in the /CloudFormation CLI User Guide/ .
    schemaHandlerPackage :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to assume when invoking the resource provider. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. When CloudFormation needs to invoke the resource provider handler, CloudFormation assumes this execution role to create a temporary session token, which it then passes to the resource provider handler, thereby supplying your resource provider with the appropriate credentials.
    executionRoleARN :: Lude.Maybe Lude.Text,
    -- | The kind of type.
    --
    -- Currently, the only valid value is @RESOURCE@ .
    type' :: Lude.Maybe RegistryType,
    -- | A unique identifier that acts as an idempotency key for this registration request. Specifying a client request token prevents CloudFormation from generating more than one version of a type from the same registeration request, even if the request is submitted multiple times.
    clientRequestToken :: Lude.Maybe Lude.Text,
    -- | Specifies logging configuration information for a type.
    loggingConfig :: Lude.Maybe LoggingConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterType' with the minimum fields required to make a request.
--
-- * 'typeName' - The name of the type being registered.
--
-- We recommend that type names adhere to the following pattern: /company_or_organization/ ::/service/ ::/type/ .
-- * 'schemaHandlerPackage' - A url to the S3 bucket containing the schema handler package that contains the schema, event handlers, and associated files for the type you want to register.
--
-- For information on generating a schema handler package for the type you want to register, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit> in the /CloudFormation CLI User Guide/ .
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the IAM role for CloudFormation to assume when invoking the resource provider. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. When CloudFormation needs to invoke the resource provider handler, CloudFormation assumes this execution role to create a temporary session token, which it then passes to the resource provider handler, thereby supplying your resource provider with the appropriate credentials.
-- * 'type'' - The kind of type.
--
-- Currently, the only valid value is @RESOURCE@ .
-- * 'clientRequestToken' - A unique identifier that acts as an idempotency key for this registration request. Specifying a client request token prevents CloudFormation from generating more than one version of a type from the same registeration request, even if the request is submitted multiple times.
-- * 'loggingConfig' - Specifies logging configuration information for a type.
mkRegisterType ::
  -- | 'typeName'
  Lude.Text ->
  -- | 'schemaHandlerPackage'
  Lude.Text ->
  RegisterType
mkRegisterType pTypeName_ pSchemaHandlerPackage_ =
  RegisterType'
    { typeName = pTypeName_,
      schemaHandlerPackage = pSchemaHandlerPackage_,
      executionRoleARN = Lude.Nothing,
      type' = Lude.Nothing,
      clientRequestToken = Lude.Nothing,
      loggingConfig = Lude.Nothing
    }

-- | The name of the type being registered.
--
-- We recommend that type names adhere to the following pattern: /company_or_organization/ ::/service/ ::/type/ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTypeName :: Lens.Lens' RegisterType Lude.Text
rTypeName = Lens.lens (typeName :: RegisterType -> Lude.Text) (\s a -> s {typeName = a} :: RegisterType)
{-# DEPRECATED rTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | A url to the S3 bucket containing the schema handler package that contains the schema, event handlers, and associated files for the type you want to register.
--
-- For information on generating a schema handler package for the type you want to register, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit> in the /CloudFormation CLI User Guide/ .
--
-- /Note:/ Consider using 'schemaHandlerPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSchemaHandlerPackage :: Lens.Lens' RegisterType Lude.Text
rSchemaHandlerPackage = Lens.lens (schemaHandlerPackage :: RegisterType -> Lude.Text) (\s a -> s {schemaHandlerPackage = a} :: RegisterType)
{-# DEPRECATED rSchemaHandlerPackage "Use generic-lens or generic-optics with 'schemaHandlerPackage' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to assume when invoking the resource provider. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. When CloudFormation needs to invoke the resource provider handler, CloudFormation assumes this execution role to create a temporary session token, which it then passes to the resource provider handler, thereby supplying your resource provider with the appropriate credentials.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rExecutionRoleARN :: Lens.Lens' RegisterType (Lude.Maybe Lude.Text)
rExecutionRoleARN = Lens.lens (executionRoleARN :: RegisterType -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleARN = a} :: RegisterType)
{-# DEPRECATED rExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

-- | The kind of type.
--
-- Currently, the only valid value is @RESOURCE@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' RegisterType (Lude.Maybe RegistryType)
rType = Lens.lens (type' :: RegisterType -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: RegisterType)
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A unique identifier that acts as an idempotency key for this registration request. Specifying a client request token prevents CloudFormation from generating more than one version of a type from the same registeration request, even if the request is submitted multiple times.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rClientRequestToken :: Lens.Lens' RegisterType (Lude.Maybe Lude.Text)
rClientRequestToken = Lens.lens (clientRequestToken :: RegisterType -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: RegisterType)
{-# DEPRECATED rClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Specifies logging configuration information for a type.
--
-- /Note:/ Consider using 'loggingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLoggingConfig :: Lens.Lens' RegisterType (Lude.Maybe LoggingConfig)
rLoggingConfig = Lens.lens (loggingConfig :: RegisterType -> Lude.Maybe LoggingConfig) (\s a -> s {loggingConfig = a} :: RegisterType)
{-# DEPRECATED rLoggingConfig "Use generic-lens or generic-optics with 'loggingConfig' instead." #-}

instance Lude.AWSRequest RegisterType where
  type Rs RegisterType = RegisterTypeResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "RegisterTypeResult"
      ( \s h x ->
          RegisterTypeResponse'
            Lude.<$> (x Lude..@? "RegistrationToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterType where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RegisterType where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterType where
  toQuery RegisterType' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RegisterType" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "TypeName" Lude.=: typeName,
        "SchemaHandlerPackage" Lude.=: schemaHandlerPackage,
        "ExecutionRoleArn" Lude.=: executionRoleARN,
        "Type" Lude.=: type',
        "ClientRequestToken" Lude.=: clientRequestToken,
        "LoggingConfig" Lude.=: loggingConfig
      ]

-- | /See:/ 'mkRegisterTypeResponse' smart constructor.
data RegisterTypeResponse = RegisterTypeResponse'
  { -- | The identifier for this registration request.
    --
    -- Use this registration token when calling @'DescribeTypeRegistration' @ , which returns information about the status and IDs of the type registration.
    registrationToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTypeResponse' with the minimum fields required to make a request.
--
-- * 'registrationToken' - The identifier for this registration request.
--
-- Use this registration token when calling @'DescribeTypeRegistration' @ , which returns information about the status and IDs of the type registration.
-- * 'responseStatus' - The response status code.
mkRegisterTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterTypeResponse
mkRegisterTypeResponse pResponseStatus_ =
  RegisterTypeResponse'
    { registrationToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier for this registration request.
--
-- Use this registration token when calling @'DescribeTypeRegistration' @ , which returns information about the status and IDs of the type registration.
--
-- /Note:/ Consider using 'registrationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsRegistrationToken :: Lens.Lens' RegisterTypeResponse (Lude.Maybe Lude.Text)
rtrsRegistrationToken = Lens.lens (registrationToken :: RegisterTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {registrationToken = a} :: RegisterTypeResponse)
{-# DEPRECATED rtrsRegistrationToken "Use generic-lens or generic-optics with 'registrationToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrsResponseStatus :: Lens.Lens' RegisterTypeResponse Lude.Int
rtrsResponseStatus = Lens.lens (responseStatus :: RegisterTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterTypeResponse)
{-# DEPRECATED rtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

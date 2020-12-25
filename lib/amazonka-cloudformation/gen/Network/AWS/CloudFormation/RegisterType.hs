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
    rClientRequestToken,
    rExecutionRoleArn,
    rLoggingConfig,
    rType,

    -- * Destructuring the response
    RegisterTypeResponse (..),
    mkRegisterTypeResponse,

    -- ** Response lenses
    rtrrsRegistrationToken,
    rtrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterType' smart constructor.
data RegisterType = RegisterType'
  { -- | The name of the type being registered.
    --
    -- We recommend that type names adhere to the following pattern: /company_or_organization/ ::/service/ ::/type/ .
    typeName :: Types.TypeName,
    -- | A url to the S3 bucket containing the schema handler package that contains the schema, event handlers, and associated files for the type you want to register.
    --
    -- For information on generating a schema handler package for the type you want to register, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit> in the /CloudFormation CLI User Guide/ .
    schemaHandlerPackage :: Types.SchemaHandlerPackage,
    -- | A unique identifier that acts as an idempotency key for this registration request. Specifying a client request token prevents CloudFormation from generating more than one version of a type from the same registeration request, even if the request is submitted multiple times.
    clientRequestToken :: Core.Maybe Types.RequestToken,
    -- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to assume when invoking the resource provider. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. When CloudFormation needs to invoke the resource provider handler, CloudFormation assumes this execution role to create a temporary session token, which it then passes to the resource provider handler, thereby supplying your resource provider with the appropriate credentials.
    executionRoleArn :: Core.Maybe Types.ExecutionRoleArn,
    -- | Specifies logging configuration information for a type.
    loggingConfig :: Core.Maybe Types.LoggingConfig,
    -- | The kind of type.
    --
    -- Currently, the only valid value is @RESOURCE@ .
    type' :: Core.Maybe Types.RegistryType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterType' value with any optional fields omitted.
mkRegisterType ::
  -- | 'typeName'
  Types.TypeName ->
  -- | 'schemaHandlerPackage'
  Types.SchemaHandlerPackage ->
  RegisterType
mkRegisterType typeName schemaHandlerPackage =
  RegisterType'
    { typeName,
      schemaHandlerPackage,
      clientRequestToken = Core.Nothing,
      executionRoleArn = Core.Nothing,
      loggingConfig = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the type being registered.
--
-- We recommend that type names adhere to the following pattern: /company_or_organization/ ::/service/ ::/type/ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rTypeName :: Lens.Lens' RegisterType Types.TypeName
rTypeName = Lens.field @"typeName"
{-# DEPRECATED rTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | A url to the S3 bucket containing the schema handler package that contains the schema, event handlers, and associated files for the type you want to register.
--
-- For information on generating a schema handler package for the type you want to register, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-cli-submit.html submit> in the /CloudFormation CLI User Guide/ .
--
-- /Note:/ Consider using 'schemaHandlerPackage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSchemaHandlerPackage :: Lens.Lens' RegisterType Types.SchemaHandlerPackage
rSchemaHandlerPackage = Lens.field @"schemaHandlerPackage"
{-# DEPRECATED rSchemaHandlerPackage "Use generic-lens or generic-optics with 'schemaHandlerPackage' instead." #-}

-- | A unique identifier that acts as an idempotency key for this registration request. Specifying a client request token prevents CloudFormation from generating more than one version of a type from the same registeration request, even if the request is submitted multiple times.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rClientRequestToken :: Lens.Lens' RegisterType (Core.Maybe Types.RequestToken)
rClientRequestToken = Lens.field @"clientRequestToken"
{-# DEPRECATED rClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role for CloudFormation to assume when invoking the resource provider. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. When CloudFormation needs to invoke the resource provider handler, CloudFormation assumes this execution role to create a temporary session token, which it then passes to the resource provider handler, thereby supplying your resource provider with the appropriate credentials.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rExecutionRoleArn :: Lens.Lens' RegisterType (Core.Maybe Types.ExecutionRoleArn)
rExecutionRoleArn = Lens.field @"executionRoleArn"
{-# DEPRECATED rExecutionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead." #-}

-- | Specifies logging configuration information for a type.
--
-- /Note:/ Consider using 'loggingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rLoggingConfig :: Lens.Lens' RegisterType (Core.Maybe Types.LoggingConfig)
rLoggingConfig = Lens.field @"loggingConfig"
{-# DEPRECATED rLoggingConfig "Use generic-lens or generic-optics with 'loggingConfig' instead." #-}

-- | The kind of type.
--
-- Currently, the only valid value is @RESOURCE@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' RegisterType (Core.Maybe Types.RegistryType)
rType = Lens.field @"type'"
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.AWSRequest RegisterType where
  type Rs RegisterType = RegisterTypeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RegisterType")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "TypeName" typeName)
                Core.<> (Core.toQueryValue "SchemaHandlerPackage" schemaHandlerPackage)
                Core.<> ( Core.toQueryValue "ClientRequestToken"
                            Core.<$> clientRequestToken
                        )
                Core.<> (Core.toQueryValue "ExecutionRoleArn" Core.<$> executionRoleArn)
                Core.<> (Core.toQueryValue "LoggingConfig" Core.<$> loggingConfig)
                Core.<> (Core.toQueryValue "Type" Core.<$> type')
            )
      }
  response =
    Response.receiveXMLWrapper
      "RegisterTypeResult"
      ( \s h x ->
          RegisterTypeResponse'
            Core.<$> (x Core..@? "RegistrationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterTypeResponse' smart constructor.
data RegisterTypeResponse = RegisterTypeResponse'
  { -- | The identifier for this registration request.
    --
    -- Use this registration token when calling @'DescribeTypeRegistration' @ , which returns information about the status and IDs of the type registration.
    registrationToken :: Core.Maybe Types.RegistrationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTypeResponse' value with any optional fields omitted.
mkRegisterTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterTypeResponse
mkRegisterTypeResponse responseStatus =
  RegisterTypeResponse'
    { registrationToken = Core.Nothing,
      responseStatus
    }

-- | The identifier for this registration request.
--
-- Use this registration token when calling @'DescribeTypeRegistration' @ , which returns information about the status and IDs of the type registration.
--
-- /Note:/ Consider using 'registrationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsRegistrationToken :: Lens.Lens' RegisterTypeResponse (Core.Maybe Types.RegistrationToken)
rtrrsRegistrationToken = Lens.field @"registrationToken"
{-# DEPRECATED rtrrsRegistrationToken "Use generic-lens or generic-optics with 'registrationToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrrsResponseStatus :: Lens.Lens' RegisterTypeResponse Core.Int
rtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

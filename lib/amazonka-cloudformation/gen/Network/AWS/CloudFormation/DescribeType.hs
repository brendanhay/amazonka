{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a type that has been registered.
--
-- If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
module Network.AWS.CloudFormation.DescribeType
  ( -- * Creating a request
    DescribeType (..),
    mkDescribeType,

    -- ** Request lenses
    dtArn,
    dtType,
    dtTypeName,
    dtVersionId,

    -- * Destructuring the response
    DescribeTypeResponse (..),
    mkDescribeTypeResponse,

    -- ** Response lenses
    dtrrsArn,
    dtrrsDefaultVersionId,
    dtrrsDeprecatedStatus,
    dtrrsDescription,
    dtrrsDocumentationUrl,
    dtrrsExecutionRoleArn,
    dtrrsIsDefaultVersion,
    dtrrsLastUpdated,
    dtrrsLoggingConfig,
    dtrrsProvisioningType,
    dtrrsSchema,
    dtrrsSourceUrl,
    dtrrsTimeCreated,
    dtrrsType,
    dtrrsTypeName,
    dtrrsVisibility,
    dtrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeType' smart constructor.
data DescribeType = DescribeType'
  { -- | The Amazon Resource Name (ARN) of the type.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    arn :: Core.Maybe Types.Arn,
    -- | The kind of type.
    --
    -- Currently the only valid value is @RESOURCE@ .
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    type' :: Core.Maybe Types.RegistryType,
    -- | The name of the type.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    typeName :: Core.Maybe Types.TypeName,
    -- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
    --
    -- If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
    versionId :: Core.Maybe Types.VersionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeType' value with any optional fields omitted.
mkDescribeType ::
  DescribeType
mkDescribeType =
  DescribeType'
    { arn = Core.Nothing,
      type' = Core.Nothing,
      typeName = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtArn :: Lens.Lens' DescribeType (Core.Maybe Types.Arn)
dtArn = Lens.field @"arn"
{-# DEPRECATED dtArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtType :: Lens.Lens' DescribeType (Core.Maybe Types.RegistryType)
dtType = Lens.field @"type'"
{-# DEPRECATED dtType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTypeName :: Lens.Lens' DescribeType (Core.Maybe Types.TypeName)
dtTypeName = Lens.field @"typeName"
{-# DEPRECATED dtTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtVersionId :: Lens.Lens' DescribeType (Core.Maybe Types.VersionId)
dtVersionId = Lens.field @"versionId"
{-# DEPRECATED dtVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.AWSRequest DescribeType where
  type Rs DescribeType = DescribeTypeResponse
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
            ( Core.pure ("Action", "DescribeType")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "Arn" Core.<$> arn)
                Core.<> (Core.toQueryValue "Type" Core.<$> type')
                Core.<> (Core.toQueryValue "TypeName" Core.<$> typeName)
                Core.<> (Core.toQueryValue "VersionId" Core.<$> versionId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeTypeResult"
      ( \s h x ->
          DescribeTypeResponse'
            Core.<$> (x Core..@? "Arn")
            Core.<*> (x Core..@? "DefaultVersionId")
            Core.<*> (x Core..@? "DeprecatedStatus")
            Core.<*> (x Core..@? "Description")
            Core.<*> (x Core..@? "DocumentationUrl")
            Core.<*> (x Core..@? "ExecutionRoleArn")
            Core.<*> (x Core..@? "IsDefaultVersion")
            Core.<*> (x Core..@? "LastUpdated")
            Core.<*> (x Core..@? "LoggingConfig")
            Core.<*> (x Core..@? "ProvisioningType")
            Core.<*> (x Core..@? "Schema")
            Core.<*> (x Core..@? "SourceUrl")
            Core.<*> (x Core..@? "TimeCreated")
            Core.<*> (x Core..@? "Type")
            Core.<*> (x Core..@? "TypeName")
            Core.<*> (x Core..@? "Visibility")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeTypeResponse' smart constructor.
data DescribeTypeResponse = DescribeTypeResponse'
  { -- | The Amazon Resource Name (ARN) of the type.
    arn :: Core.Maybe Types.Arn,
    -- | The ID of the default version of the type. The default version is used when the type version is not specified.
    --
    -- To set the default version of a type, use @'SetTypeDefaultVersion' @ .
    defaultVersionId :: Core.Maybe Types.TypeVersionId,
    -- | The deprecation status of the type.
    --
    -- Valid values include:
    --
    --     * @LIVE@ : The type is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.
    --
    --
    --     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
    deprecatedStatus :: Core.Maybe Types.DeprecatedStatus,
    -- | The description of the registered type.
    description :: Core.Maybe Types.Description,
    -- | The URL of a page providing detailed documentation for this type.
    documentationUrl :: Core.Maybe Types.DocumentationUrl,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
    executionRoleArn :: Core.Maybe Types.ExecutionRoleArn,
    -- | Whether the specified type version is set as the default version.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | When the specified type version was registered.
    lastUpdated :: Core.Maybe Core.UTCTime,
    -- | Contains logging configuration information for a type.
    loggingConfig :: Core.Maybe Types.LoggingConfig,
    -- | The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted.
    --
    -- Valid values include:
    --
    --     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.
    --
    --
    --     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.
    --
    --
    --     * @NON_PROVISIONABLE@ : The type does not include all of the following handlers, and therefore cannot actually be provisioned.
    --
    --     * create
    --
    --
    --     * read
    --
    --
    --     * delete
    provisioningType :: Core.Maybe Types.ProvisioningType,
    -- | The schema that defines the type.
    --
    -- For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
    schema :: Core.Maybe Types.Schema,
    -- | The URL of the source code for the type.
    sourceUrl :: Core.Maybe Types.SourceUrl,
    -- | When the specified type version was registered.
    timeCreated :: Core.Maybe Core.UTCTime,
    -- | The kind of type.
    --
    -- Currently the only valid value is @RESOURCE@ .
    type' :: Core.Maybe Types.RegistryType,
    -- | The name of the registered type.
    typeName :: Core.Maybe Types.TypeName,
    -- | The scope at which the type is visible and usable in CloudFormation operations.
    --
    -- Valid values include:
    --
    --     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you register as @PRIVATE@ .
    --
    --
    --     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
    visibility :: Core.Maybe Types.Visibility,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeTypeResponse' value with any optional fields omitted.
mkDescribeTypeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeTypeResponse
mkDescribeTypeResponse responseStatus =
  DescribeTypeResponse'
    { arn = Core.Nothing,
      defaultVersionId = Core.Nothing,
      deprecatedStatus = Core.Nothing,
      description = Core.Nothing,
      documentationUrl = Core.Nothing,
      executionRoleArn = Core.Nothing,
      isDefaultVersion = Core.Nothing,
      lastUpdated = Core.Nothing,
      loggingConfig = Core.Nothing,
      provisioningType = Core.Nothing,
      schema = Core.Nothing,
      sourceUrl = Core.Nothing,
      timeCreated = Core.Nothing,
      type' = Core.Nothing,
      typeName = Core.Nothing,
      visibility = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the type.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsArn :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.Arn)
dtrrsArn = Lens.field @"arn"
{-# DEPRECATED dtrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the default version of the type. The default version is used when the type version is not specified.
--
-- To set the default version of a type, use @'SetTypeDefaultVersion' @ .
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsDefaultVersionId :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.TypeVersionId)
dtrrsDefaultVersionId = Lens.field @"defaultVersionId"
{-# DEPRECATED dtrrsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The deprecation status of the type.
--
-- Valid values include:
--
--     * @LIVE@ : The type is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.
--
--
--     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
--
--
--
-- /Note:/ Consider using 'deprecatedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsDeprecatedStatus :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.DeprecatedStatus)
dtrrsDeprecatedStatus = Lens.field @"deprecatedStatus"
{-# DEPRECATED dtrrsDeprecatedStatus "Use generic-lens or generic-optics with 'deprecatedStatus' instead." #-}

-- | The description of the registered type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsDescription :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.Description)
dtrrsDescription = Lens.field @"description"
{-# DEPRECATED dtrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The URL of a page providing detailed documentation for this type.
--
-- /Note:/ Consider using 'documentationUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsDocumentationUrl :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.DocumentationUrl)
dtrrsDocumentationUrl = Lens.field @"documentationUrl"
{-# DEPRECATED dtrrsDocumentationUrl "Use generic-lens or generic-optics with 'documentationUrl' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsExecutionRoleArn :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.ExecutionRoleArn)
dtrrsExecutionRoleArn = Lens.field @"executionRoleArn"
{-# DEPRECATED dtrrsExecutionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead." #-}

-- | Whether the specified type version is set as the default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsIsDefaultVersion :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Bool)
dtrrsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# DEPRECATED dtrrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | When the specified type version was registered.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsLastUpdated :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.UTCTime)
dtrrsLastUpdated = Lens.field @"lastUpdated"
{-# DEPRECATED dtrrsLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | Contains logging configuration information for a type.
--
-- /Note:/ Consider using 'loggingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsLoggingConfig :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.LoggingConfig)
dtrrsLoggingConfig = Lens.field @"loggingConfig"
{-# DEPRECATED dtrrsLoggingConfig "Use generic-lens or generic-optics with 'loggingConfig' instead." #-}

-- | The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted.
--
-- Valid values include:
--
--     * @FULLY_MUTABLE@ : The type includes an update handler to process updates to the type during stack update operations.
--
--
--     * @IMMUTABLE@ : The type does not include an update handler, so the type cannot be updated and must instead be replaced during stack update operations.
--
--
--     * @NON_PROVISIONABLE@ : The type does not include all of the following handlers, and therefore cannot actually be provisioned.
--
--     * create
--
--
--     * read
--
--
--     * delete
--
--
--
--
--
-- /Note:/ Consider using 'provisioningType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsProvisioningType :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.ProvisioningType)
dtrrsProvisioningType = Lens.field @"provisioningType"
{-# DEPRECATED dtrrsProvisioningType "Use generic-lens or generic-optics with 'provisioningType' instead." #-}

-- | The schema that defines the type.
--
-- For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsSchema :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.Schema)
dtrrsSchema = Lens.field @"schema"
{-# DEPRECATED dtrrsSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The URL of the source code for the type.
--
-- /Note:/ Consider using 'sourceUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsSourceUrl :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.SourceUrl)
dtrrsSourceUrl = Lens.field @"sourceUrl"
{-# DEPRECATED dtrrsSourceUrl "Use generic-lens or generic-optics with 'sourceUrl' instead." #-}

-- | When the specified type version was registered.
--
-- /Note:/ Consider using 'timeCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTimeCreated :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.UTCTime)
dtrrsTimeCreated = Lens.field @"timeCreated"
{-# DEPRECATED dtrrsTimeCreated "Use generic-lens or generic-optics with 'timeCreated' instead." #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsType :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.RegistryType)
dtrrsType = Lens.field @"type'"
{-# DEPRECATED dtrrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The name of the registered type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsTypeName :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.TypeName)
dtrrsTypeName = Lens.field @"typeName"
{-# DEPRECATED dtrrsTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The scope at which the type is visible and usable in CloudFormation operations.
--
-- Valid values include:
--
--     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you register as @PRIVATE@ .
--
--
--     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
--
--
--
-- /Note:/ Consider using 'visibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsVisibility :: Lens.Lens' DescribeTypeResponse (Core.Maybe Types.Visibility)
dtrrsVisibility = Lens.field @"visibility"
{-# DEPRECATED dtrrsVisibility "Use generic-lens or generic-optics with 'visibility' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeTypeResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

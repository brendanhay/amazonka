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
    dtVersionId,
    dtTypeName,
    dtARN,
    dtType,

    -- * Destructuring the response
    DescribeTypeResponse (..),
    mkDescribeTypeResponse,

    -- ** Response lenses
    drsLastUpdated,
    drsTypeName,
    drsARN,
    drsExecutionRoleARN,
    drsVisibility,
    drsSchema,
    drsDefaultVersionId,
    drsDeprecatedStatus,
    drsTimeCreated,
    drsType,
    drsIsDefaultVersion,
    drsDescription,
    drsSourceURL,
    drsDocumentationURL,
    drsProvisioningType,
    drsLoggingConfig,
    drsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeType' smart constructor.
data DescribeType = DescribeType'
  { -- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
    --
    -- If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
    versionId :: Lude.Maybe Lude.Text,
    -- | The name of the type.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    typeName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the type.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    arn :: Lude.Maybe Lude.Text,
    -- | The kind of type.
    --
    -- Currently the only valid value is @RESOURCE@ .
    -- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
    type' :: Lude.Maybe RegistryType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeType' with the minimum fields required to make a request.
--
-- * 'versionId' - The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
-- * 'typeName' - The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'arn' - The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
-- * 'type'' - The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
mkDescribeType ::
  DescribeType
mkDescribeType =
  DescribeType'
    { versionId = Lude.Nothing,
      typeName = Lude.Nothing,
      arn = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The ID of a specific version of the type. The version ID is the value at the end of the Amazon Resource Name (ARN) assigned to the type version when it is registered.
--
-- If you specify a @VersionId@ , @DescribeType@ returns information about that specific type version. Otherwise, it returns information about the default type version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtVersionId :: Lens.Lens' DescribeType (Lude.Maybe Lude.Text)
dtVersionId = Lens.lens (versionId :: DescribeType -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: DescribeType)
{-# DEPRECATED dtVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTypeName :: Lens.Lens' DescribeType (Lude.Maybe Lude.Text)
dtTypeName = Lens.lens (typeName :: DescribeType -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: DescribeType)
{-# DEPRECATED dtTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The Amazon Resource Name (ARN) of the type.
--
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtARN :: Lens.Lens' DescribeType (Lude.Maybe Lude.Text)
dtARN = Lens.lens (arn :: DescribeType -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeType)
{-# DEPRECATED dtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- Conditional: You must specify either @TypeName@ and @Type@ , or @Arn@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtType :: Lens.Lens' DescribeType (Lude.Maybe RegistryType)
dtType = Lens.lens (type' :: DescribeType -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: DescribeType)
{-# DEPRECATED dtType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.AWSRequest DescribeType where
  type Rs DescribeType = DescribeTypeResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "DescribeTypeResult"
      ( \s h x ->
          DescribeTypeResponse'
            Lude.<$> (x Lude..@? "LastUpdated")
            Lude.<*> (x Lude..@? "TypeName")
            Lude.<*> (x Lude..@? "Arn")
            Lude.<*> (x Lude..@? "ExecutionRoleArn")
            Lude.<*> (x Lude..@? "Visibility")
            Lude.<*> (x Lude..@? "Schema")
            Lude.<*> (x Lude..@? "DefaultVersionId")
            Lude.<*> (x Lude..@? "DeprecatedStatus")
            Lude.<*> (x Lude..@? "TimeCreated")
            Lude.<*> (x Lude..@? "Type")
            Lude.<*> (x Lude..@? "IsDefaultVersion")
            Lude.<*> (x Lude..@? "Description")
            Lude.<*> (x Lude..@? "SourceUrl")
            Lude.<*> (x Lude..@? "DocumentationUrl")
            Lude.<*> (x Lude..@? "ProvisioningType")
            Lude.<*> (x Lude..@? "LoggingConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeType where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeType where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeType where
  toQuery DescribeType' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeType" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "VersionId" Lude.=: versionId,
        "TypeName" Lude.=: typeName,
        "Arn" Lude.=: arn,
        "Type" Lude.=: type'
      ]

-- | /See:/ 'mkDescribeTypeResponse' smart constructor.
data DescribeTypeResponse = DescribeTypeResponse'
  { -- | When the specified type version was registered.
    lastUpdated :: Lude.Maybe Lude.DateTime,
    -- | The name of the registered type.
    typeName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the type.
    arn :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
    executionRoleARN :: Lude.Maybe Lude.Text,
    -- | The scope at which the type is visible and usable in CloudFormation operations.
    --
    -- Valid values include:
    --
    --     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you register as @PRIVATE@ .
    --
    --
    --     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
    visibility :: Lude.Maybe Visibility,
    -- | The schema that defines the type.
    --
    -- For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
    schema :: Lude.Maybe Lude.Text,
    -- | The ID of the default version of the type. The default version is used when the type version is not specified.
    --
    -- To set the default version of a type, use @'SetTypeDefaultVersion' @ .
    defaultVersionId :: Lude.Maybe Lude.Text,
    -- | The deprecation status of the type.
    --
    -- Valid values include:
    --
    --     * @LIVE@ : The type is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.
    --
    --
    --     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
    deprecatedStatus :: Lude.Maybe DeprecatedStatus,
    -- | When the specified type version was registered.
    timeCreated :: Lude.Maybe Lude.DateTime,
    -- | The kind of type.
    --
    -- Currently the only valid value is @RESOURCE@ .
    type' :: Lude.Maybe RegistryType,
    -- | Whether the specified type version is set as the default version.
    isDefaultVersion :: Lude.Maybe Lude.Bool,
    -- | The description of the registered type.
    description :: Lude.Maybe Lude.Text,
    -- | The URL of the source code for the type.
    sourceURL :: Lude.Maybe Lude.Text,
    -- | The URL of a page providing detailed documentation for this type.
    documentationURL :: Lude.Maybe Lude.Text,
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
    provisioningType :: Lude.Maybe ProvisioningType,
    -- | Contains logging configuration information for a type.
    loggingConfig :: Lude.Maybe LoggingConfig,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTypeResponse' with the minimum fields required to make a request.
--
-- * 'lastUpdated' - When the specified type version was registered.
-- * 'typeName' - The name of the registered type.
-- * 'arn' - The Amazon Resource Name (ARN) of the type.
-- * 'executionRoleARN' - The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
-- * 'visibility' - The scope at which the type is visible and usable in CloudFormation operations.
--
-- Valid values include:
--
--     * @PRIVATE@ : The type is only visible and usable within the account in which it is registered. Currently, AWS CloudFormation marks any types you register as @PRIVATE@ .
--
--
--     * @PUBLIC@ : The type is publically visible and usable within any Amazon account.
--
--
-- * 'schema' - The schema that defines the type.
--
-- For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
-- * 'defaultVersionId' - The ID of the default version of the type. The default version is used when the type version is not specified.
--
-- To set the default version of a type, use @'SetTypeDefaultVersion' @ .
-- * 'deprecatedStatus' - The deprecation status of the type.
--
-- Valid values include:
--
--     * @LIVE@ : The type is registered and can be used in CloudFormation operations, dependent on its provisioning behavior and visibility scope.
--
--
--     * @DEPRECATED@ : The type has been deregistered and can no longer be used in CloudFormation operations.
--
--
-- * 'timeCreated' - When the specified type version was registered.
-- * 'type'' - The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
-- * 'isDefaultVersion' - Whether the specified type version is set as the default version.
-- * 'description' - The description of the registered type.
-- * 'sourceURL' - The URL of the source code for the type.
-- * 'documentationURL' - The URL of a page providing detailed documentation for this type.
-- * 'provisioningType' - The provisioning behavior of the type. AWS CloudFormation determines the provisioning type during registration, based on the types of handlers in the schema handler package submitted.
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
-- * 'loggingConfig' - Contains logging configuration information for a type.
-- * 'responseStatus' - The response status code.
mkDescribeTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTypeResponse
mkDescribeTypeResponse pResponseStatus_ =
  DescribeTypeResponse'
    { lastUpdated = Lude.Nothing,
      typeName = Lude.Nothing,
      arn = Lude.Nothing,
      executionRoleARN = Lude.Nothing,
      visibility = Lude.Nothing,
      schema = Lude.Nothing,
      defaultVersionId = Lude.Nothing,
      deprecatedStatus = Lude.Nothing,
      timeCreated = Lude.Nothing,
      type' = Lude.Nothing,
      isDefaultVersion = Lude.Nothing,
      description = Lude.Nothing,
      sourceURL = Lude.Nothing,
      documentationURL = Lude.Nothing,
      provisioningType = Lude.Nothing,
      loggingConfig = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the specified type version was registered.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLastUpdated :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.DateTime)
drsLastUpdated = Lens.lens (lastUpdated :: DescribeTypeResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {lastUpdated = a} :: DescribeTypeResponse)
{-# DEPRECATED drsLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | The name of the registered type.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTypeName :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Text)
drsTypeName = Lens.lens (typeName :: DescribeTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: DescribeTypeResponse)
{-# DEPRECATED drsTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | The Amazon Resource Name (ARN) of the type.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsARN :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Text)
drsARN = Lens.lens (arn :: DescribeTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeTypeResponse)
{-# DEPRECATED drsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM execution role used to register the type. If your resource type calls AWS APIs in any of its handlers, you must create an /<https:\/\/docs.aws.amazon.com\/IAM\/latest\/UserGuide\/id_roles.html IAM execution role> / that includes the necessary permissions to call those AWS APIs, and provision that execution role in your account. CloudFormation then assumes that execution role to provide your resource type with the appropriate credentials.
--
-- /Note:/ Consider using 'executionRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsExecutionRoleARN :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Text)
drsExecutionRoleARN = Lens.lens (executionRoleARN :: DescribeTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleARN = a} :: DescribeTypeResponse)
{-# DEPRECATED drsExecutionRoleARN "Use generic-lens or generic-optics with 'executionRoleARN' instead." #-}

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
drsVisibility :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Visibility)
drsVisibility = Lens.lens (visibility :: DescribeTypeResponse -> Lude.Maybe Visibility) (\s a -> s {visibility = a} :: DescribeTypeResponse)
{-# DEPRECATED drsVisibility "Use generic-lens or generic-optics with 'visibility' instead." #-}

-- | The schema that defines the type.
--
-- For more information on type schemas, see <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema> in the /CloudFormation CLI User Guide/ .
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsSchema :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Text)
drsSchema = Lens.lens (schema :: DescribeTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {schema = a} :: DescribeTypeResponse)
{-# DEPRECATED drsSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | The ID of the default version of the type. The default version is used when the type version is not specified.
--
-- To set the default version of a type, use @'SetTypeDefaultVersion' @ .
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDefaultVersionId :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Text)
drsDefaultVersionId = Lens.lens (defaultVersionId :: DescribeTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultVersionId = a} :: DescribeTypeResponse)
{-# DEPRECATED drsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

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
drsDeprecatedStatus :: Lens.Lens' DescribeTypeResponse (Lude.Maybe DeprecatedStatus)
drsDeprecatedStatus = Lens.lens (deprecatedStatus :: DescribeTypeResponse -> Lude.Maybe DeprecatedStatus) (\s a -> s {deprecatedStatus = a} :: DescribeTypeResponse)
{-# DEPRECATED drsDeprecatedStatus "Use generic-lens or generic-optics with 'deprecatedStatus' instead." #-}

-- | When the specified type version was registered.
--
-- /Note:/ Consider using 'timeCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTimeCreated :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.DateTime)
drsTimeCreated = Lens.lens (timeCreated :: DescribeTypeResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {timeCreated = a} :: DescribeTypeResponse)
{-# DEPRECATED drsTimeCreated "Use generic-lens or generic-optics with 'timeCreated' instead." #-}

-- | The kind of type.
--
-- Currently the only valid value is @RESOURCE@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsType :: Lens.Lens' DescribeTypeResponse (Lude.Maybe RegistryType)
drsType = Lens.lens (type' :: DescribeTypeResponse -> Lude.Maybe RegistryType) (\s a -> s {type' = a} :: DescribeTypeResponse)
{-# DEPRECATED drsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Whether the specified type version is set as the default version.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsIsDefaultVersion :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Bool)
drsIsDefaultVersion = Lens.lens (isDefaultVersion :: DescribeTypeResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: DescribeTypeResponse)
{-# DEPRECATED drsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The description of the registered type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Text)
drsDescription = Lens.lens (description :: DescribeTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeTypeResponse)
{-# DEPRECATED drsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The URL of the source code for the type.
--
-- /Note:/ Consider using 'sourceURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsSourceURL :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Text)
drsSourceURL = Lens.lens (sourceURL :: DescribeTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {sourceURL = a} :: DescribeTypeResponse)
{-# DEPRECATED drsSourceURL "Use generic-lens or generic-optics with 'sourceURL' instead." #-}

-- | The URL of a page providing detailed documentation for this type.
--
-- /Note:/ Consider using 'documentationURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDocumentationURL :: Lens.Lens' DescribeTypeResponse (Lude.Maybe Lude.Text)
drsDocumentationURL = Lens.lens (documentationURL :: DescribeTypeResponse -> Lude.Maybe Lude.Text) (\s a -> s {documentationURL = a} :: DescribeTypeResponse)
{-# DEPRECATED drsDocumentationURL "Use generic-lens or generic-optics with 'documentationURL' instead." #-}

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
drsProvisioningType :: Lens.Lens' DescribeTypeResponse (Lude.Maybe ProvisioningType)
drsProvisioningType = Lens.lens (provisioningType :: DescribeTypeResponse -> Lude.Maybe ProvisioningType) (\s a -> s {provisioningType = a} :: DescribeTypeResponse)
{-# DEPRECATED drsProvisioningType "Use generic-lens or generic-optics with 'provisioningType' instead." #-}

-- | Contains logging configuration information for a type.
--
-- /Note:/ Consider using 'loggingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsLoggingConfig :: Lens.Lens' DescribeTypeResponse (Lude.Maybe LoggingConfig)
drsLoggingConfig = Lens.lens (loggingConfig :: DescribeTypeResponse -> Lude.Maybe LoggingConfig) (\s a -> s {loggingConfig = a} :: DescribeTypeResponse)
{-# DEPRECATED drsLoggingConfig "Use generic-lens or generic-optics with 'loggingConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeTypeResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTypeResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

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
-- Module      : Network.AWS.CloudFormation.DescribeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about an extension that has been
-- registered.
--
-- If you specify a @VersionId@, @DescribeType@ returns information about
-- that specific extension version. Otherwise, it returns information about
-- the default extension version.
module Network.AWS.CloudFormation.DescribeType
  ( -- * Creating a Request
    DescribeType (..),
    newDescribeType,

    -- * Request Lenses
    describeType_typeName,
    describeType_arn,
    describeType_versionId,
    describeType_type,

    -- * Destructuring the Response
    DescribeTypeResponse (..),
    newDescribeTypeResponse,

    -- * Response Lenses
    describeTypeResponse_typeName,
    describeTypeResponse_schema,
    describeTypeResponse_loggingConfig,
    describeTypeResponse_executionRoleArn,
    describeTypeResponse_arn,
    describeTypeResponse_deprecatedStatus,
    describeTypeResponse_lastUpdated,
    describeTypeResponse_defaultVersionId,
    describeTypeResponse_documentationUrl,
    describeTypeResponse_provisioningType,
    describeTypeResponse_visibility,
    describeTypeResponse_description,
    describeTypeResponse_sourceUrl,
    describeTypeResponse_isDefaultVersion,
    describeTypeResponse_type,
    describeTypeResponse_timeCreated,
    describeTypeResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeType' smart constructor.
data DescribeType = DescribeType'
  { -- | The name of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    arn :: Core.Maybe Core.Text,
    -- | The ID of a specific version of the extension. The version ID is the
    -- value at the end of the Amazon Resource Name (ARN) assigned to the
    -- extension version when it is registered.
    --
    -- If you specify a @VersionId@, @DescribeType@ returns information about
    -- that specific extension version. Otherwise, it returns information about
    -- the default extension version.
    versionId :: Core.Maybe Core.Text,
    -- | The kind of extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    type' :: Core.Maybe RegistryType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'describeType_typeName' - The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'arn', 'describeType_arn' - The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'versionId', 'describeType_versionId' - The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
--
-- If you specify a @VersionId@, @DescribeType@ returns information about
-- that specific extension version. Otherwise, it returns information about
-- the default extension version.
--
-- 'type'', 'describeType_type' - The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
newDescribeType ::
  DescribeType
newDescribeType =
  DescribeType'
    { typeName = Core.Nothing,
      arn = Core.Nothing,
      versionId = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
describeType_typeName :: Lens.Lens' DescribeType (Core.Maybe Core.Text)
describeType_typeName = Lens.lens (\DescribeType' {typeName} -> typeName) (\s@DescribeType' {} a -> s {typeName = a} :: DescribeType)

-- | The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
describeType_arn :: Lens.Lens' DescribeType (Core.Maybe Core.Text)
describeType_arn = Lens.lens (\DescribeType' {arn} -> arn) (\s@DescribeType' {} a -> s {arn = a} :: DescribeType)

-- | The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
--
-- If you specify a @VersionId@, @DescribeType@ returns information about
-- that specific extension version. Otherwise, it returns information about
-- the default extension version.
describeType_versionId :: Lens.Lens' DescribeType (Core.Maybe Core.Text)
describeType_versionId = Lens.lens (\DescribeType' {versionId} -> versionId) (\s@DescribeType' {} a -> s {versionId = a} :: DescribeType)

-- | The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
describeType_type :: Lens.Lens' DescribeType (Core.Maybe RegistryType)
describeType_type = Lens.lens (\DescribeType' {type'} -> type') (\s@DescribeType' {} a -> s {type' = a} :: DescribeType)

instance Core.AWSRequest DescribeType where
  type AWSResponse DescribeType = DescribeTypeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeTypeResult"
      ( \s h x ->
          DescribeTypeResponse'
            Core.<$> (x Core..@? "TypeName")
            Core.<*> (x Core..@? "Schema")
            Core.<*> (x Core..@? "LoggingConfig")
            Core.<*> (x Core..@? "ExecutionRoleArn")
            Core.<*> (x Core..@? "Arn")
            Core.<*> (x Core..@? "DeprecatedStatus")
            Core.<*> (x Core..@? "LastUpdated")
            Core.<*> (x Core..@? "DefaultVersionId")
            Core.<*> (x Core..@? "DocumentationUrl")
            Core.<*> (x Core..@? "ProvisioningType")
            Core.<*> (x Core..@? "Visibility")
            Core.<*> (x Core..@? "Description")
            Core.<*> (x Core..@? "SourceUrl")
            Core.<*> (x Core..@? "IsDefaultVersion")
            Core.<*> (x Core..@? "Type")
            Core.<*> (x Core..@? "TimeCreated")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeType

instance Core.NFData DescribeType

instance Core.ToHeaders DescribeType where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeType where
  toPath = Core.const "/"

instance Core.ToQuery DescribeType where
  toQuery DescribeType' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeType" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "TypeName" Core.=: typeName,
        "Arn" Core.=: arn,
        "VersionId" Core.=: versionId,
        "Type" Core.=: type'
      ]

-- | /See:/ 'newDescribeTypeResponse' smart constructor.
data DescribeTypeResponse = DescribeTypeResponse'
  { -- | The name of the registered extension.
    typeName :: Core.Maybe Core.Text,
    -- | The schema that defines the extension.
    --
    -- For more information on extension schemas, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema>
    -- in the /CloudFormation CLI User Guide/.
    schema :: Core.Maybe Core.Text,
    -- | Contains logging configuration information for an extension.
    loggingConfig :: Core.Maybe LoggingConfig,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to
    -- register the extension. If your resource type calls AWS APIs in any of
    -- its handlers, you must create an
    -- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
    -- that includes the necessary permissions to call those AWS APIs, and
    -- provision that execution role in your account. CloudFormation then
    -- assumes that execution role to provide your extension with the
    -- appropriate credentials.
    executionRoleArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the extension.
    arn :: Core.Maybe Core.Text,
    -- | The deprecation status of the extension version.
    --
    -- Valid values include:
    --
    -- -   @LIVE@: The extension is registered and can be used in
    --     CloudFormation operations, dependent on its provisioning behavior
    --     and visibility scope.
    --
    -- -   @DEPRECATED@: The extension has been deregistered and can no longer
    --     be used in CloudFormation operations.
    deprecatedStatus :: Core.Maybe DeprecatedStatus,
    -- | When the specified extension version was registered.
    lastUpdated :: Core.Maybe Core.ISO8601,
    -- | The ID of the default version of the extension. The default version is
    -- used when the extension version is not specified.
    --
    -- To set the default version of an extension, use
    -- @ SetTypeDefaultVersion @.
    defaultVersionId :: Core.Maybe Core.Text,
    -- | The URL of a page providing detailed documentation for this extension.
    documentationUrl :: Core.Maybe Core.Text,
    -- | The provisioning behavior of the extension. AWS CloudFormation
    -- determines the provisioning type during registration, based on the types
    -- of handlers in the schema handler package submitted.
    --
    -- Valid values include:
    --
    -- -   @FULLY_MUTABLE@: The extension includes an update handler to process
    --     updates to the extension during stack update operations.
    --
    -- -   @IMMUTABLE@: The extension does not include an update handler, so
    --     the extension cannot be updated and must instead be replaced during
    --     stack update operations.
    --
    -- -   @NON_PROVISIONABLE@: The extension does not include all of the
    --     following handlers, and therefore cannot actually be provisioned.
    --
    --     -   create
    --
    --     -   read
    --
    --     -   delete
    provisioningType :: Core.Maybe ProvisioningType,
    -- | The scope at which the extension is visible and usable in CloudFormation
    -- operations.
    --
    -- Valid values include:
    --
    -- -   @PRIVATE@: The extension is only visible and usable within the
    --     account in which it is registered. Currently, AWS CloudFormation
    --     marks any types you register as @PRIVATE@.
    --
    -- -   @PUBLIC@: The extension is publically visible and usable within any
    --     Amazon account.
    visibility :: Core.Maybe Visibility,
    -- | The description of the registered extension.
    description :: Core.Maybe Core.Text,
    -- | The URL of the source code for the extension.
    sourceUrl :: Core.Maybe Core.Text,
    -- | Whether the specified extension version is set as the default version.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The kind of extension.
    type' :: Core.Maybe RegistryType,
    -- | When the specified extension version was registered.
    timeCreated :: Core.Maybe Core.ISO8601,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'describeTypeResponse_typeName' - The name of the registered extension.
--
-- 'schema', 'describeTypeResponse_schema' - The schema that defines the extension.
--
-- For more information on extension schemas, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema>
-- in the /CloudFormation CLI User Guide/.
--
-- 'loggingConfig', 'describeTypeResponse_loggingConfig' - Contains logging configuration information for an extension.
--
-- 'executionRoleArn', 'describeTypeResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM execution role used to
-- register the extension. If your resource type calls AWS APIs in any of
-- its handlers, you must create an
-- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
-- that includes the necessary permissions to call those AWS APIs, and
-- provision that execution role in your account. CloudFormation then
-- assumes that execution role to provide your extension with the
-- appropriate credentials.
--
-- 'arn', 'describeTypeResponse_arn' - The Amazon Resource Name (ARN) of the extension.
--
-- 'deprecatedStatus', 'describeTypeResponse_deprecatedStatus' - The deprecation status of the extension version.
--
-- Valid values include:
--
-- -   @LIVE@: The extension is registered and can be used in
--     CloudFormation operations, dependent on its provisioning behavior
--     and visibility scope.
--
-- -   @DEPRECATED@: The extension has been deregistered and can no longer
--     be used in CloudFormation operations.
--
-- 'lastUpdated', 'describeTypeResponse_lastUpdated' - When the specified extension version was registered.
--
-- 'defaultVersionId', 'describeTypeResponse_defaultVersionId' - The ID of the default version of the extension. The default version is
-- used when the extension version is not specified.
--
-- To set the default version of an extension, use
-- @ SetTypeDefaultVersion @.
--
-- 'documentationUrl', 'describeTypeResponse_documentationUrl' - The URL of a page providing detailed documentation for this extension.
--
-- 'provisioningType', 'describeTypeResponse_provisioningType' - The provisioning behavior of the extension. AWS CloudFormation
-- determines the provisioning type during registration, based on the types
-- of handlers in the schema handler package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The extension includes an update handler to process
--     updates to the extension during stack update operations.
--
-- -   @IMMUTABLE@: The extension does not include an update handler, so
--     the extension cannot be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The extension does not include all of the
--     following handlers, and therefore cannot actually be provisioned.
--
--     -   create
--
--     -   read
--
--     -   delete
--
-- 'visibility', 'describeTypeResponse_visibility' - The scope at which the extension is visible and usable in CloudFormation
-- operations.
--
-- Valid values include:
--
-- -   @PRIVATE@: The extension is only visible and usable within the
--     account in which it is registered. Currently, AWS CloudFormation
--     marks any types you register as @PRIVATE@.
--
-- -   @PUBLIC@: The extension is publically visible and usable within any
--     Amazon account.
--
-- 'description', 'describeTypeResponse_description' - The description of the registered extension.
--
-- 'sourceUrl', 'describeTypeResponse_sourceUrl' - The URL of the source code for the extension.
--
-- 'isDefaultVersion', 'describeTypeResponse_isDefaultVersion' - Whether the specified extension version is set as the default version.
--
-- 'type'', 'describeTypeResponse_type' - The kind of extension.
--
-- 'timeCreated', 'describeTypeResponse_timeCreated' - When the specified extension version was registered.
--
-- 'httpStatus', 'describeTypeResponse_httpStatus' - The response's http status code.
newDescribeTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTypeResponse
newDescribeTypeResponse pHttpStatus_ =
  DescribeTypeResponse'
    { typeName = Core.Nothing,
      schema = Core.Nothing,
      loggingConfig = Core.Nothing,
      executionRoleArn = Core.Nothing,
      arn = Core.Nothing,
      deprecatedStatus = Core.Nothing,
      lastUpdated = Core.Nothing,
      defaultVersionId = Core.Nothing,
      documentationUrl = Core.Nothing,
      provisioningType = Core.Nothing,
      visibility = Core.Nothing,
      description = Core.Nothing,
      sourceUrl = Core.Nothing,
      isDefaultVersion = Core.Nothing,
      type' = Core.Nothing,
      timeCreated = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the registered extension.
describeTypeResponse_typeName :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Text)
describeTypeResponse_typeName = Lens.lens (\DescribeTypeResponse' {typeName} -> typeName) (\s@DescribeTypeResponse' {} a -> s {typeName = a} :: DescribeTypeResponse)

-- | The schema that defines the extension.
--
-- For more information on extension schemas, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema>
-- in the /CloudFormation CLI User Guide/.
describeTypeResponse_schema :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Text)
describeTypeResponse_schema = Lens.lens (\DescribeTypeResponse' {schema} -> schema) (\s@DescribeTypeResponse' {} a -> s {schema = a} :: DescribeTypeResponse)

-- | Contains logging configuration information for an extension.
describeTypeResponse_loggingConfig :: Lens.Lens' DescribeTypeResponse (Core.Maybe LoggingConfig)
describeTypeResponse_loggingConfig = Lens.lens (\DescribeTypeResponse' {loggingConfig} -> loggingConfig) (\s@DescribeTypeResponse' {} a -> s {loggingConfig = a} :: DescribeTypeResponse)

-- | The Amazon Resource Name (ARN) of the IAM execution role used to
-- register the extension. If your resource type calls AWS APIs in any of
-- its handlers, you must create an
-- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
-- that includes the necessary permissions to call those AWS APIs, and
-- provision that execution role in your account. CloudFormation then
-- assumes that execution role to provide your extension with the
-- appropriate credentials.
describeTypeResponse_executionRoleArn :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Text)
describeTypeResponse_executionRoleArn = Lens.lens (\DescribeTypeResponse' {executionRoleArn} -> executionRoleArn) (\s@DescribeTypeResponse' {} a -> s {executionRoleArn = a} :: DescribeTypeResponse)

-- | The Amazon Resource Name (ARN) of the extension.
describeTypeResponse_arn :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Text)
describeTypeResponse_arn = Lens.lens (\DescribeTypeResponse' {arn} -> arn) (\s@DescribeTypeResponse' {} a -> s {arn = a} :: DescribeTypeResponse)

-- | The deprecation status of the extension version.
--
-- Valid values include:
--
-- -   @LIVE@: The extension is registered and can be used in
--     CloudFormation operations, dependent on its provisioning behavior
--     and visibility scope.
--
-- -   @DEPRECATED@: The extension has been deregistered and can no longer
--     be used in CloudFormation operations.
describeTypeResponse_deprecatedStatus :: Lens.Lens' DescribeTypeResponse (Core.Maybe DeprecatedStatus)
describeTypeResponse_deprecatedStatus = Lens.lens (\DescribeTypeResponse' {deprecatedStatus} -> deprecatedStatus) (\s@DescribeTypeResponse' {} a -> s {deprecatedStatus = a} :: DescribeTypeResponse)

-- | When the specified extension version was registered.
describeTypeResponse_lastUpdated :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.UTCTime)
describeTypeResponse_lastUpdated = Lens.lens (\DescribeTypeResponse' {lastUpdated} -> lastUpdated) (\s@DescribeTypeResponse' {} a -> s {lastUpdated = a} :: DescribeTypeResponse) Core.. Lens.mapping Core._Time

-- | The ID of the default version of the extension. The default version is
-- used when the extension version is not specified.
--
-- To set the default version of an extension, use
-- @ SetTypeDefaultVersion @.
describeTypeResponse_defaultVersionId :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Text)
describeTypeResponse_defaultVersionId = Lens.lens (\DescribeTypeResponse' {defaultVersionId} -> defaultVersionId) (\s@DescribeTypeResponse' {} a -> s {defaultVersionId = a} :: DescribeTypeResponse)

-- | The URL of a page providing detailed documentation for this extension.
describeTypeResponse_documentationUrl :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Text)
describeTypeResponse_documentationUrl = Lens.lens (\DescribeTypeResponse' {documentationUrl} -> documentationUrl) (\s@DescribeTypeResponse' {} a -> s {documentationUrl = a} :: DescribeTypeResponse)

-- | The provisioning behavior of the extension. AWS CloudFormation
-- determines the provisioning type during registration, based on the types
-- of handlers in the schema handler package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The extension includes an update handler to process
--     updates to the extension during stack update operations.
--
-- -   @IMMUTABLE@: The extension does not include an update handler, so
--     the extension cannot be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The extension does not include all of the
--     following handlers, and therefore cannot actually be provisioned.
--
--     -   create
--
--     -   read
--
--     -   delete
describeTypeResponse_provisioningType :: Lens.Lens' DescribeTypeResponse (Core.Maybe ProvisioningType)
describeTypeResponse_provisioningType = Lens.lens (\DescribeTypeResponse' {provisioningType} -> provisioningType) (\s@DescribeTypeResponse' {} a -> s {provisioningType = a} :: DescribeTypeResponse)

-- | The scope at which the extension is visible and usable in CloudFormation
-- operations.
--
-- Valid values include:
--
-- -   @PRIVATE@: The extension is only visible and usable within the
--     account in which it is registered. Currently, AWS CloudFormation
--     marks any types you register as @PRIVATE@.
--
-- -   @PUBLIC@: The extension is publically visible and usable within any
--     Amazon account.
describeTypeResponse_visibility :: Lens.Lens' DescribeTypeResponse (Core.Maybe Visibility)
describeTypeResponse_visibility = Lens.lens (\DescribeTypeResponse' {visibility} -> visibility) (\s@DescribeTypeResponse' {} a -> s {visibility = a} :: DescribeTypeResponse)

-- | The description of the registered extension.
describeTypeResponse_description :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Text)
describeTypeResponse_description = Lens.lens (\DescribeTypeResponse' {description} -> description) (\s@DescribeTypeResponse' {} a -> s {description = a} :: DescribeTypeResponse)

-- | The URL of the source code for the extension.
describeTypeResponse_sourceUrl :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Text)
describeTypeResponse_sourceUrl = Lens.lens (\DescribeTypeResponse' {sourceUrl} -> sourceUrl) (\s@DescribeTypeResponse' {} a -> s {sourceUrl = a} :: DescribeTypeResponse)

-- | Whether the specified extension version is set as the default version.
describeTypeResponse_isDefaultVersion :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.Bool)
describeTypeResponse_isDefaultVersion = Lens.lens (\DescribeTypeResponse' {isDefaultVersion} -> isDefaultVersion) (\s@DescribeTypeResponse' {} a -> s {isDefaultVersion = a} :: DescribeTypeResponse)

-- | The kind of extension.
describeTypeResponse_type :: Lens.Lens' DescribeTypeResponse (Core.Maybe RegistryType)
describeTypeResponse_type = Lens.lens (\DescribeTypeResponse' {type'} -> type') (\s@DescribeTypeResponse' {} a -> s {type' = a} :: DescribeTypeResponse)

-- | When the specified extension version was registered.
describeTypeResponse_timeCreated :: Lens.Lens' DescribeTypeResponse (Core.Maybe Core.UTCTime)
describeTypeResponse_timeCreated = Lens.lens (\DescribeTypeResponse' {timeCreated} -> timeCreated) (\s@DescribeTypeResponse' {} a -> s {timeCreated = a} :: DescribeTypeResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
describeTypeResponse_httpStatus :: Lens.Lens' DescribeTypeResponse Core.Int
describeTypeResponse_httpStatus = Lens.lens (\DescribeTypeResponse' {httpStatus} -> httpStatus) (\s@DescribeTypeResponse' {} a -> s {httpStatus = a} :: DescribeTypeResponse)

instance Core.NFData DescribeTypeResponse

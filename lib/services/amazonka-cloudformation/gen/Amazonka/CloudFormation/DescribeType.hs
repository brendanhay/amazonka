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
-- Module      : Amazonka.CloudFormation.DescribeType
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudFormation.DescribeType
  ( -- * Creating a Request
    DescribeType (..),
    newDescribeType,

    -- * Request Lenses
    describeType_type,
    describeType_arn,
    describeType_publicVersionNumber,
    describeType_publisherId,
    describeType_typeName,
    describeType_versionId,

    -- * Destructuring the Response
    DescribeTypeResponse (..),
    newDescribeTypeResponse,

    -- * Response Lenses
    describeTypeResponse_typeTestsStatusDescription,
    describeTypeResponse_deprecatedStatus,
    describeTypeResponse_isDefaultVersion,
    describeTypeResponse_defaultVersionId,
    describeTypeResponse_type,
    describeTypeResponse_documentationUrl,
    describeTypeResponse_requiredActivatedTypes,
    describeTypeResponse_configurationSchema,
    describeTypeResponse_visibility,
    describeTypeResponse_autoUpdate,
    describeTypeResponse_arn,
    describeTypeResponse_timeCreated,
    describeTypeResponse_publicVersionNumber,
    describeTypeResponse_publisherId,
    describeTypeResponse_typeName,
    describeTypeResponse_description,
    describeTypeResponse_lastUpdated,
    describeTypeResponse_originalTypeName,
    describeTypeResponse_provisioningType,
    describeTypeResponse_latestPublicVersion,
    describeTypeResponse_schema,
    describeTypeResponse_isActivated,
    describeTypeResponse_executionRoleArn,
    describeTypeResponse_typeTestsStatus,
    describeTypeResponse_originalTypeArn,
    describeTypeResponse_sourceUrl,
    describeTypeResponse_loggingConfig,
    describeTypeResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeType' smart constructor.
data DescribeType = DescribeType'
  { -- | The kind of extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    type' :: Prelude.Maybe RegistryType,
    -- | The Amazon Resource Name (ARN) of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version number of a public third-party extension.
    publicVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The publisher ID of the extension publisher.
    --
    -- Extensions provided by Amazon Web Services are not assigned a publisher
    -- ID.
    publisherId :: Prelude.Maybe Prelude.Text,
    -- | The name of the extension.
    --
    -- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The ID of a specific version of the extension. The version ID is the
    -- value at the end of the Amazon Resource Name (ARN) assigned to the
    -- extension version when it is registered.
    --
    -- If you specify a @VersionId@, @DescribeType@ returns information about
    -- that specific extension version. Otherwise, it returns information about
    -- the default extension version.
    versionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'describeType_type' - The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'arn', 'describeType_arn' - The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
--
-- 'publicVersionNumber', 'describeType_publicVersionNumber' - The version number of a public third-party extension.
--
-- 'publisherId', 'describeType_publisherId' - The publisher ID of the extension publisher.
--
-- Extensions provided by Amazon Web Services are not assigned a publisher
-- ID.
--
-- 'typeName', 'describeType_typeName' - The name of the extension.
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
newDescribeType ::
  DescribeType
newDescribeType =
  DescribeType'
    { type' = Prelude.Nothing,
      arn = Prelude.Nothing,
      publicVersionNumber = Prelude.Nothing,
      publisherId = Prelude.Nothing,
      typeName = Prelude.Nothing,
      versionId = Prelude.Nothing
    }

-- | The kind of extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
describeType_type :: Lens.Lens' DescribeType (Prelude.Maybe RegistryType)
describeType_type = Lens.lens (\DescribeType' {type'} -> type') (\s@DescribeType' {} a -> s {type' = a} :: DescribeType)

-- | The Amazon Resource Name (ARN) of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
describeType_arn :: Lens.Lens' DescribeType (Prelude.Maybe Prelude.Text)
describeType_arn = Lens.lens (\DescribeType' {arn} -> arn) (\s@DescribeType' {} a -> s {arn = a} :: DescribeType)

-- | The version number of a public third-party extension.
describeType_publicVersionNumber :: Lens.Lens' DescribeType (Prelude.Maybe Prelude.Text)
describeType_publicVersionNumber = Lens.lens (\DescribeType' {publicVersionNumber} -> publicVersionNumber) (\s@DescribeType' {} a -> s {publicVersionNumber = a} :: DescribeType)

-- | The publisher ID of the extension publisher.
--
-- Extensions provided by Amazon Web Services are not assigned a publisher
-- ID.
describeType_publisherId :: Lens.Lens' DescribeType (Prelude.Maybe Prelude.Text)
describeType_publisherId = Lens.lens (\DescribeType' {publisherId} -> publisherId) (\s@DescribeType' {} a -> s {publisherId = a} :: DescribeType)

-- | The name of the extension.
--
-- Conditional: You must specify either @TypeName@ and @Type@, or @Arn@.
describeType_typeName :: Lens.Lens' DescribeType (Prelude.Maybe Prelude.Text)
describeType_typeName = Lens.lens (\DescribeType' {typeName} -> typeName) (\s@DescribeType' {} a -> s {typeName = a} :: DescribeType)

-- | The ID of a specific version of the extension. The version ID is the
-- value at the end of the Amazon Resource Name (ARN) assigned to the
-- extension version when it is registered.
--
-- If you specify a @VersionId@, @DescribeType@ returns information about
-- that specific extension version. Otherwise, it returns information about
-- the default extension version.
describeType_versionId :: Lens.Lens' DescribeType (Prelude.Maybe Prelude.Text)
describeType_versionId = Lens.lens (\DescribeType' {versionId} -> versionId) (\s@DescribeType' {} a -> s {versionId = a} :: DescribeType)

instance Core.AWSRequest DescribeType where
  type AWSResponse DescribeType = DescribeTypeResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTypeResult"
      ( \s h x ->
          DescribeTypeResponse'
            Prelude.<$> (x Data..@? "TypeTestsStatusDescription")
            Prelude.<*> (x Data..@? "DeprecatedStatus")
            Prelude.<*> (x Data..@? "IsDefaultVersion")
            Prelude.<*> (x Data..@? "DefaultVersionId")
            Prelude.<*> (x Data..@? "Type")
            Prelude.<*> (x Data..@? "DocumentationUrl")
            Prelude.<*> ( x Data..@? "RequiredActivatedTypes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "ConfigurationSchema")
            Prelude.<*> (x Data..@? "Visibility")
            Prelude.<*> (x Data..@? "AutoUpdate")
            Prelude.<*> (x Data..@? "Arn")
            Prelude.<*> (x Data..@? "TimeCreated")
            Prelude.<*> (x Data..@? "PublicVersionNumber")
            Prelude.<*> (x Data..@? "PublisherId")
            Prelude.<*> (x Data..@? "TypeName")
            Prelude.<*> (x Data..@? "Description")
            Prelude.<*> (x Data..@? "LastUpdated")
            Prelude.<*> (x Data..@? "OriginalTypeName")
            Prelude.<*> (x Data..@? "ProvisioningType")
            Prelude.<*> (x Data..@? "LatestPublicVersion")
            Prelude.<*> (x Data..@? "Schema")
            Prelude.<*> (x Data..@? "IsActivated")
            Prelude.<*> (x Data..@? "ExecutionRoleArn")
            Prelude.<*> (x Data..@? "TypeTestsStatus")
            Prelude.<*> (x Data..@? "OriginalTypeArn")
            Prelude.<*> (x Data..@? "SourceUrl")
            Prelude.<*> (x Data..@? "LoggingConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeType where
  hashWithSalt _salt DescribeType' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` publicVersionNumber
      `Prelude.hashWithSalt` publisherId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData DescribeType where
  rnf DescribeType' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf publicVersionNumber
      `Prelude.seq` Prelude.rnf publisherId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToHeaders DescribeType where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeType where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeType where
  toQuery DescribeType' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeType" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Type" Data.=: type',
        "Arn" Data.=: arn,
        "PublicVersionNumber" Data.=: publicVersionNumber,
        "PublisherId" Data.=: publisherId,
        "TypeName" Data.=: typeName,
        "VersionId" Data.=: versionId
      ]

-- | /See:/ 'newDescribeTypeResponse' smart constructor.
data DescribeTypeResponse = DescribeTypeResponse'
  { -- | The description of the test status. To return the extension test status
    -- of a specific extension version, you must specify @VersionId@.
    --
    -- This applies only to registered private extension versions.
    -- CloudFormation doesn\'t return this information for public extensions,
    -- whether they are activated in your account.
    typeTestsStatusDescription :: Prelude.Maybe Prelude.Text,
    -- | The deprecation status of the extension version.
    --
    -- Valid values include:
    --
    -- -   @LIVE@: The extension is activated or registered and can be used in
    --     CloudFormation operations, dependent on its provisioning behavior
    --     and visibility scope.
    --
    -- -   @DEPRECATED@: The extension has been deactivated or deregistered and
    --     can no longer be used in CloudFormation operations.
    --
    -- For public third-party extensions, CloudFormation returns @null@.
    deprecatedStatus :: Prelude.Maybe DeprecatedStatus,
    -- | Whether the specified extension version is set as the default version.
    --
    -- This applies only to private extensions you have registered in your
    -- account, and extensions published by Amazon Web Services. For public
    -- third-party extensions, whether they are activated in your account,
    -- CloudFormation returns @null@.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the default version of the extension. The default version is
    -- used when the extension version isn\'t specified.
    --
    -- This applies only to private extensions you have registered in your
    -- account. For public extensions, both those provided by Amazon Web
    -- Services and published by third parties, CloudFormation returns @null@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
    --
    -- To set the default version of an extension, use
    -- @ SetTypeDefaultVersion @.
    defaultVersionId :: Prelude.Maybe Prelude.Text,
    -- | The kind of extension.
    type' :: Prelude.Maybe RegistryType,
    -- | The URL of a page providing detailed documentation for this extension.
    documentationUrl :: Prelude.Maybe Prelude.Text,
    -- | For extensions that are modules, the public third-party extensions that
    -- must be activated in your account in order for the module itself to be
    -- activated.
    requiredActivatedTypes :: Prelude.Maybe [RequiredActivatedType],
    -- | A JSON string that represent the current configuration data for the
    -- extension in this account and region.
    --
    -- To set the configuration data for an extension, use
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_SetTypeConfiguration.html SetTypeConfiguration>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-register.html#registry-set-configuration Configuring extensions at the account level>
    -- in the /CloudFormation User Guide/.
    configurationSchema :: Prelude.Maybe Prelude.Text,
    -- | The scope at which the extension is visible and usable in CloudFormation
    -- operations.
    --
    -- Valid values include:
    --
    -- -   @PRIVATE@: The extension is only visible and usable within the
    --     account in which it is registered. CloudFormation marks any
    --     extensions you register as @PRIVATE@.
    --
    -- -   @PUBLIC@: The extension is publicly visible and usable within any
    --     Amazon Web Services account.
    visibility :: Prelude.Maybe Visibility,
    -- | Whether CloudFormation automatically updates the extension in this
    -- account and region when a new /minor/ version is published by the
    -- extension publisher. Major versions released by the publisher must be
    -- manually updated. For more information, see
    -- <AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable Activating public extensions for use in your account>
    -- in the /CloudFormation User Guide/.
    autoUpdate :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the extension.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the specified private extension version was registered or activated
    -- in your account.
    timeCreated :: Prelude.Maybe Data.ISO8601,
    -- | The version number of a public third-party extension.
    --
    -- This applies only if you specify a public extension you have activated
    -- in your account, or specify a public extension without specifying a
    -- version. For all other extensions, CloudFormation returns @null@.
    publicVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The publisher ID of the extension publisher.
    --
    -- This applies only to public third-party extensions. For private
    -- registered extensions, and extensions provided by Amazon Web Services,
    -- CloudFormation returns @null@.
    publisherId :: Prelude.Maybe Prelude.Text,
    -- | The name of the extension.
    --
    -- If the extension is a public third-party type you have activated with a
    -- type name alias, CloudFormation returns the type name alias. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html ActivateType>.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The description of the extension.
    description :: Prelude.Maybe Prelude.Text,
    -- | When the specified extension version was registered. This applies only
    -- to:
    --
    -- -   Private extensions you have registered in your account. For more
    --     information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
    --
    -- -   Public extensions you have activated in your account with
    --     auto-update specified. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html ActivateType>.
    lastUpdated :: Prelude.Maybe Data.ISO8601,
    -- | For public extensions that have been activated for this account and
    -- region, the type name of the public extension.
    --
    -- If you specified a @TypeNameAlias@ when enabling the extension in this
    -- account and region, CloudFormation treats that alias as the extension\'s
    -- type name within the account and region, not the type name of the public
    -- extension. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-alias Specifying aliases to refer to extensions>
    -- in the /CloudFormation User Guide/.
    originalTypeName :: Prelude.Maybe Prelude.Text,
    -- | For resource type extensions, the provisioning behavior of the resource
    -- type. CloudFormation determines the provisioning type during
    -- registration, based on the types of handlers in the schema handler
    -- package submitted.
    --
    -- Valid values include:
    --
    -- -   @FULLY_MUTABLE@: The resource type includes an update handler to
    --     process updates to the type during stack update operations.
    --
    -- -   @IMMUTABLE@: The resource type doesn\'t include an update handler,
    --     so the type can\'t be updated and must instead be replaced during
    --     stack update operations.
    --
    -- -   @NON_PROVISIONABLE@: The resource type doesn\'t include all the
    --     following handlers, and therefore can\'t actually be provisioned.
    --
    --     -   create
    --
    --     -   read
    --
    --     -   delete
    provisioningType :: Prelude.Maybe ProvisioningType,
    -- | The latest version of a public extension /that is available/ for use.
    --
    -- This only applies if you specify a public extension, and you don\'t
    -- specify a version. For all other requests, CloudFormation returns
    -- @null@.
    latestPublicVersion :: Prelude.Maybe Prelude.Text,
    -- | The schema that defines the extension.
    --
    -- For more information about extension schemas, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema>
    -- in the /CloudFormation CLI User Guide/.
    schema :: Prelude.Maybe Prelude.Text,
    -- | Whether the extension is activated in the account and region.
    --
    -- This only applies to public third-party extensions. For all other
    -- extensions, CloudFormation returns @null@.
    isActivated :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM execution role used to
    -- register the extension. This applies only to private extensions you have
    -- registered in your account. For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
    --
    -- If the registered extension calls any Amazon Web Services APIs, you must
    -- create an
    -- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
    -- that includes the necessary permissions to call those Amazon Web
    -- Services APIs, and provision that execution role in your account.
    -- CloudFormation then assumes that execution role to provide your
    -- extension with the appropriate credentials.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The contract test status of the registered extension version. To return
    -- the extension test status of a specific extension version, you must
    -- specify @VersionId@.
    --
    -- This applies only to registered private extension versions.
    -- CloudFormation doesn\'t return this information for public extensions,
    -- whether they are activated in your account.
    --
    -- -   @PASSED@: The extension has passed all its contract tests.
    --
    --     An extension must have a test status of @PASSED@ before it can be
    --     published. For more information, see
    --     <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-publish.html Publishing extensions to make them available for public use>
    --     in the /CloudFormation Command Line Interface User Guide/.
    --
    -- -   @FAILED@: The extension has failed one or more contract tests.
    --
    -- -   @IN_PROGRESS@: Contract tests are currently being performed on the
    --     extension.
    --
    -- -   @NOT_TESTED@: Contract tests haven\'t been performed on the
    --     extension.
    typeTestsStatus :: Prelude.Maybe TypeTestsStatus,
    -- | For public extensions that have been activated for this account and
    -- region, the Amazon Resource Name (ARN) of the public extension.
    originalTypeArn :: Prelude.Maybe Prelude.Text,
    -- | The URL of the source code for the extension.
    sourceUrl :: Prelude.Maybe Prelude.Text,
    -- | Contains logging configuration information for private extensions. This
    -- applies only to private extensions you have registered in your account.
    -- For public extensions, both those provided by Amazon Web Services and
    -- published by third parties, CloudFormation returns @null@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
    loggingConfig :: Prelude.Maybe LoggingConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeTestsStatusDescription', 'describeTypeResponse_typeTestsStatusDescription' - The description of the test status. To return the extension test status
-- of a specific extension version, you must specify @VersionId@.
--
-- This applies only to registered private extension versions.
-- CloudFormation doesn\'t return this information for public extensions,
-- whether they are activated in your account.
--
-- 'deprecatedStatus', 'describeTypeResponse_deprecatedStatus' - The deprecation status of the extension version.
--
-- Valid values include:
--
-- -   @LIVE@: The extension is activated or registered and can be used in
--     CloudFormation operations, dependent on its provisioning behavior
--     and visibility scope.
--
-- -   @DEPRECATED@: The extension has been deactivated or deregistered and
--     can no longer be used in CloudFormation operations.
--
-- For public third-party extensions, CloudFormation returns @null@.
--
-- 'isDefaultVersion', 'describeTypeResponse_isDefaultVersion' - Whether the specified extension version is set as the default version.
--
-- This applies only to private extensions you have registered in your
-- account, and extensions published by Amazon Web Services. For public
-- third-party extensions, whether they are activated in your account,
-- CloudFormation returns @null@.
--
-- 'defaultVersionId', 'describeTypeResponse_defaultVersionId' - The ID of the default version of the extension. The default version is
-- used when the extension version isn\'t specified.
--
-- This applies only to private extensions you have registered in your
-- account. For public extensions, both those provided by Amazon Web
-- Services and published by third parties, CloudFormation returns @null@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- To set the default version of an extension, use
-- @ SetTypeDefaultVersion @.
--
-- 'type'', 'describeTypeResponse_type' - The kind of extension.
--
-- 'documentationUrl', 'describeTypeResponse_documentationUrl' - The URL of a page providing detailed documentation for this extension.
--
-- 'requiredActivatedTypes', 'describeTypeResponse_requiredActivatedTypes' - For extensions that are modules, the public third-party extensions that
-- must be activated in your account in order for the module itself to be
-- activated.
--
-- 'configurationSchema', 'describeTypeResponse_configurationSchema' - A JSON string that represent the current configuration data for the
-- extension in this account and region.
--
-- To set the configuration data for an extension, use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_SetTypeConfiguration.html SetTypeConfiguration>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-register.html#registry-set-configuration Configuring extensions at the account level>
-- in the /CloudFormation User Guide/.
--
-- 'visibility', 'describeTypeResponse_visibility' - The scope at which the extension is visible and usable in CloudFormation
-- operations.
--
-- Valid values include:
--
-- -   @PRIVATE@: The extension is only visible and usable within the
--     account in which it is registered. CloudFormation marks any
--     extensions you register as @PRIVATE@.
--
-- -   @PUBLIC@: The extension is publicly visible and usable within any
--     Amazon Web Services account.
--
-- 'autoUpdate', 'describeTypeResponse_autoUpdate' - Whether CloudFormation automatically updates the extension in this
-- account and region when a new /minor/ version is published by the
-- extension publisher. Major versions released by the publisher must be
-- manually updated. For more information, see
-- <AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable Activating public extensions for use in your account>
-- in the /CloudFormation User Guide/.
--
-- 'arn', 'describeTypeResponse_arn' - The Amazon Resource Name (ARN) of the extension.
--
-- 'timeCreated', 'describeTypeResponse_timeCreated' - When the specified private extension version was registered or activated
-- in your account.
--
-- 'publicVersionNumber', 'describeTypeResponse_publicVersionNumber' - The version number of a public third-party extension.
--
-- This applies only if you specify a public extension you have activated
-- in your account, or specify a public extension without specifying a
-- version. For all other extensions, CloudFormation returns @null@.
--
-- 'publisherId', 'describeTypeResponse_publisherId' - The publisher ID of the extension publisher.
--
-- This applies only to public third-party extensions. For private
-- registered extensions, and extensions provided by Amazon Web Services,
-- CloudFormation returns @null@.
--
-- 'typeName', 'describeTypeResponse_typeName' - The name of the extension.
--
-- If the extension is a public third-party type you have activated with a
-- type name alias, CloudFormation returns the type name alias. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html ActivateType>.
--
-- 'description', 'describeTypeResponse_description' - The description of the extension.
--
-- 'lastUpdated', 'describeTypeResponse_lastUpdated' - When the specified extension version was registered. This applies only
-- to:
--
-- -   Private extensions you have registered in your account. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- -   Public extensions you have activated in your account with
--     auto-update specified. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html ActivateType>.
--
-- 'originalTypeName', 'describeTypeResponse_originalTypeName' - For public extensions that have been activated for this account and
-- region, the type name of the public extension.
--
-- If you specified a @TypeNameAlias@ when enabling the extension in this
-- account and region, CloudFormation treats that alias as the extension\'s
-- type name within the account and region, not the type name of the public
-- extension. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-alias Specifying aliases to refer to extensions>
-- in the /CloudFormation User Guide/.
--
-- 'provisioningType', 'describeTypeResponse_provisioningType' - For resource type extensions, the provisioning behavior of the resource
-- type. CloudFormation determines the provisioning type during
-- registration, based on the types of handlers in the schema handler
-- package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The resource type includes an update handler to
--     process updates to the type during stack update operations.
--
-- -   @IMMUTABLE@: The resource type doesn\'t include an update handler,
--     so the type can\'t be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The resource type doesn\'t include all the
--     following handlers, and therefore can\'t actually be provisioned.
--
--     -   create
--
--     -   read
--
--     -   delete
--
-- 'latestPublicVersion', 'describeTypeResponse_latestPublicVersion' - The latest version of a public extension /that is available/ for use.
--
-- This only applies if you specify a public extension, and you don\'t
-- specify a version. For all other requests, CloudFormation returns
-- @null@.
--
-- 'schema', 'describeTypeResponse_schema' - The schema that defines the extension.
--
-- For more information about extension schemas, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema>
-- in the /CloudFormation CLI User Guide/.
--
-- 'isActivated', 'describeTypeResponse_isActivated' - Whether the extension is activated in the account and region.
--
-- This only applies to public third-party extensions. For all other
-- extensions, CloudFormation returns @null@.
--
-- 'executionRoleArn', 'describeTypeResponse_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM execution role used to
-- register the extension. This applies only to private extensions you have
-- registered in your account. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- If the registered extension calls any Amazon Web Services APIs, you must
-- create an
-- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
-- that includes the necessary permissions to call those Amazon Web
-- Services APIs, and provision that execution role in your account.
-- CloudFormation then assumes that execution role to provide your
-- extension with the appropriate credentials.
--
-- 'typeTestsStatus', 'describeTypeResponse_typeTestsStatus' - The contract test status of the registered extension version. To return
-- the extension test status of a specific extension version, you must
-- specify @VersionId@.
--
-- This applies only to registered private extension versions.
-- CloudFormation doesn\'t return this information for public extensions,
-- whether they are activated in your account.
--
-- -   @PASSED@: The extension has passed all its contract tests.
--
--     An extension must have a test status of @PASSED@ before it can be
--     published. For more information, see
--     <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-publish.html Publishing extensions to make them available for public use>
--     in the /CloudFormation Command Line Interface User Guide/.
--
-- -   @FAILED@: The extension has failed one or more contract tests.
--
-- -   @IN_PROGRESS@: Contract tests are currently being performed on the
--     extension.
--
-- -   @NOT_TESTED@: Contract tests haven\'t been performed on the
--     extension.
--
-- 'originalTypeArn', 'describeTypeResponse_originalTypeArn' - For public extensions that have been activated for this account and
-- region, the Amazon Resource Name (ARN) of the public extension.
--
-- 'sourceUrl', 'describeTypeResponse_sourceUrl' - The URL of the source code for the extension.
--
-- 'loggingConfig', 'describeTypeResponse_loggingConfig' - Contains logging configuration information for private extensions. This
-- applies only to private extensions you have registered in your account.
-- For public extensions, both those provided by Amazon Web Services and
-- published by third parties, CloudFormation returns @null@. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- 'httpStatus', 'describeTypeResponse_httpStatus' - The response's http status code.
newDescribeTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTypeResponse
newDescribeTypeResponse pHttpStatus_ =
  DescribeTypeResponse'
    { typeTestsStatusDescription =
        Prelude.Nothing,
      deprecatedStatus = Prelude.Nothing,
      isDefaultVersion = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      type' = Prelude.Nothing,
      documentationUrl = Prelude.Nothing,
      requiredActivatedTypes = Prelude.Nothing,
      configurationSchema = Prelude.Nothing,
      visibility = Prelude.Nothing,
      autoUpdate = Prelude.Nothing,
      arn = Prelude.Nothing,
      timeCreated = Prelude.Nothing,
      publicVersionNumber = Prelude.Nothing,
      publisherId = Prelude.Nothing,
      typeName = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      originalTypeName = Prelude.Nothing,
      provisioningType = Prelude.Nothing,
      latestPublicVersion = Prelude.Nothing,
      schema = Prelude.Nothing,
      isActivated = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      typeTestsStatus = Prelude.Nothing,
      originalTypeArn = Prelude.Nothing,
      sourceUrl = Prelude.Nothing,
      loggingConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the test status. To return the extension test status
-- of a specific extension version, you must specify @VersionId@.
--
-- This applies only to registered private extension versions.
-- CloudFormation doesn\'t return this information for public extensions,
-- whether they are activated in your account.
describeTypeResponse_typeTestsStatusDescription :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_typeTestsStatusDescription = Lens.lens (\DescribeTypeResponse' {typeTestsStatusDescription} -> typeTestsStatusDescription) (\s@DescribeTypeResponse' {} a -> s {typeTestsStatusDescription = a} :: DescribeTypeResponse)

-- | The deprecation status of the extension version.
--
-- Valid values include:
--
-- -   @LIVE@: The extension is activated or registered and can be used in
--     CloudFormation operations, dependent on its provisioning behavior
--     and visibility scope.
--
-- -   @DEPRECATED@: The extension has been deactivated or deregistered and
--     can no longer be used in CloudFormation operations.
--
-- For public third-party extensions, CloudFormation returns @null@.
describeTypeResponse_deprecatedStatus :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe DeprecatedStatus)
describeTypeResponse_deprecatedStatus = Lens.lens (\DescribeTypeResponse' {deprecatedStatus} -> deprecatedStatus) (\s@DescribeTypeResponse' {} a -> s {deprecatedStatus = a} :: DescribeTypeResponse)

-- | Whether the specified extension version is set as the default version.
--
-- This applies only to private extensions you have registered in your
-- account, and extensions published by Amazon Web Services. For public
-- third-party extensions, whether they are activated in your account,
-- CloudFormation returns @null@.
describeTypeResponse_isDefaultVersion :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Bool)
describeTypeResponse_isDefaultVersion = Lens.lens (\DescribeTypeResponse' {isDefaultVersion} -> isDefaultVersion) (\s@DescribeTypeResponse' {} a -> s {isDefaultVersion = a} :: DescribeTypeResponse)

-- | The ID of the default version of the extension. The default version is
-- used when the extension version isn\'t specified.
--
-- This applies only to private extensions you have registered in your
-- account. For public extensions, both those provided by Amazon Web
-- Services and published by third parties, CloudFormation returns @null@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- To set the default version of an extension, use
-- @ SetTypeDefaultVersion @.
describeTypeResponse_defaultVersionId :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_defaultVersionId = Lens.lens (\DescribeTypeResponse' {defaultVersionId} -> defaultVersionId) (\s@DescribeTypeResponse' {} a -> s {defaultVersionId = a} :: DescribeTypeResponse)

-- | The kind of extension.
describeTypeResponse_type :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe RegistryType)
describeTypeResponse_type = Lens.lens (\DescribeTypeResponse' {type'} -> type') (\s@DescribeTypeResponse' {} a -> s {type' = a} :: DescribeTypeResponse)

-- | The URL of a page providing detailed documentation for this extension.
describeTypeResponse_documentationUrl :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_documentationUrl = Lens.lens (\DescribeTypeResponse' {documentationUrl} -> documentationUrl) (\s@DescribeTypeResponse' {} a -> s {documentationUrl = a} :: DescribeTypeResponse)

-- | For extensions that are modules, the public third-party extensions that
-- must be activated in your account in order for the module itself to be
-- activated.
describeTypeResponse_requiredActivatedTypes :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe [RequiredActivatedType])
describeTypeResponse_requiredActivatedTypes = Lens.lens (\DescribeTypeResponse' {requiredActivatedTypes} -> requiredActivatedTypes) (\s@DescribeTypeResponse' {} a -> s {requiredActivatedTypes = a} :: DescribeTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | A JSON string that represent the current configuration data for the
-- extension in this account and region.
--
-- To set the configuration data for an extension, use
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_SetTypeConfiguration.html SetTypeConfiguration>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-register.html#registry-set-configuration Configuring extensions at the account level>
-- in the /CloudFormation User Guide/.
describeTypeResponse_configurationSchema :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_configurationSchema = Lens.lens (\DescribeTypeResponse' {configurationSchema} -> configurationSchema) (\s@DescribeTypeResponse' {} a -> s {configurationSchema = a} :: DescribeTypeResponse)

-- | The scope at which the extension is visible and usable in CloudFormation
-- operations.
--
-- Valid values include:
--
-- -   @PRIVATE@: The extension is only visible and usable within the
--     account in which it is registered. CloudFormation marks any
--     extensions you register as @PRIVATE@.
--
-- -   @PUBLIC@: The extension is publicly visible and usable within any
--     Amazon Web Services account.
describeTypeResponse_visibility :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Visibility)
describeTypeResponse_visibility = Lens.lens (\DescribeTypeResponse' {visibility} -> visibility) (\s@DescribeTypeResponse' {} a -> s {visibility = a} :: DescribeTypeResponse)

-- | Whether CloudFormation automatically updates the extension in this
-- account and region when a new /minor/ version is published by the
-- extension publisher. Major versions released by the publisher must be
-- manually updated. For more information, see
-- <AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable Activating public extensions for use in your account>
-- in the /CloudFormation User Guide/.
describeTypeResponse_autoUpdate :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Bool)
describeTypeResponse_autoUpdate = Lens.lens (\DescribeTypeResponse' {autoUpdate} -> autoUpdate) (\s@DescribeTypeResponse' {} a -> s {autoUpdate = a} :: DescribeTypeResponse)

-- | The Amazon Resource Name (ARN) of the extension.
describeTypeResponse_arn :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_arn = Lens.lens (\DescribeTypeResponse' {arn} -> arn) (\s@DescribeTypeResponse' {} a -> s {arn = a} :: DescribeTypeResponse)

-- | When the specified private extension version was registered or activated
-- in your account.
describeTypeResponse_timeCreated :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.UTCTime)
describeTypeResponse_timeCreated = Lens.lens (\DescribeTypeResponse' {timeCreated} -> timeCreated) (\s@DescribeTypeResponse' {} a -> s {timeCreated = a} :: DescribeTypeResponse) Prelude.. Lens.mapping Data._Time

-- | The version number of a public third-party extension.
--
-- This applies only if you specify a public extension you have activated
-- in your account, or specify a public extension without specifying a
-- version. For all other extensions, CloudFormation returns @null@.
describeTypeResponse_publicVersionNumber :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_publicVersionNumber = Lens.lens (\DescribeTypeResponse' {publicVersionNumber} -> publicVersionNumber) (\s@DescribeTypeResponse' {} a -> s {publicVersionNumber = a} :: DescribeTypeResponse)

-- | The publisher ID of the extension publisher.
--
-- This applies only to public third-party extensions. For private
-- registered extensions, and extensions provided by Amazon Web Services,
-- CloudFormation returns @null@.
describeTypeResponse_publisherId :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_publisherId = Lens.lens (\DescribeTypeResponse' {publisherId} -> publisherId) (\s@DescribeTypeResponse' {} a -> s {publisherId = a} :: DescribeTypeResponse)

-- | The name of the extension.
--
-- If the extension is a public third-party type you have activated with a
-- type name alias, CloudFormation returns the type name alias. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html ActivateType>.
describeTypeResponse_typeName :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_typeName = Lens.lens (\DescribeTypeResponse' {typeName} -> typeName) (\s@DescribeTypeResponse' {} a -> s {typeName = a} :: DescribeTypeResponse)

-- | The description of the extension.
describeTypeResponse_description :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_description = Lens.lens (\DescribeTypeResponse' {description} -> description) (\s@DescribeTypeResponse' {} a -> s {description = a} :: DescribeTypeResponse)

-- | When the specified extension version was registered. This applies only
-- to:
--
-- -   Private extensions you have registered in your account. For more
--     information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- -   Public extensions you have activated in your account with
--     auto-update specified. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html ActivateType>.
describeTypeResponse_lastUpdated :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.UTCTime)
describeTypeResponse_lastUpdated = Lens.lens (\DescribeTypeResponse' {lastUpdated} -> lastUpdated) (\s@DescribeTypeResponse' {} a -> s {lastUpdated = a} :: DescribeTypeResponse) Prelude.. Lens.mapping Data._Time

-- | For public extensions that have been activated for this account and
-- region, the type name of the public extension.
--
-- If you specified a @TypeNameAlias@ when enabling the extension in this
-- account and region, CloudFormation treats that alias as the extension\'s
-- type name within the account and region, not the type name of the public
-- extension. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-alias Specifying aliases to refer to extensions>
-- in the /CloudFormation User Guide/.
describeTypeResponse_originalTypeName :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_originalTypeName = Lens.lens (\DescribeTypeResponse' {originalTypeName} -> originalTypeName) (\s@DescribeTypeResponse' {} a -> s {originalTypeName = a} :: DescribeTypeResponse)

-- | For resource type extensions, the provisioning behavior of the resource
-- type. CloudFormation determines the provisioning type during
-- registration, based on the types of handlers in the schema handler
-- package submitted.
--
-- Valid values include:
--
-- -   @FULLY_MUTABLE@: The resource type includes an update handler to
--     process updates to the type during stack update operations.
--
-- -   @IMMUTABLE@: The resource type doesn\'t include an update handler,
--     so the type can\'t be updated and must instead be replaced during
--     stack update operations.
--
-- -   @NON_PROVISIONABLE@: The resource type doesn\'t include all the
--     following handlers, and therefore can\'t actually be provisioned.
--
--     -   create
--
--     -   read
--
--     -   delete
describeTypeResponse_provisioningType :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe ProvisioningType)
describeTypeResponse_provisioningType = Lens.lens (\DescribeTypeResponse' {provisioningType} -> provisioningType) (\s@DescribeTypeResponse' {} a -> s {provisioningType = a} :: DescribeTypeResponse)

-- | The latest version of a public extension /that is available/ for use.
--
-- This only applies if you specify a public extension, and you don\'t
-- specify a version. For all other requests, CloudFormation returns
-- @null@.
describeTypeResponse_latestPublicVersion :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_latestPublicVersion = Lens.lens (\DescribeTypeResponse' {latestPublicVersion} -> latestPublicVersion) (\s@DescribeTypeResponse' {} a -> s {latestPublicVersion = a} :: DescribeTypeResponse)

-- | The schema that defines the extension.
--
-- For more information about extension schemas, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-schema.html Resource Provider Schema>
-- in the /CloudFormation CLI User Guide/.
describeTypeResponse_schema :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_schema = Lens.lens (\DescribeTypeResponse' {schema} -> schema) (\s@DescribeTypeResponse' {} a -> s {schema = a} :: DescribeTypeResponse)

-- | Whether the extension is activated in the account and region.
--
-- This only applies to public third-party extensions. For all other
-- extensions, CloudFormation returns @null@.
describeTypeResponse_isActivated :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Bool)
describeTypeResponse_isActivated = Lens.lens (\DescribeTypeResponse' {isActivated} -> isActivated) (\s@DescribeTypeResponse' {} a -> s {isActivated = a} :: DescribeTypeResponse)

-- | The Amazon Resource Name (ARN) of the IAM execution role used to
-- register the extension. This applies only to private extensions you have
-- registered in your account. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- If the registered extension calls any Amazon Web Services APIs, you must
-- create an
-- /<https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles.html IAM execution role>/
-- that includes the necessary permissions to call those Amazon Web
-- Services APIs, and provision that execution role in your account.
-- CloudFormation then assumes that execution role to provide your
-- extension with the appropriate credentials.
describeTypeResponse_executionRoleArn :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_executionRoleArn = Lens.lens (\DescribeTypeResponse' {executionRoleArn} -> executionRoleArn) (\s@DescribeTypeResponse' {} a -> s {executionRoleArn = a} :: DescribeTypeResponse)

-- | The contract test status of the registered extension version. To return
-- the extension test status of a specific extension version, you must
-- specify @VersionId@.
--
-- This applies only to registered private extension versions.
-- CloudFormation doesn\'t return this information for public extensions,
-- whether they are activated in your account.
--
-- -   @PASSED@: The extension has passed all its contract tests.
--
--     An extension must have a test status of @PASSED@ before it can be
--     published. For more information, see
--     <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-publish.html Publishing extensions to make them available for public use>
--     in the /CloudFormation Command Line Interface User Guide/.
--
-- -   @FAILED@: The extension has failed one or more contract tests.
--
-- -   @IN_PROGRESS@: Contract tests are currently being performed on the
--     extension.
--
-- -   @NOT_TESTED@: Contract tests haven\'t been performed on the
--     extension.
describeTypeResponse_typeTestsStatus :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe TypeTestsStatus)
describeTypeResponse_typeTestsStatus = Lens.lens (\DescribeTypeResponse' {typeTestsStatus} -> typeTestsStatus) (\s@DescribeTypeResponse' {} a -> s {typeTestsStatus = a} :: DescribeTypeResponse)

-- | For public extensions that have been activated for this account and
-- region, the Amazon Resource Name (ARN) of the public extension.
describeTypeResponse_originalTypeArn :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_originalTypeArn = Lens.lens (\DescribeTypeResponse' {originalTypeArn} -> originalTypeArn) (\s@DescribeTypeResponse' {} a -> s {originalTypeArn = a} :: DescribeTypeResponse)

-- | The URL of the source code for the extension.
describeTypeResponse_sourceUrl :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe Prelude.Text)
describeTypeResponse_sourceUrl = Lens.lens (\DescribeTypeResponse' {sourceUrl} -> sourceUrl) (\s@DescribeTypeResponse' {} a -> s {sourceUrl = a} :: DescribeTypeResponse)

-- | Contains logging configuration information for private extensions. This
-- applies only to private extensions you have registered in your account.
-- For public extensions, both those provided by Amazon Web Services and
-- published by third parties, CloudFormation returns @null@. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
describeTypeResponse_loggingConfig :: Lens.Lens' DescribeTypeResponse (Prelude.Maybe LoggingConfig)
describeTypeResponse_loggingConfig = Lens.lens (\DescribeTypeResponse' {loggingConfig} -> loggingConfig) (\s@DescribeTypeResponse' {} a -> s {loggingConfig = a} :: DescribeTypeResponse)

-- | The response's http status code.
describeTypeResponse_httpStatus :: Lens.Lens' DescribeTypeResponse Prelude.Int
describeTypeResponse_httpStatus = Lens.lens (\DescribeTypeResponse' {httpStatus} -> httpStatus) (\s@DescribeTypeResponse' {} a -> s {httpStatus = a} :: DescribeTypeResponse)

instance Prelude.NFData DescribeTypeResponse where
  rnf DescribeTypeResponse' {..} =
    Prelude.rnf typeTestsStatusDescription
      `Prelude.seq` Prelude.rnf deprecatedStatus
      `Prelude.seq` Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf documentationUrl
      `Prelude.seq` Prelude.rnf requiredActivatedTypes
      `Prelude.seq` Prelude.rnf configurationSchema
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf autoUpdate
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf timeCreated
      `Prelude.seq` Prelude.rnf publicVersionNumber
      `Prelude.seq` Prelude.rnf publisherId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf originalTypeName
      `Prelude.seq` Prelude.rnf provisioningType
      `Prelude.seq` Prelude.rnf
        latestPublicVersion
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf isActivated
      `Prelude.seq` Prelude.rnf
        executionRoleArn
      `Prelude.seq` Prelude.rnf
        typeTestsStatus
      `Prelude.seq` Prelude.rnf
        originalTypeArn
      `Prelude.seq` Prelude.rnf
        sourceUrl
      `Prelude.seq` Prelude.rnf
        loggingConfig
      `Prelude.seq` Prelude.rnf
        httpStatus

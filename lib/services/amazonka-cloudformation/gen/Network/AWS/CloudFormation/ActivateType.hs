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
-- Module      : Network.AWS.CloudFormation.ActivateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates a public third-party extension, making it available for use in
-- stack templates. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html Using public extensions>
-- in the /CloudFormation User Guide/.
--
-- Once you have activated a public third-party extension in your account
-- and region, use
-- <AWSCloudFormation/latest/APIReference/API_SetTypeConfiguration.html SetTypeConfiguration>
-- to specify configuration properties for the extension. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-register.html#registry-set-configuration Configuring extensions at the account level>
-- in the /CloudFormation User Guide/.
module Network.AWS.CloudFormation.ActivateType
  ( -- * Creating a Request
    ActivateType (..),
    newActivateType,

    -- * Request Lenses
    activateType_typeName,
    activateType_publicTypeArn,
    activateType_loggingConfig,
    activateType_majorVersion,
    activateType_publisherId,
    activateType_executionRoleArn,
    activateType_typeNameAlias,
    activateType_autoUpdate,
    activateType_versionBump,
    activateType_type,

    -- * Destructuring the Response
    ActivateTypeResponse (..),
    newActivateTypeResponse,

    -- * Response Lenses
    activateTypeResponse_arn,
    activateTypeResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newActivateType' smart constructor.
data ActivateType = ActivateType'
  { -- | The name of the extension.
    --
    -- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
    -- and @PublisherId@.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the public extension.
    --
    -- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
    -- and @PublisherId@.
    publicTypeArn :: Prelude.Maybe Prelude.Text,
    loggingConfig :: Prelude.Maybe LoggingConfig,
    -- | The major version of this extension you want to activate, if multiple
    -- major versions are available. The default is the latest major version.
    -- CloudFormation uses the latest available /minor/ version of the major
    -- version selected.
    --
    -- You can specify @MajorVersion@ or @VersionBump@, but not both.
    majorVersion :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the extension publisher.
    --
    -- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
    -- and @PublisherId@.
    publisherId :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM execution role to use to activate the extension.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | An alias to assign to the public extension, in this account and region.
    -- If you specify an alias for the extension, CloudFormation treats the
    -- alias as the extension type name within this account and region. You
    -- must use the alias to refer to the extension in your templates, API
    -- calls, and CloudFormation console.
    --
    -- An extension alias must be unique within a given account and region. You
    -- can activate the same public resource multiple times in the same account
    -- and region, using different type name aliases.
    typeNameAlias :: Prelude.Maybe Prelude.Text,
    -- | Whether to automatically update the extension in this account and region
    -- when a new /minor/ version is published by the extension publisher.
    -- Major versions released by the publisher must be manually updated.
    --
    -- The default is @true@.
    autoUpdate :: Prelude.Maybe Prelude.Bool,
    -- | Manually updates a previously-activated type to a new major or minor
    -- version, if available. You can also use this parameter to update the
    -- value of @AutoUpdate@.
    --
    -- -   @MAJOR@: CloudFormation updates the extension to the newest major
    --     version, if one is available.
    --
    -- -   @MINOR@: CloudFormation updates the extension to the newest minor
    --     version, if one is available.
    versionBump :: Prelude.Maybe VersionBump,
    -- | The extension type.
    --
    -- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
    -- and @PublisherId@.
    type' :: Prelude.Maybe ThirdPartyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeName', 'activateType_typeName' - The name of the extension.
--
-- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
-- and @PublisherId@.
--
-- 'publicTypeArn', 'activateType_publicTypeArn' - The Amazon Resource Number (ARN) of the public extension.
--
-- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
-- and @PublisherId@.
--
-- 'loggingConfig', 'activateType_loggingConfig' - Undocumented member.
--
-- 'majorVersion', 'activateType_majorVersion' - The major version of this extension you want to activate, if multiple
-- major versions are available. The default is the latest major version.
-- CloudFormation uses the latest available /minor/ version of the major
-- version selected.
--
-- You can specify @MajorVersion@ or @VersionBump@, but not both.
--
-- 'publisherId', 'activateType_publisherId' - The ID of the extension publisher.
--
-- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
-- and @PublisherId@.
--
-- 'executionRoleArn', 'activateType_executionRoleArn' - The name of the IAM execution role to use to activate the extension.
--
-- 'typeNameAlias', 'activateType_typeNameAlias' - An alias to assign to the public extension, in this account and region.
-- If you specify an alias for the extension, CloudFormation treats the
-- alias as the extension type name within this account and region. You
-- must use the alias to refer to the extension in your templates, API
-- calls, and CloudFormation console.
--
-- An extension alias must be unique within a given account and region. You
-- can activate the same public resource multiple times in the same account
-- and region, using different type name aliases.
--
-- 'autoUpdate', 'activateType_autoUpdate' - Whether to automatically update the extension in this account and region
-- when a new /minor/ version is published by the extension publisher.
-- Major versions released by the publisher must be manually updated.
--
-- The default is @true@.
--
-- 'versionBump', 'activateType_versionBump' - Manually updates a previously-activated type to a new major or minor
-- version, if available. You can also use this parameter to update the
-- value of @AutoUpdate@.
--
-- -   @MAJOR@: CloudFormation updates the extension to the newest major
--     version, if one is available.
--
-- -   @MINOR@: CloudFormation updates the extension to the newest minor
--     version, if one is available.
--
-- 'type'', 'activateType_type' - The extension type.
--
-- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
-- and @PublisherId@.
newActivateType ::
  ActivateType
newActivateType =
  ActivateType'
    { typeName = Prelude.Nothing,
      publicTypeArn = Prelude.Nothing,
      loggingConfig = Prelude.Nothing,
      majorVersion = Prelude.Nothing,
      publisherId = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      typeNameAlias = Prelude.Nothing,
      autoUpdate = Prelude.Nothing,
      versionBump = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the extension.
--
-- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
-- and @PublisherId@.
activateType_typeName :: Lens.Lens' ActivateType (Prelude.Maybe Prelude.Text)
activateType_typeName = Lens.lens (\ActivateType' {typeName} -> typeName) (\s@ActivateType' {} a -> s {typeName = a} :: ActivateType)

-- | The Amazon Resource Number (ARN) of the public extension.
--
-- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
-- and @PublisherId@.
activateType_publicTypeArn :: Lens.Lens' ActivateType (Prelude.Maybe Prelude.Text)
activateType_publicTypeArn = Lens.lens (\ActivateType' {publicTypeArn} -> publicTypeArn) (\s@ActivateType' {} a -> s {publicTypeArn = a} :: ActivateType)

-- | Undocumented member.
activateType_loggingConfig :: Lens.Lens' ActivateType (Prelude.Maybe LoggingConfig)
activateType_loggingConfig = Lens.lens (\ActivateType' {loggingConfig} -> loggingConfig) (\s@ActivateType' {} a -> s {loggingConfig = a} :: ActivateType)

-- | The major version of this extension you want to activate, if multiple
-- major versions are available. The default is the latest major version.
-- CloudFormation uses the latest available /minor/ version of the major
-- version selected.
--
-- You can specify @MajorVersion@ or @VersionBump@, but not both.
activateType_majorVersion :: Lens.Lens' ActivateType (Prelude.Maybe Prelude.Natural)
activateType_majorVersion = Lens.lens (\ActivateType' {majorVersion} -> majorVersion) (\s@ActivateType' {} a -> s {majorVersion = a} :: ActivateType)

-- | The ID of the extension publisher.
--
-- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
-- and @PublisherId@.
activateType_publisherId :: Lens.Lens' ActivateType (Prelude.Maybe Prelude.Text)
activateType_publisherId = Lens.lens (\ActivateType' {publisherId} -> publisherId) (\s@ActivateType' {} a -> s {publisherId = a} :: ActivateType)

-- | The name of the IAM execution role to use to activate the extension.
activateType_executionRoleArn :: Lens.Lens' ActivateType (Prelude.Maybe Prelude.Text)
activateType_executionRoleArn = Lens.lens (\ActivateType' {executionRoleArn} -> executionRoleArn) (\s@ActivateType' {} a -> s {executionRoleArn = a} :: ActivateType)

-- | An alias to assign to the public extension, in this account and region.
-- If you specify an alias for the extension, CloudFormation treats the
-- alias as the extension type name within this account and region. You
-- must use the alias to refer to the extension in your templates, API
-- calls, and CloudFormation console.
--
-- An extension alias must be unique within a given account and region. You
-- can activate the same public resource multiple times in the same account
-- and region, using different type name aliases.
activateType_typeNameAlias :: Lens.Lens' ActivateType (Prelude.Maybe Prelude.Text)
activateType_typeNameAlias = Lens.lens (\ActivateType' {typeNameAlias} -> typeNameAlias) (\s@ActivateType' {} a -> s {typeNameAlias = a} :: ActivateType)

-- | Whether to automatically update the extension in this account and region
-- when a new /minor/ version is published by the extension publisher.
-- Major versions released by the publisher must be manually updated.
--
-- The default is @true@.
activateType_autoUpdate :: Lens.Lens' ActivateType (Prelude.Maybe Prelude.Bool)
activateType_autoUpdate = Lens.lens (\ActivateType' {autoUpdate} -> autoUpdate) (\s@ActivateType' {} a -> s {autoUpdate = a} :: ActivateType)

-- | Manually updates a previously-activated type to a new major or minor
-- version, if available. You can also use this parameter to update the
-- value of @AutoUpdate@.
--
-- -   @MAJOR@: CloudFormation updates the extension to the newest major
--     version, if one is available.
--
-- -   @MINOR@: CloudFormation updates the extension to the newest minor
--     version, if one is available.
activateType_versionBump :: Lens.Lens' ActivateType (Prelude.Maybe VersionBump)
activateType_versionBump = Lens.lens (\ActivateType' {versionBump} -> versionBump) (\s@ActivateType' {} a -> s {versionBump = a} :: ActivateType)

-- | The extension type.
--
-- Conditional: You must specify @PublicTypeArn@, or @TypeName@, @Type@,
-- and @PublisherId@.
activateType_type :: Lens.Lens' ActivateType (Prelude.Maybe ThirdPartyType)
activateType_type = Lens.lens (\ActivateType' {type'} -> type') (\s@ActivateType' {} a -> s {type' = a} :: ActivateType)

instance Core.AWSRequest ActivateType where
  type AWSResponse ActivateType = ActivateTypeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ActivateTypeResult"
      ( \s h x ->
          ActivateTypeResponse'
            Prelude.<$> (x Core..@? "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ActivateType

instance Prelude.NFData ActivateType

instance Core.ToHeaders ActivateType where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ActivateType where
  toPath = Prelude.const "/"

instance Core.ToQuery ActivateType where
  toQuery ActivateType' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ActivateType" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "TypeName" Core.=: typeName,
        "PublicTypeArn" Core.=: publicTypeArn,
        "LoggingConfig" Core.=: loggingConfig,
        "MajorVersion" Core.=: majorVersion,
        "PublisherId" Core.=: publisherId,
        "ExecutionRoleArn" Core.=: executionRoleArn,
        "TypeNameAlias" Core.=: typeNameAlias,
        "AutoUpdate" Core.=: autoUpdate,
        "VersionBump" Core.=: versionBump,
        "Type" Core.=: type'
      ]

-- | /See:/ 'newActivateTypeResponse' smart constructor.
data ActivateTypeResponse = ActivateTypeResponse'
  { -- | The Amazon Resource Number (ARN) of the activated extension, in this
    -- account and region.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'activateTypeResponse_arn' - The Amazon Resource Number (ARN) of the activated extension, in this
-- account and region.
--
-- 'httpStatus', 'activateTypeResponse_httpStatus' - The response's http status code.
newActivateTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ActivateTypeResponse
newActivateTypeResponse pHttpStatus_ =
  ActivateTypeResponse'
    { arn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Number (ARN) of the activated extension, in this
-- account and region.
activateTypeResponse_arn :: Lens.Lens' ActivateTypeResponse (Prelude.Maybe Prelude.Text)
activateTypeResponse_arn = Lens.lens (\ActivateTypeResponse' {arn} -> arn) (\s@ActivateTypeResponse' {} a -> s {arn = a} :: ActivateTypeResponse)

-- | The response's http status code.
activateTypeResponse_httpStatus :: Lens.Lens' ActivateTypeResponse Prelude.Int
activateTypeResponse_httpStatus = Lens.lens (\ActivateTypeResponse' {httpStatus} -> httpStatus) (\s@ActivateTypeResponse' {} a -> s {httpStatus = a} :: ActivateTypeResponse)

instance Prelude.NFData ActivateTypeResponse

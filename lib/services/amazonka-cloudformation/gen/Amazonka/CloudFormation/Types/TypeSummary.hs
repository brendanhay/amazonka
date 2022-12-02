{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFormation.Types.TypeSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.TypeSummary where

import Amazonka.CloudFormation.Types.IdentityProvider
import Amazonka.CloudFormation.Types.RegistryType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about the specified CloudFormation
-- extension.
--
-- /See:/ 'newTypeSummary' smart constructor.
data TypeSummary = TypeSummary'
  { -- | The ID of the default version of the extension. The default version is
    -- used when the extension version isn\'t specified.
    --
    -- This applies only to private extensions you have registered in your
    -- account. For public extensions, both those provided by Amazon and
    -- published by third parties, CloudFormation returns @null@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
    --
    -- To set the default version of an extension, use
    -- @ SetTypeDefaultVersion @.
    defaultVersionId :: Prelude.Maybe Prelude.Text,
    -- | The kind of extension.
    type' :: Prelude.Maybe RegistryType,
    -- | The Amazon Resource Name (ARN) of the extension.
    typeArn :: Prelude.Maybe Prelude.Text,
    -- | For public extensions that have been activated for this account and
    -- region, the version of the public extension to be used for
    -- CloudFormation operations in this account and Region.
    --
    -- How you specified @AutoUpdate@ when enabling the extension affects
    -- whether CloudFormation automatically updates the extension in this
    -- account and region when a new version is released. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
    -- in the /CloudFormation User Guide/.
    publicVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The ID of the extension publisher, if the extension is published by a
    -- third party. Extensions published by Amazon don\'t return a publisher
    -- ID.
    publisherId :: Prelude.Maybe Prelude.Text,
    -- | The name of the extension.
    --
    -- If you specified a @TypeNameAlias@ when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate this extension>
    -- in your account and region, CloudFormation considers that alias as the
    -- type name.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The service used to verify the publisher identity.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html Registering your account to publish CloudFormation extensions>
    -- in the /CFN-CLI User Guide for Extension Development/.
    publisherIdentity :: Prelude.Maybe IdentityProvider,
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
    --
    -- For all other extension types, CloudFormation returns @null@.
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
    -- | For public extensions that have been activated for this account and
    -- region, the latest version of the public extension /that is available/.
    -- For any extensions other than activated third-arty extensions,
    -- CloudFormation returns @null@.
    --
    -- How you specified @AutoUpdate@ when enabling the extension affects
    -- whether CloudFormation automatically updates the extension in this
    -- account and region when a new version is released. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
    -- in the /CloudFormation User Guide/.
    latestPublicVersion :: Prelude.Maybe Prelude.Text,
    -- | Whether the extension is activated for this account and region.
    --
    -- This applies only to third-party public extensions. Extensions published
    -- by Amazon are activated by default.
    isActivated :: Prelude.Maybe Prelude.Bool,
    -- | The publisher name, as defined in the public profile for that publisher
    -- in the service used to verify the publisher identity.
    publisherName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVersionId', 'typeSummary_defaultVersionId' - The ID of the default version of the extension. The default version is
-- used when the extension version isn\'t specified.
--
-- This applies only to private extensions you have registered in your
-- account. For public extensions, both those provided by Amazon and
-- published by third parties, CloudFormation returns @null@. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- To set the default version of an extension, use
-- @ SetTypeDefaultVersion @.
--
-- 'type'', 'typeSummary_type' - The kind of extension.
--
-- 'typeArn', 'typeSummary_typeArn' - The Amazon Resource Name (ARN) of the extension.
--
-- 'publicVersionNumber', 'typeSummary_publicVersionNumber' - For public extensions that have been activated for this account and
-- region, the version of the public extension to be used for
-- CloudFormation operations in this account and Region.
--
-- How you specified @AutoUpdate@ when enabling the extension affects
-- whether CloudFormation automatically updates the extension in this
-- account and region when a new version is released. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
-- in the /CloudFormation User Guide/.
--
-- 'publisherId', 'typeSummary_publisherId' - The ID of the extension publisher, if the extension is published by a
-- third party. Extensions published by Amazon don\'t return a publisher
-- ID.
--
-- 'typeName', 'typeSummary_typeName' - The name of the extension.
--
-- If you specified a @TypeNameAlias@ when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate this extension>
-- in your account and region, CloudFormation considers that alias as the
-- type name.
--
-- 'publisherIdentity', 'typeSummary_publisherIdentity' - The service used to verify the publisher identity.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html Registering your account to publish CloudFormation extensions>
-- in the /CFN-CLI User Guide for Extension Development/.
--
-- 'description', 'typeSummary_description' - The description of the extension.
--
-- 'lastUpdated', 'typeSummary_lastUpdated' - When the specified extension version was registered. This applies only
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
-- For all other extension types, CloudFormation returns @null@.
--
-- 'originalTypeName', 'typeSummary_originalTypeName' - For public extensions that have been activated for this account and
-- region, the type name of the public extension.
--
-- If you specified a @TypeNameAlias@ when enabling the extension in this
-- account and region, CloudFormation treats that alias as the extension\'s
-- type name within the account and region, not the type name of the public
-- extension. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-alias Specifying aliases to refer to extensions>
-- in the /CloudFormation User Guide/.
--
-- 'latestPublicVersion', 'typeSummary_latestPublicVersion' - For public extensions that have been activated for this account and
-- region, the latest version of the public extension /that is available/.
-- For any extensions other than activated third-arty extensions,
-- CloudFormation returns @null@.
--
-- How you specified @AutoUpdate@ when enabling the extension affects
-- whether CloudFormation automatically updates the extension in this
-- account and region when a new version is released. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
-- in the /CloudFormation User Guide/.
--
-- 'isActivated', 'typeSummary_isActivated' - Whether the extension is activated for this account and region.
--
-- This applies only to third-party public extensions. Extensions published
-- by Amazon are activated by default.
--
-- 'publisherName', 'typeSummary_publisherName' - The publisher name, as defined in the public profile for that publisher
-- in the service used to verify the publisher identity.
newTypeSummary ::
  TypeSummary
newTypeSummary =
  TypeSummary'
    { defaultVersionId = Prelude.Nothing,
      type' = Prelude.Nothing,
      typeArn = Prelude.Nothing,
      publicVersionNumber = Prelude.Nothing,
      publisherId = Prelude.Nothing,
      typeName = Prelude.Nothing,
      publisherIdentity = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      originalTypeName = Prelude.Nothing,
      latestPublicVersion = Prelude.Nothing,
      isActivated = Prelude.Nothing,
      publisherName = Prelude.Nothing
    }

-- | The ID of the default version of the extension. The default version is
-- used when the extension version isn\'t specified.
--
-- This applies only to private extensions you have registered in your
-- account. For public extensions, both those provided by Amazon and
-- published by third parties, CloudFormation returns @null@. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html RegisterType>.
--
-- To set the default version of an extension, use
-- @ SetTypeDefaultVersion @.
typeSummary_defaultVersionId :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_defaultVersionId = Lens.lens (\TypeSummary' {defaultVersionId} -> defaultVersionId) (\s@TypeSummary' {} a -> s {defaultVersionId = a} :: TypeSummary)

-- | The kind of extension.
typeSummary_type :: Lens.Lens' TypeSummary (Prelude.Maybe RegistryType)
typeSummary_type = Lens.lens (\TypeSummary' {type'} -> type') (\s@TypeSummary' {} a -> s {type' = a} :: TypeSummary)

-- | The Amazon Resource Name (ARN) of the extension.
typeSummary_typeArn :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_typeArn = Lens.lens (\TypeSummary' {typeArn} -> typeArn) (\s@TypeSummary' {} a -> s {typeArn = a} :: TypeSummary)

-- | For public extensions that have been activated for this account and
-- region, the version of the public extension to be used for
-- CloudFormation operations in this account and Region.
--
-- How you specified @AutoUpdate@ when enabling the extension affects
-- whether CloudFormation automatically updates the extension in this
-- account and region when a new version is released. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
-- in the /CloudFormation User Guide/.
typeSummary_publicVersionNumber :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_publicVersionNumber = Lens.lens (\TypeSummary' {publicVersionNumber} -> publicVersionNumber) (\s@TypeSummary' {} a -> s {publicVersionNumber = a} :: TypeSummary)

-- | The ID of the extension publisher, if the extension is published by a
-- third party. Extensions published by Amazon don\'t return a publisher
-- ID.
typeSummary_publisherId :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_publisherId = Lens.lens (\TypeSummary' {publisherId} -> publisherId) (\s@TypeSummary' {} a -> s {publisherId = a} :: TypeSummary)

-- | The name of the extension.
--
-- If you specified a @TypeNameAlias@ when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate this extension>
-- in your account and region, CloudFormation considers that alias as the
-- type name.
typeSummary_typeName :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_typeName = Lens.lens (\TypeSummary' {typeName} -> typeName) (\s@TypeSummary' {} a -> s {typeName = a} :: TypeSummary)

-- | The service used to verify the publisher identity.
--
-- For more information, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/publish-extension.html Registering your account to publish CloudFormation extensions>
-- in the /CFN-CLI User Guide for Extension Development/.
typeSummary_publisherIdentity :: Lens.Lens' TypeSummary (Prelude.Maybe IdentityProvider)
typeSummary_publisherIdentity = Lens.lens (\TypeSummary' {publisherIdentity} -> publisherIdentity) (\s@TypeSummary' {} a -> s {publisherIdentity = a} :: TypeSummary)

-- | The description of the extension.
typeSummary_description :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_description = Lens.lens (\TypeSummary' {description} -> description) (\s@TypeSummary' {} a -> s {description = a} :: TypeSummary)

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
--
-- For all other extension types, CloudFormation returns @null@.
typeSummary_lastUpdated :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.UTCTime)
typeSummary_lastUpdated = Lens.lens (\TypeSummary' {lastUpdated} -> lastUpdated) (\s@TypeSummary' {} a -> s {lastUpdated = a} :: TypeSummary) Prelude.. Lens.mapping Data._Time

-- | For public extensions that have been activated for this account and
-- region, the type name of the public extension.
--
-- If you specified a @TypeNameAlias@ when enabling the extension in this
-- account and region, CloudFormation treats that alias as the extension\'s
-- type name within the account and region, not the type name of the public
-- extension. For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-alias Specifying aliases to refer to extensions>
-- in the /CloudFormation User Guide/.
typeSummary_originalTypeName :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_originalTypeName = Lens.lens (\TypeSummary' {originalTypeName} -> originalTypeName) (\s@TypeSummary' {} a -> s {originalTypeName = a} :: TypeSummary)

-- | For public extensions that have been activated for this account and
-- region, the latest version of the public extension /that is available/.
-- For any extensions other than activated third-arty extensions,
-- CloudFormation returns @null@.
--
-- How you specified @AutoUpdate@ when enabling the extension affects
-- whether CloudFormation automatically updates the extension in this
-- account and region when a new version is released. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-public.html#registry-public-enable-auto Setting CloudFormation to automatically use new versions of extensions>
-- in the /CloudFormation User Guide/.
typeSummary_latestPublicVersion :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_latestPublicVersion = Lens.lens (\TypeSummary' {latestPublicVersion} -> latestPublicVersion) (\s@TypeSummary' {} a -> s {latestPublicVersion = a} :: TypeSummary)

-- | Whether the extension is activated for this account and region.
--
-- This applies only to third-party public extensions. Extensions published
-- by Amazon are activated by default.
typeSummary_isActivated :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Bool)
typeSummary_isActivated = Lens.lens (\TypeSummary' {isActivated} -> isActivated) (\s@TypeSummary' {} a -> s {isActivated = a} :: TypeSummary)

-- | The publisher name, as defined in the public profile for that publisher
-- in the service used to verify the publisher identity.
typeSummary_publisherName :: Lens.Lens' TypeSummary (Prelude.Maybe Prelude.Text)
typeSummary_publisherName = Lens.lens (\TypeSummary' {publisherName} -> publisherName) (\s@TypeSummary' {} a -> s {publisherName = a} :: TypeSummary)

instance Data.FromXML TypeSummary where
  parseXML x =
    TypeSummary'
      Prelude.<$> (x Data..@? "DefaultVersionId")
      Prelude.<*> (x Data..@? "Type")
      Prelude.<*> (x Data..@? "TypeArn")
      Prelude.<*> (x Data..@? "PublicVersionNumber")
      Prelude.<*> (x Data..@? "PublisherId")
      Prelude.<*> (x Data..@? "TypeName")
      Prelude.<*> (x Data..@? "PublisherIdentity")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "LastUpdated")
      Prelude.<*> (x Data..@? "OriginalTypeName")
      Prelude.<*> (x Data..@? "LatestPublicVersion")
      Prelude.<*> (x Data..@? "IsActivated")
      Prelude.<*> (x Data..@? "PublisherName")

instance Prelude.Hashable TypeSummary where
  hashWithSalt _salt TypeSummary' {..} =
    _salt `Prelude.hashWithSalt` defaultVersionId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` typeArn
      `Prelude.hashWithSalt` publicVersionNumber
      `Prelude.hashWithSalt` publisherId
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` publisherIdentity
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` originalTypeName
      `Prelude.hashWithSalt` latestPublicVersion
      `Prelude.hashWithSalt` isActivated
      `Prelude.hashWithSalt` publisherName

instance Prelude.NFData TypeSummary where
  rnf TypeSummary' {..} =
    Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf typeArn
      `Prelude.seq` Prelude.rnf publicVersionNumber
      `Prelude.seq` Prelude.rnf publisherId
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf publisherIdentity
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf originalTypeName
      `Prelude.seq` Prelude.rnf latestPublicVersion
      `Prelude.seq` Prelude.rnf isActivated
      `Prelude.seq` Prelude.rnf publisherName

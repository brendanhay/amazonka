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
-- Module      : Amazonka.RDS.CreateCustomDBEngineVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom DB engine version (CEV). A CEV is a binary volume
-- snapshot of a database engine and specific AMI. The supported engines
-- are the following:
--
-- -   Oracle Database 12.1 Enterprise Edition with the January 2021 or
--     later RU\/RUR
--
-- -   Oracle Database 19c Enterprise Edition with the January 2021 or
--     later RU\/RUR
--
-- Amazon RDS, which is a fully managed service, supplies the Amazon
-- Machine Image (AMI) and database software. The Amazon RDS database
-- software is preinstalled, so you need only select a DB engine and
-- version, and create your database. With Amazon RDS Custom for Oracle,
-- you upload your database installation files in Amazon S3.
--
-- When you create a custom engine version, you specify the files in a JSON
-- document called a CEV manifest. This document describes installation
-- .zip files stored in Amazon S3. RDS Custom creates your CEV from the
-- installation files that you provided. This service model is called Bring
-- Your Own Media (BYOM).
--
-- Creation takes approximately two hours. If creation fails, RDS Custom
-- issues @RDS-EVENT-0196@ with the message
-- @Creation failed for custom engine version@, and includes details about
-- the failure. For example, the event prints missing files.
--
-- After you create the CEV, it is available for use. You can create
-- multiple CEVs, and create multiple RDS Custom instances from any CEV.
-- You can also change the status of a CEV to make it available or
-- inactive.
--
-- The MediaImport service that imports files from Amazon S3 to create CEVs
-- isn\'t integrated with Amazon Web Services CloudTrail. If you turn on
-- data logging for Amazon RDS in CloudTrail, calls to the
-- @CreateCustomDbEngineVersion@ event aren\'t logged. However, you might
-- see calls from the API gateway that accesses your Amazon S3 bucket.
-- These calls originate from the MediaImport service for the
-- @CreateCustomDbEngineVersion@ event.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.html#custom-cev.create Creating a CEV>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.CreateCustomDBEngineVersion
  ( -- * Creating a Request
    CreateCustomDBEngineVersion (..),
    newCreateCustomDBEngineVersion,

    -- * Request Lenses
    createCustomDBEngineVersion_tags,
    createCustomDBEngineVersion_description,
    createCustomDBEngineVersion_databaseInstallationFilesS3Prefix,
    createCustomDBEngineVersion_engine,
    createCustomDBEngineVersion_engineVersion,
    createCustomDBEngineVersion_databaseInstallationFilesS3BucketName,
    createCustomDBEngineVersion_kmsKeyId,
    createCustomDBEngineVersion_manifest,

    -- * Destructuring the Response
    DBEngineVersion (..),
    newDBEngineVersion,

    -- * Response Lenses
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_tagList,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_status,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_createTime,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineDescription,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomDBEngineVersion' smart constructor.
data CreateCustomDBEngineVersion = CreateCustomDBEngineVersion'
  { tags :: Prelude.Maybe [Tag],
    -- | An optional description of your CEV.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 directory that contains the database installation files
    -- for your CEV. For example, a valid bucket name is @123456789012\/cev1@.
    -- If this setting isn\'t specified, no prefix is assumed.
    databaseInstallationFilesS3Prefix :: Prelude.Maybe Prelude.Text,
    -- | The database engine to use for your custom engine version (CEV). The
    -- only supported value is @custom-oracle-ee@.
    engine :: Prelude.Text,
    -- | The name of your CEV. The name format is @19.customized_string @. For
    -- example, a valid name is @19.my_cev1@. This setting is required for RDS
    -- Custom for Oracle, but optional for Amazon RDS. The combination of
    -- @Engine@ and @EngineVersion@ is unique per customer per Region.
    engineVersion :: Prelude.Text,
    -- | The name of an Amazon S3 bucket that contains database installation
    -- files for your CEV. For example, a valid bucket name is
    -- @my-custom-installation-files@.
    databaseInstallationFilesS3BucketName :: Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for an encrypted CEV. A
    -- symmetric encryption KMS key is required for RDS Custom, but optional
    -- for Amazon RDS.
    --
    -- If you have an existing symmetric encryption KMS key in your account,
    -- you can use it with RDS Custom. No further action is necessary. If you
    -- don\'t already have a symmetric encryption KMS key in your account,
    -- follow the instructions in
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html#create-symmetric-cmk Creating a symmetric encryption KMS key>
    -- in the /Amazon Web Services Key Management Service Developer Guide/.
    --
    -- You can choose the same symmetric encryption key when you create a CEV
    -- and a DB instance, or choose different keys.
    kmsKeyId :: Prelude.Text,
    -- | The CEV manifest, which is a JSON document that describes the
    -- installation .zip files stored in Amazon S3. Specify the name\/value
    -- pairs in a file or a quoted string. RDS Custom applies the patches in
    -- the order in which they are listed.
    --
    -- The following JSON fields are valid:
    --
    -- [MediaImportTemplateVersion]
    --     Version of the CEV manifest. The date is in the format @YYYY-MM-DD@.
    --
    -- [databaseInstallationFileNames]
    --     Ordered list of installation files for the CEV.
    --
    -- [opatchFileNames]
    --     Ordered list of OPatch installers used for the Oracle DB engine.
    --
    -- [psuRuPatchFileNames]
    --     The PSU and RU patches for this CEV.
    --
    -- [OtherPatchFileNames]
    --     The patches that are not in the list of PSU and RU patches. Amazon
    --     RDS applies these patches after applying the PSU and RU patches.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.html#custom-cev.preparing.manifest Creating the CEV manifest>
    -- in the /Amazon RDS User Guide/.
    manifest :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomDBEngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCustomDBEngineVersion_tags' - Undocumented member.
--
-- 'description', 'createCustomDBEngineVersion_description' - An optional description of your CEV.
--
-- 'databaseInstallationFilesS3Prefix', 'createCustomDBEngineVersion_databaseInstallationFilesS3Prefix' - The Amazon S3 directory that contains the database installation files
-- for your CEV. For example, a valid bucket name is @123456789012\/cev1@.
-- If this setting isn\'t specified, no prefix is assumed.
--
-- 'engine', 'createCustomDBEngineVersion_engine' - The database engine to use for your custom engine version (CEV). The
-- only supported value is @custom-oracle-ee@.
--
-- 'engineVersion', 'createCustomDBEngineVersion_engineVersion' - The name of your CEV. The name format is @19.customized_string @. For
-- example, a valid name is @19.my_cev1@. This setting is required for RDS
-- Custom for Oracle, but optional for Amazon RDS. The combination of
-- @Engine@ and @EngineVersion@ is unique per customer per Region.
--
-- 'databaseInstallationFilesS3BucketName', 'createCustomDBEngineVersion_databaseInstallationFilesS3BucketName' - The name of an Amazon S3 bucket that contains database installation
-- files for your CEV. For example, a valid bucket name is
-- @my-custom-installation-files@.
--
-- 'kmsKeyId', 'createCustomDBEngineVersion_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted CEV. A
-- symmetric encryption KMS key is required for RDS Custom, but optional
-- for Amazon RDS.
--
-- If you have an existing symmetric encryption KMS key in your account,
-- you can use it with RDS Custom. No further action is necessary. If you
-- don\'t already have a symmetric encryption KMS key in your account,
-- follow the instructions in
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html#create-symmetric-cmk Creating a symmetric encryption KMS key>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
--
-- You can choose the same symmetric encryption key when you create a CEV
-- and a DB instance, or choose different keys.
--
-- 'manifest', 'createCustomDBEngineVersion_manifest' - The CEV manifest, which is a JSON document that describes the
-- installation .zip files stored in Amazon S3. Specify the name\/value
-- pairs in a file or a quoted string. RDS Custom applies the patches in
-- the order in which they are listed.
--
-- The following JSON fields are valid:
--
-- [MediaImportTemplateVersion]
--     Version of the CEV manifest. The date is in the format @YYYY-MM-DD@.
--
-- [databaseInstallationFileNames]
--     Ordered list of installation files for the CEV.
--
-- [opatchFileNames]
--     Ordered list of OPatch installers used for the Oracle DB engine.
--
-- [psuRuPatchFileNames]
--     The PSU and RU patches for this CEV.
--
-- [OtherPatchFileNames]
--     The patches that are not in the list of PSU and RU patches. Amazon
--     RDS applies these patches after applying the PSU and RU patches.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.html#custom-cev.preparing.manifest Creating the CEV manifest>
-- in the /Amazon RDS User Guide/.
newCreateCustomDBEngineVersion ::
  -- | 'engine'
  Prelude.Text ->
  -- | 'engineVersion'
  Prelude.Text ->
  -- | 'databaseInstallationFilesS3BucketName'
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  -- | 'manifest'
  Prelude.Text ->
  CreateCustomDBEngineVersion
newCreateCustomDBEngineVersion
  pEngine_
  pEngineVersion_
  pDatabaseInstallationFilesS3BucketName_
  pKMSKeyId_
  pManifest_ =
    CreateCustomDBEngineVersion'
      { tags =
          Prelude.Nothing,
        description = Prelude.Nothing,
        databaseInstallationFilesS3Prefix =
          Prelude.Nothing,
        engine = pEngine_,
        engineVersion = pEngineVersion_,
        databaseInstallationFilesS3BucketName =
          pDatabaseInstallationFilesS3BucketName_,
        kmsKeyId = pKMSKeyId_,
        manifest = pManifest_
      }

-- | Undocumented member.
createCustomDBEngineVersion_tags :: Lens.Lens' CreateCustomDBEngineVersion (Prelude.Maybe [Tag])
createCustomDBEngineVersion_tags = Lens.lens (\CreateCustomDBEngineVersion' {tags} -> tags) (\s@CreateCustomDBEngineVersion' {} a -> s {tags = a} :: CreateCustomDBEngineVersion) Prelude.. Lens.mapping Lens.coerced

-- | An optional description of your CEV.
createCustomDBEngineVersion_description :: Lens.Lens' CreateCustomDBEngineVersion (Prelude.Maybe Prelude.Text)
createCustomDBEngineVersion_description = Lens.lens (\CreateCustomDBEngineVersion' {description} -> description) (\s@CreateCustomDBEngineVersion' {} a -> s {description = a} :: CreateCustomDBEngineVersion)

-- | The Amazon S3 directory that contains the database installation files
-- for your CEV. For example, a valid bucket name is @123456789012\/cev1@.
-- If this setting isn\'t specified, no prefix is assumed.
createCustomDBEngineVersion_databaseInstallationFilesS3Prefix :: Lens.Lens' CreateCustomDBEngineVersion (Prelude.Maybe Prelude.Text)
createCustomDBEngineVersion_databaseInstallationFilesS3Prefix = Lens.lens (\CreateCustomDBEngineVersion' {databaseInstallationFilesS3Prefix} -> databaseInstallationFilesS3Prefix) (\s@CreateCustomDBEngineVersion' {} a -> s {databaseInstallationFilesS3Prefix = a} :: CreateCustomDBEngineVersion)

-- | The database engine to use for your custom engine version (CEV). The
-- only supported value is @custom-oracle-ee@.
createCustomDBEngineVersion_engine :: Lens.Lens' CreateCustomDBEngineVersion Prelude.Text
createCustomDBEngineVersion_engine = Lens.lens (\CreateCustomDBEngineVersion' {engine} -> engine) (\s@CreateCustomDBEngineVersion' {} a -> s {engine = a} :: CreateCustomDBEngineVersion)

-- | The name of your CEV. The name format is @19.customized_string @. For
-- example, a valid name is @19.my_cev1@. This setting is required for RDS
-- Custom for Oracle, but optional for Amazon RDS. The combination of
-- @Engine@ and @EngineVersion@ is unique per customer per Region.
createCustomDBEngineVersion_engineVersion :: Lens.Lens' CreateCustomDBEngineVersion Prelude.Text
createCustomDBEngineVersion_engineVersion = Lens.lens (\CreateCustomDBEngineVersion' {engineVersion} -> engineVersion) (\s@CreateCustomDBEngineVersion' {} a -> s {engineVersion = a} :: CreateCustomDBEngineVersion)

-- | The name of an Amazon S3 bucket that contains database installation
-- files for your CEV. For example, a valid bucket name is
-- @my-custom-installation-files@.
createCustomDBEngineVersion_databaseInstallationFilesS3BucketName :: Lens.Lens' CreateCustomDBEngineVersion Prelude.Text
createCustomDBEngineVersion_databaseInstallationFilesS3BucketName = Lens.lens (\CreateCustomDBEngineVersion' {databaseInstallationFilesS3BucketName} -> databaseInstallationFilesS3BucketName) (\s@CreateCustomDBEngineVersion' {} a -> s {databaseInstallationFilesS3BucketName = a} :: CreateCustomDBEngineVersion)

-- | The Amazon Web Services KMS key identifier for an encrypted CEV. A
-- symmetric encryption KMS key is required for RDS Custom, but optional
-- for Amazon RDS.
--
-- If you have an existing symmetric encryption KMS key in your account,
-- you can use it with RDS Custom. No further action is necessary. If you
-- don\'t already have a symmetric encryption KMS key in your account,
-- follow the instructions in
-- <https://docs.aws.amazon.com/kms/latest/developerguide/create-keys.html#create-symmetric-cmk Creating a symmetric encryption KMS key>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
--
-- You can choose the same symmetric encryption key when you create a CEV
-- and a DB instance, or choose different keys.
createCustomDBEngineVersion_kmsKeyId :: Lens.Lens' CreateCustomDBEngineVersion Prelude.Text
createCustomDBEngineVersion_kmsKeyId = Lens.lens (\CreateCustomDBEngineVersion' {kmsKeyId} -> kmsKeyId) (\s@CreateCustomDBEngineVersion' {} a -> s {kmsKeyId = a} :: CreateCustomDBEngineVersion)

-- | The CEV manifest, which is a JSON document that describes the
-- installation .zip files stored in Amazon S3. Specify the name\/value
-- pairs in a file or a quoted string. RDS Custom applies the patches in
-- the order in which they are listed.
--
-- The following JSON fields are valid:
--
-- [MediaImportTemplateVersion]
--     Version of the CEV manifest. The date is in the format @YYYY-MM-DD@.
--
-- [databaseInstallationFileNames]
--     Ordered list of installation files for the CEV.
--
-- [opatchFileNames]
--     Ordered list of OPatch installers used for the Oracle DB engine.
--
-- [psuRuPatchFileNames]
--     The PSU and RU patches for this CEV.
--
-- [OtherPatchFileNames]
--     The patches that are not in the list of PSU and RU patches. Amazon
--     RDS applies these patches after applying the PSU and RU patches.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.html#custom-cev.preparing.manifest Creating the CEV manifest>
-- in the /Amazon RDS User Guide/.
createCustomDBEngineVersion_manifest :: Lens.Lens' CreateCustomDBEngineVersion Prelude.Text
createCustomDBEngineVersion_manifest = Lens.lens (\CreateCustomDBEngineVersion' {manifest} -> manifest) (\s@CreateCustomDBEngineVersion' {} a -> s {manifest = a} :: CreateCustomDBEngineVersion)

instance Core.AWSRequest CreateCustomDBEngineVersion where
  type
    AWSResponse CreateCustomDBEngineVersion =
      DBEngineVersion
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateCustomDBEngineVersionResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable CreateCustomDBEngineVersion where
  hashWithSalt _salt CreateCustomDBEngineVersion' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` databaseInstallationFilesS3Prefix
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` databaseInstallationFilesS3BucketName
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` manifest

instance Prelude.NFData CreateCustomDBEngineVersion where
  rnf CreateCustomDBEngineVersion' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf databaseInstallationFilesS3Prefix
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf databaseInstallationFilesS3BucketName
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf manifest

instance Data.ToHeaders CreateCustomDBEngineVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateCustomDBEngineVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCustomDBEngineVersion where
  toQuery CreateCustomDBEngineVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateCustomDBEngineVersion" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "Description" Data.=: description,
        "DatabaseInstallationFilesS3Prefix"
          Data.=: databaseInstallationFilesS3Prefix,
        "Engine" Data.=: engine,
        "EngineVersion" Data.=: engineVersion,
        "DatabaseInstallationFilesS3BucketName"
          Data.=: databaseInstallationFilesS3BucketName,
        "KMSKeyId" Data.=: kmsKeyId,
        "Manifest" Data.=: manifest
      ]

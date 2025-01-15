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
-- Module      : Amazonka.RDS.ModifyCustomDBEngineVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the status of a custom engine version (CEV). You can find CEVs
-- to modify by calling @DescribeDBEngineVersions@.
--
-- The MediaImport service that imports files from Amazon S3 to create CEVs
-- isn\'t integrated with Amazon Web Services CloudTrail. If you turn on
-- data logging for Amazon RDS in CloudTrail, calls to the
-- @ModifyCustomDbEngineVersion@ event aren\'t logged. However, you might
-- see calls from the API gateway that accesses your Amazon S3 bucket.
-- These calls originate from the MediaImport service for the
-- @ModifyCustomDbEngineVersion@ event.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.html#custom-cev.modify Modifying CEV status>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.ModifyCustomDBEngineVersion
  ( -- * Creating a Request
    ModifyCustomDBEngineVersion (..),
    newModifyCustomDBEngineVersion,

    -- * Request Lenses
    modifyCustomDBEngineVersion_description,
    modifyCustomDBEngineVersion_status,
    modifyCustomDBEngineVersion_engine,
    modifyCustomDBEngineVersion_engineVersion,

    -- * Destructuring the Response
    DBEngineVersion (..),
    newDBEngineVersion,

    -- * Response Lenses
    dbEngineVersion_createTime,
    dbEngineVersion_customDBEngineVersionManifest,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_dbEngineMediaType,
    dbEngineVersion_dbEngineVersionArn,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_databaseInstallationFilesS3BucketName,
    dbEngineVersion_databaseInstallationFilesS3Prefix,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_image,
    dbEngineVersion_kmsKeyId,
    dbEngineVersion_majorEngineVersion,
    dbEngineVersion_status,
    dbEngineVersion_supportedCACertificateIdentifiers,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_supportedEngineModes,
    dbEngineVersion_supportedFeatureNames,
    dbEngineVersion_supportedNcharCharacterSets,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsBabelfish,
    dbEngineVersion_supportsCertificateRotationWithoutRestart,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsParallelQuery,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_tagList,
    dbEngineVersion_validUpgradeTarget,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyCustomDBEngineVersion' smart constructor.
data ModifyCustomDBEngineVersion = ModifyCustomDBEngineVersion'
  { -- | An optional description of your CEV.
    description :: Prelude.Maybe Prelude.Text,
    -- | The availability status to be assigned to the CEV. Valid values are as
    -- follows:
    --
    -- [available]
    --     You can use this CEV to create a new RDS Custom DB instance.
    --
    -- [inactive]
    --     You can create a new RDS Custom instance by restoring a DB snapshot
    --     with this CEV. You can\'t patch or create new instances with this
    --     CEV.
    --
    -- You can change any status to any status. A typical reason to change
    -- status is to prevent the accidental use of a CEV, or to make a
    -- deprecated CEV eligible for use again. For example, you might change the
    -- status of your CEV from @available@ to @inactive@, and from @inactive@
    -- back to @available@. To change the availability status of the CEV, it
    -- must not currently be in use by an RDS Custom instance, snapshot, or
    -- automated backup.
    status :: Prelude.Maybe CustomEngineVersionStatus,
    -- | The DB engine. The only supported value is @custom-oracle-ee@.
    engine :: Prelude.Text,
    -- | The custom engine version (CEV) that you want to modify. This option is
    -- required for RDS Custom for Oracle, but optional for Amazon RDS. The
    -- combination of @Engine@ and @EngineVersion@ is unique per customer per
    -- Amazon Web Services Region.
    engineVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCustomDBEngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'modifyCustomDBEngineVersion_description' - An optional description of your CEV.
--
-- 'status', 'modifyCustomDBEngineVersion_status' - The availability status to be assigned to the CEV. Valid values are as
-- follows:
--
-- [available]
--     You can use this CEV to create a new RDS Custom DB instance.
--
-- [inactive]
--     You can create a new RDS Custom instance by restoring a DB snapshot
--     with this CEV. You can\'t patch or create new instances with this
--     CEV.
--
-- You can change any status to any status. A typical reason to change
-- status is to prevent the accidental use of a CEV, or to make a
-- deprecated CEV eligible for use again. For example, you might change the
-- status of your CEV from @available@ to @inactive@, and from @inactive@
-- back to @available@. To change the availability status of the CEV, it
-- must not currently be in use by an RDS Custom instance, snapshot, or
-- automated backup.
--
-- 'engine', 'modifyCustomDBEngineVersion_engine' - The DB engine. The only supported value is @custom-oracle-ee@.
--
-- 'engineVersion', 'modifyCustomDBEngineVersion_engineVersion' - The custom engine version (CEV) that you want to modify. This option is
-- required for RDS Custom for Oracle, but optional for Amazon RDS. The
-- combination of @Engine@ and @EngineVersion@ is unique per customer per
-- Amazon Web Services Region.
newModifyCustomDBEngineVersion ::
  -- | 'engine'
  Prelude.Text ->
  -- | 'engineVersion'
  Prelude.Text ->
  ModifyCustomDBEngineVersion
newModifyCustomDBEngineVersion
  pEngine_
  pEngineVersion_ =
    ModifyCustomDBEngineVersion'
      { description =
          Prelude.Nothing,
        status = Prelude.Nothing,
        engine = pEngine_,
        engineVersion = pEngineVersion_
      }

-- | An optional description of your CEV.
modifyCustomDBEngineVersion_description :: Lens.Lens' ModifyCustomDBEngineVersion (Prelude.Maybe Prelude.Text)
modifyCustomDBEngineVersion_description = Lens.lens (\ModifyCustomDBEngineVersion' {description} -> description) (\s@ModifyCustomDBEngineVersion' {} a -> s {description = a} :: ModifyCustomDBEngineVersion)

-- | The availability status to be assigned to the CEV. Valid values are as
-- follows:
--
-- [available]
--     You can use this CEV to create a new RDS Custom DB instance.
--
-- [inactive]
--     You can create a new RDS Custom instance by restoring a DB snapshot
--     with this CEV. You can\'t patch or create new instances with this
--     CEV.
--
-- You can change any status to any status. A typical reason to change
-- status is to prevent the accidental use of a CEV, or to make a
-- deprecated CEV eligible for use again. For example, you might change the
-- status of your CEV from @available@ to @inactive@, and from @inactive@
-- back to @available@. To change the availability status of the CEV, it
-- must not currently be in use by an RDS Custom instance, snapshot, or
-- automated backup.
modifyCustomDBEngineVersion_status :: Lens.Lens' ModifyCustomDBEngineVersion (Prelude.Maybe CustomEngineVersionStatus)
modifyCustomDBEngineVersion_status = Lens.lens (\ModifyCustomDBEngineVersion' {status} -> status) (\s@ModifyCustomDBEngineVersion' {} a -> s {status = a} :: ModifyCustomDBEngineVersion)

-- | The DB engine. The only supported value is @custom-oracle-ee@.
modifyCustomDBEngineVersion_engine :: Lens.Lens' ModifyCustomDBEngineVersion Prelude.Text
modifyCustomDBEngineVersion_engine = Lens.lens (\ModifyCustomDBEngineVersion' {engine} -> engine) (\s@ModifyCustomDBEngineVersion' {} a -> s {engine = a} :: ModifyCustomDBEngineVersion)

-- | The custom engine version (CEV) that you want to modify. This option is
-- required for RDS Custom for Oracle, but optional for Amazon RDS. The
-- combination of @Engine@ and @EngineVersion@ is unique per customer per
-- Amazon Web Services Region.
modifyCustomDBEngineVersion_engineVersion :: Lens.Lens' ModifyCustomDBEngineVersion Prelude.Text
modifyCustomDBEngineVersion_engineVersion = Lens.lens (\ModifyCustomDBEngineVersion' {engineVersion} -> engineVersion) (\s@ModifyCustomDBEngineVersion' {} a -> s {engineVersion = a} :: ModifyCustomDBEngineVersion)

instance Core.AWSRequest ModifyCustomDBEngineVersion where
  type
    AWSResponse ModifyCustomDBEngineVersion =
      DBEngineVersion
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyCustomDBEngineVersionResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable ModifyCustomDBEngineVersion where
  hashWithSalt _salt ModifyCustomDBEngineVersion' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData ModifyCustomDBEngineVersion where
  rnf ModifyCustomDBEngineVersion' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf engine `Prelude.seq`
          Prelude.rnf engineVersion

instance Data.ToHeaders ModifyCustomDBEngineVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyCustomDBEngineVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyCustomDBEngineVersion where
  toQuery ModifyCustomDBEngineVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyCustomDBEngineVersion" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Description" Data.=: description,
        "Status" Data.=: status,
        "Engine" Data.=: engine,
        "EngineVersion" Data.=: engineVersion
      ]

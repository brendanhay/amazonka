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
-- Module      : Amazonka.RDS.DeleteCustomDBEngineVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom engine version. To run this command, make sure you meet
-- the following prerequisites:
--
-- -   The CEV must not be the default for RDS Custom. If it is, change the
--     default before running this command.
--
-- -   The CEV must not be associated with an RDS Custom DB instance, RDS
--     Custom instance snapshot, or automated backup of your RDS Custom
--     instance.
--
-- Typically, deletion takes a few minutes.
--
-- The MediaImport service that imports files from Amazon S3 to create CEVs
-- isn\'t integrated with Amazon Web Services CloudTrail. If you turn on
-- data logging for Amazon RDS in CloudTrail, calls to the
-- @DeleteCustomDbEngineVersion@ event aren\'t logged. However, you might
-- see calls from the API gateway that accesses your Amazon S3 bucket.
-- These calls originate from the MediaImport service for the
-- @DeleteCustomDbEngineVersion@ event.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-cev.html#custom-cev.delete Deleting a CEV>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.DeleteCustomDBEngineVersion
  ( -- * Creating a Request
    DeleteCustomDBEngineVersion (..),
    newDeleteCustomDBEngineVersion,

    -- * Request Lenses
    deleteCustomDBEngineVersion_engine,
    deleteCustomDBEngineVersion_engineVersion,

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
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCustomDBEngineVersion' smart constructor.
data DeleteCustomDBEngineVersion = DeleteCustomDBEngineVersion'
  { -- | The database engine. The only supported engine is @custom-oracle-ee@.
    engine :: Prelude.Text,
    -- | The custom engine version (CEV) for your DB instance. This option is
    -- required for RDS Custom, but optional for Amazon RDS. The combination of
    -- @Engine@ and @EngineVersion@ is unique per customer per Amazon Web
    -- Services Region.
    engineVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomDBEngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engine', 'deleteCustomDBEngineVersion_engine' - The database engine. The only supported engine is @custom-oracle-ee@.
--
-- 'engineVersion', 'deleteCustomDBEngineVersion_engineVersion' - The custom engine version (CEV) for your DB instance. This option is
-- required for RDS Custom, but optional for Amazon RDS. The combination of
-- @Engine@ and @EngineVersion@ is unique per customer per Amazon Web
-- Services Region.
newDeleteCustomDBEngineVersion ::
  -- | 'engine'
  Prelude.Text ->
  -- | 'engineVersion'
  Prelude.Text ->
  DeleteCustomDBEngineVersion
newDeleteCustomDBEngineVersion
  pEngine_
  pEngineVersion_ =
    DeleteCustomDBEngineVersion'
      { engine = pEngine_,
        engineVersion = pEngineVersion_
      }

-- | The database engine. The only supported engine is @custom-oracle-ee@.
deleteCustomDBEngineVersion_engine :: Lens.Lens' DeleteCustomDBEngineVersion Prelude.Text
deleteCustomDBEngineVersion_engine = Lens.lens (\DeleteCustomDBEngineVersion' {engine} -> engine) (\s@DeleteCustomDBEngineVersion' {} a -> s {engine = a} :: DeleteCustomDBEngineVersion)

-- | The custom engine version (CEV) for your DB instance. This option is
-- required for RDS Custom, but optional for Amazon RDS. The combination of
-- @Engine@ and @EngineVersion@ is unique per customer per Amazon Web
-- Services Region.
deleteCustomDBEngineVersion_engineVersion :: Lens.Lens' DeleteCustomDBEngineVersion Prelude.Text
deleteCustomDBEngineVersion_engineVersion = Lens.lens (\DeleteCustomDBEngineVersion' {engineVersion} -> engineVersion) (\s@DeleteCustomDBEngineVersion' {} a -> s {engineVersion = a} :: DeleteCustomDBEngineVersion)

instance Core.AWSRequest DeleteCustomDBEngineVersion where
  type
    AWSResponse DeleteCustomDBEngineVersion =
      DBEngineVersion
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteCustomDBEngineVersionResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable DeleteCustomDBEngineVersion where
  hashWithSalt _salt DeleteCustomDBEngineVersion' {..} =
    _salt `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData DeleteCustomDBEngineVersion where
  rnf DeleteCustomDBEngineVersion' {..} =
    Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion

instance Core.ToHeaders DeleteCustomDBEngineVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteCustomDBEngineVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCustomDBEngineVersion where
  toQuery DeleteCustomDBEngineVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteCustomDBEngineVersion" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Engine" Core.=: engine,
        "EngineVersion" Core.=: engineVersion
      ]

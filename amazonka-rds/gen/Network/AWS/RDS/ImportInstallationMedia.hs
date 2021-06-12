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
-- Module      : Network.AWS.RDS.ImportInstallationMedia
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the installation media for a DB engine that requires an
-- on-premises customer provided license, such as SQL Server.
module Network.AWS.RDS.ImportInstallationMedia
  ( -- * Creating a Request
    ImportInstallationMedia (..),
    newImportInstallationMedia,

    -- * Request Lenses
    importInstallationMedia_customAvailabilityZoneId,
    importInstallationMedia_engine,
    importInstallationMedia_engineVersion,
    importInstallationMedia_engineInstallationMediaPath,
    importInstallationMedia_oSInstallationMediaPath,

    -- * Destructuring the Response
    InstallationMedia (..),
    newInstallationMedia,

    -- * Response Lenses
    installationMedia_status,
    installationMedia_customAvailabilityZoneId,
    installationMedia_installationMediaId,
    installationMedia_engineVersion,
    installationMedia_oSInstallationMediaPath,
    installationMedia_failureCause,
    installationMedia_engine,
    installationMedia_engineInstallationMediaPath,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportInstallationMedia' smart constructor.
data ImportInstallationMedia = ImportInstallationMedia'
  { -- | The identifier of the custom Availability Zone (AZ) to import the
    -- installation media to.
    customAvailabilityZoneId :: Core.Text,
    -- | The name of the database engine to be used for this instance.
    --
    -- The list only includes supported DB engines that require an on-premises
    -- customer provided license.
    --
    -- Valid Values:
    --
    -- -   @sqlserver-ee@
    --
    -- -   @sqlserver-se@
    --
    -- -   @sqlserver-ex@
    --
    -- -   @sqlserver-web@
    engine :: Core.Text,
    -- | The version number of the database engine to use.
    --
    -- For a list of valid engine versions, call DescribeDBEngineVersions.
    --
    -- The following are the database engines and links to information about
    -- the major and minor versions. The list only includes DB engines that
    -- require an on-premises customer provided license.
    --
    -- __Microsoft SQL Server__
    --
    -- See
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS>
    -- in the /Amazon RDS User Guide./
    engineVersion :: Core.Text,
    -- | The path to the installation medium for the specified DB engine.
    --
    -- Example:
    -- @SQLServerISO\/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
    engineInstallationMediaPath :: Core.Text,
    -- | The path to the installation medium for the operating system associated
    -- with the specified DB engine.
    --
    -- Example: @WindowsISO\/en_windows_server_2016_x64_dvd_9327751.iso@
    oSInstallationMediaPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportInstallationMedia' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAvailabilityZoneId', 'importInstallationMedia_customAvailabilityZoneId' - The identifier of the custom Availability Zone (AZ) to import the
-- installation media to.
--
-- 'engine', 'importInstallationMedia_engine' - The name of the database engine to be used for this instance.
--
-- The list only includes supported DB engines that require an on-premises
-- customer provided license.
--
-- Valid Values:
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
--
-- 'engineVersion', 'importInstallationMedia_engineVersion' - The version number of the database engine to use.
--
-- For a list of valid engine versions, call DescribeDBEngineVersions.
--
-- The following are the database engines and links to information about
-- the major and minor versions. The list only includes DB engines that
-- require an on-premises customer provided license.
--
-- __Microsoft SQL Server__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- 'engineInstallationMediaPath', 'importInstallationMedia_engineInstallationMediaPath' - The path to the installation medium for the specified DB engine.
--
-- Example:
-- @SQLServerISO\/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
--
-- 'oSInstallationMediaPath', 'importInstallationMedia_oSInstallationMediaPath' - The path to the installation medium for the operating system associated
-- with the specified DB engine.
--
-- Example: @WindowsISO\/en_windows_server_2016_x64_dvd_9327751.iso@
newImportInstallationMedia ::
  -- | 'customAvailabilityZoneId'
  Core.Text ->
  -- | 'engine'
  Core.Text ->
  -- | 'engineVersion'
  Core.Text ->
  -- | 'engineInstallationMediaPath'
  Core.Text ->
  -- | 'oSInstallationMediaPath'
  Core.Text ->
  ImportInstallationMedia
newImportInstallationMedia
  pCustomAvailabilityZoneId_
  pEngine_
  pEngineVersion_
  pEngineInstallationMediaPath_
  pOSInstallationMediaPath_ =
    ImportInstallationMedia'
      { customAvailabilityZoneId =
          pCustomAvailabilityZoneId_,
        engine = pEngine_,
        engineVersion = pEngineVersion_,
        engineInstallationMediaPath =
          pEngineInstallationMediaPath_,
        oSInstallationMediaPath =
          pOSInstallationMediaPath_
      }

-- | The identifier of the custom Availability Zone (AZ) to import the
-- installation media to.
importInstallationMedia_customAvailabilityZoneId :: Lens.Lens' ImportInstallationMedia Core.Text
importInstallationMedia_customAvailabilityZoneId = Lens.lens (\ImportInstallationMedia' {customAvailabilityZoneId} -> customAvailabilityZoneId) (\s@ImportInstallationMedia' {} a -> s {customAvailabilityZoneId = a} :: ImportInstallationMedia)

-- | The name of the database engine to be used for this instance.
--
-- The list only includes supported DB engines that require an on-premises
-- customer provided license.
--
-- Valid Values:
--
-- -   @sqlserver-ee@
--
-- -   @sqlserver-se@
--
-- -   @sqlserver-ex@
--
-- -   @sqlserver-web@
importInstallationMedia_engine :: Lens.Lens' ImportInstallationMedia Core.Text
importInstallationMedia_engine = Lens.lens (\ImportInstallationMedia' {engine} -> engine) (\s@ImportInstallationMedia' {} a -> s {engine = a} :: ImportInstallationMedia)

-- | The version number of the database engine to use.
--
-- For a list of valid engine versions, call DescribeDBEngineVersions.
--
-- The following are the database engines and links to information about
-- the major and minor versions. The list only includes DB engines that
-- require an on-premises customer provided license.
--
-- __Microsoft SQL Server__
--
-- See
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS>
-- in the /Amazon RDS User Guide./
importInstallationMedia_engineVersion :: Lens.Lens' ImportInstallationMedia Core.Text
importInstallationMedia_engineVersion = Lens.lens (\ImportInstallationMedia' {engineVersion} -> engineVersion) (\s@ImportInstallationMedia' {} a -> s {engineVersion = a} :: ImportInstallationMedia)

-- | The path to the installation medium for the specified DB engine.
--
-- Example:
-- @SQLServerISO\/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
importInstallationMedia_engineInstallationMediaPath :: Lens.Lens' ImportInstallationMedia Core.Text
importInstallationMedia_engineInstallationMediaPath = Lens.lens (\ImportInstallationMedia' {engineInstallationMediaPath} -> engineInstallationMediaPath) (\s@ImportInstallationMedia' {} a -> s {engineInstallationMediaPath = a} :: ImportInstallationMedia)

-- | The path to the installation medium for the operating system associated
-- with the specified DB engine.
--
-- Example: @WindowsISO\/en_windows_server_2016_x64_dvd_9327751.iso@
importInstallationMedia_oSInstallationMediaPath :: Lens.Lens' ImportInstallationMedia Core.Text
importInstallationMedia_oSInstallationMediaPath = Lens.lens (\ImportInstallationMedia' {oSInstallationMediaPath} -> oSInstallationMediaPath) (\s@ImportInstallationMedia' {} a -> s {oSInstallationMediaPath = a} :: ImportInstallationMedia)

instance Core.AWSRequest ImportInstallationMedia where
  type
    AWSResponse ImportInstallationMedia =
      InstallationMedia
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ImportInstallationMediaResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable ImportInstallationMedia

instance Core.NFData ImportInstallationMedia

instance Core.ToHeaders ImportInstallationMedia where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ImportInstallationMedia where
  toPath = Core.const "/"

instance Core.ToQuery ImportInstallationMedia where
  toQuery ImportInstallationMedia' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ImportInstallationMedia" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "CustomAvailabilityZoneId"
          Core.=: customAvailabilityZoneId,
        "Engine" Core.=: engine,
        "EngineVersion" Core.=: engineVersion,
        "EngineInstallationMediaPath"
          Core.=: engineInstallationMediaPath,
        "OSInstallationMediaPath"
          Core.=: oSInstallationMediaPath
      ]

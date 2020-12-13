{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ImportInstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports the installation media for a DB engine that requires an on-premises customer provided license, such as SQL Server.
module Network.AWS.RDS.ImportInstallationMedia
  ( -- * Creating a request
    ImportInstallationMedia (..),
    mkImportInstallationMedia,

    -- ** Request lenses
    iimEngineVersion,
    iimEngineInstallationMediaPath,
    iimEngine,
    iimOSInstallationMediaPath,
    iimCustomAvailabilityZoneId,

    -- * Destructuring the response
    InstallationMedia (..),
    mkInstallationMedia,

    -- ** Response lenses
    imEngineVersion,
    imStatus,
    imInstallationMediaId,
    imEngineInstallationMediaPath,
    imEngine,
    imOSInstallationMediaPath,
    imCustomAvailabilityZoneId,
    imFailureCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkImportInstallationMedia' smart constructor.
data ImportInstallationMedia = ImportInstallationMedia'
  { -- | The version number of the database engine to use.
    --
    -- For a list of valid engine versions, call 'DescribeDBEngineVersions' .
    -- The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license.
    -- __Microsoft SQL Server__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
    engineVersion :: Lude.Text,
    -- | The path to the installation medium for the specified DB engine.
    --
    -- Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
    engineInstallationMediaPath :: Lude.Text,
    -- | The name of the database engine to be used for this instance.
    --
    -- The list only includes supported DB engines that require an on-premises customer provided license.
    -- Valid Values:
    --
    --     * @sqlserver-ee@
    --
    --
    --     * @sqlserver-se@
    --
    --
    --     * @sqlserver-ex@
    --
    --
    --     * @sqlserver-web@
    engine :: Lude.Text,
    -- | The path to the installation medium for the operating system associated with the specified DB engine.
    --
    -- Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@
    osInstallationMediaPath :: Lude.Text,
    -- | The identifier of the custom Availability Zone (AZ) to import the installation media to.
    customAvailabilityZoneId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportInstallationMedia' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The version number of the database engine to use.
--
-- For a list of valid engine versions, call 'DescribeDBEngineVersions' .
-- The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license.
-- __Microsoft SQL Server__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
-- * 'engineInstallationMediaPath' - The path to the installation medium for the specified DB engine.
--
-- Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
-- * 'engine' - The name of the database engine to be used for this instance.
--
-- The list only includes supported DB engines that require an on-premises customer provided license.
-- Valid Values:
--
--     * @sqlserver-ee@
--
--
--     * @sqlserver-se@
--
--
--     * @sqlserver-ex@
--
--
--     * @sqlserver-web@
--
--
-- * 'osInstallationMediaPath' - The path to the installation medium for the operating system associated with the specified DB engine.
--
-- Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@
-- * 'customAvailabilityZoneId' - The identifier of the custom Availability Zone (AZ) to import the installation media to.
mkImportInstallationMedia ::
  -- | 'engineVersion'
  Lude.Text ->
  -- | 'engineInstallationMediaPath'
  Lude.Text ->
  -- | 'engine'
  Lude.Text ->
  -- | 'osInstallationMediaPath'
  Lude.Text ->
  -- | 'customAvailabilityZoneId'
  Lude.Text ->
  ImportInstallationMedia
mkImportInstallationMedia
  pEngineVersion_
  pEngineInstallationMediaPath_
  pEngine_
  pOSInstallationMediaPath_
  pCustomAvailabilityZoneId_ =
    ImportInstallationMedia'
      { engineVersion = pEngineVersion_,
        engineInstallationMediaPath = pEngineInstallationMediaPath_,
        engine = pEngine_,
        osInstallationMediaPath = pOSInstallationMediaPath_,
        customAvailabilityZoneId = pCustomAvailabilityZoneId_
      }

-- | The version number of the database engine to use.
--
-- For a list of valid engine versions, call 'DescribeDBEngineVersions' .
-- The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license.
-- __Microsoft SQL Server__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimEngineVersion :: Lens.Lens' ImportInstallationMedia Lude.Text
iimEngineVersion = Lens.lens (engineVersion :: ImportInstallationMedia -> Lude.Text) (\s a -> s {engineVersion = a} :: ImportInstallationMedia)
{-# DEPRECATED iimEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The path to the installation medium for the specified DB engine.
--
-- Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
--
-- /Note:/ Consider using 'engineInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimEngineInstallationMediaPath :: Lens.Lens' ImportInstallationMedia Lude.Text
iimEngineInstallationMediaPath = Lens.lens (engineInstallationMediaPath :: ImportInstallationMedia -> Lude.Text) (\s a -> s {engineInstallationMediaPath = a} :: ImportInstallationMedia)
{-# DEPRECATED iimEngineInstallationMediaPath "Use generic-lens or generic-optics with 'engineInstallationMediaPath' instead." #-}

-- | The name of the database engine to be used for this instance.
--
-- The list only includes supported DB engines that require an on-premises customer provided license.
-- Valid Values:
--
--     * @sqlserver-ee@
--
--
--     * @sqlserver-se@
--
--
--     * @sqlserver-ex@
--
--
--     * @sqlserver-web@
--
--
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimEngine :: Lens.Lens' ImportInstallationMedia Lude.Text
iimEngine = Lens.lens (engine :: ImportInstallationMedia -> Lude.Text) (\s a -> s {engine = a} :: ImportInstallationMedia)
{-# DEPRECATED iimEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The path to the installation medium for the operating system associated with the specified DB engine.
--
-- Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@
--
-- /Note:/ Consider using 'osInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimOSInstallationMediaPath :: Lens.Lens' ImportInstallationMedia Lude.Text
iimOSInstallationMediaPath = Lens.lens (osInstallationMediaPath :: ImportInstallationMedia -> Lude.Text) (\s a -> s {osInstallationMediaPath = a} :: ImportInstallationMedia)
{-# DEPRECATED iimOSInstallationMediaPath "Use generic-lens or generic-optics with 'osInstallationMediaPath' instead." #-}

-- | The identifier of the custom Availability Zone (AZ) to import the installation media to.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimCustomAvailabilityZoneId :: Lens.Lens' ImportInstallationMedia Lude.Text
iimCustomAvailabilityZoneId = Lens.lens (customAvailabilityZoneId :: ImportInstallationMedia -> Lude.Text) (\s a -> s {customAvailabilityZoneId = a} :: ImportInstallationMedia)
{-# DEPRECATED iimCustomAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead." #-}

instance Lude.AWSRequest ImportInstallationMedia where
  type Rs ImportInstallationMedia = InstallationMedia
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ImportInstallationMediaResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders ImportInstallationMedia where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ImportInstallationMedia where
  toPath = Lude.const "/"

instance Lude.ToQuery ImportInstallationMedia where
  toQuery ImportInstallationMedia' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ImportInstallationMedia" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "EngineInstallationMediaPath" Lude.=: engineInstallationMediaPath,
        "Engine" Lude.=: engine,
        "OSInstallationMediaPath" Lude.=: osInstallationMediaPath,
        "CustomAvailabilityZoneId" Lude.=: customAvailabilityZoneId
      ]

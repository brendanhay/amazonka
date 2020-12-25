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
    iimCustomAvailabilityZoneId,
    iimEngine,
    iimEngineVersion,
    iimEngineInstallationMediaPath,
    iimOSInstallationMediaPath,

    -- * Destructuring the response
    Types.InstallationMedia (..),
    Types.mkInstallationMedia,

    -- ** Response lenses
    Types.imCustomAvailabilityZoneId,
    Types.imEngine,
    Types.imEngineInstallationMediaPath,
    Types.imEngineVersion,
    Types.imFailureCause,
    Types.imInstallationMediaId,
    Types.imOSInstallationMediaPath,
    Types.imStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportInstallationMedia' smart constructor.
data ImportInstallationMedia = ImportInstallationMedia'
  { -- | The identifier of the custom Availability Zone (AZ) to import the installation media to.
    customAvailabilityZoneId :: Types.String,
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
    engine :: Types.String,
    -- | The version number of the database engine to use.
    --
    -- For a list of valid engine versions, call 'DescribeDBEngineVersions' .
    -- The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license.
    -- __Microsoft SQL Server__
    -- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
    engineVersion :: Types.String,
    -- | The path to the installation medium for the specified DB engine.
    --
    -- Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
    engineInstallationMediaPath :: Types.String,
    -- | The path to the installation medium for the operating system associated with the specified DB engine.
    --
    -- Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@
    oSInstallationMediaPath :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportInstallationMedia' value with any optional fields omitted.
mkImportInstallationMedia ::
  -- | 'customAvailabilityZoneId'
  Types.String ->
  -- | 'engine'
  Types.String ->
  -- | 'engineVersion'
  Types.String ->
  -- | 'engineInstallationMediaPath'
  Types.String ->
  -- | 'oSInstallationMediaPath'
  Types.String ->
  ImportInstallationMedia
mkImportInstallationMedia
  customAvailabilityZoneId
  engine
  engineVersion
  engineInstallationMediaPath
  oSInstallationMediaPath =
    ImportInstallationMedia'
      { customAvailabilityZoneId,
        engine,
        engineVersion,
        engineInstallationMediaPath,
        oSInstallationMediaPath
      }

-- | The identifier of the custom Availability Zone (AZ) to import the installation media to.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimCustomAvailabilityZoneId :: Lens.Lens' ImportInstallationMedia Types.String
iimCustomAvailabilityZoneId = Lens.field @"customAvailabilityZoneId"
{-# DEPRECATED iimCustomAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead." #-}

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
iimEngine :: Lens.Lens' ImportInstallationMedia Types.String
iimEngine = Lens.field @"engine"
{-# DEPRECATED iimEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version number of the database engine to use.
--
-- For a list of valid engine versions, call 'DescribeDBEngineVersions' .
-- The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license.
-- __Microsoft SQL Server__
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimEngineVersion :: Lens.Lens' ImportInstallationMedia Types.String
iimEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED iimEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The path to the installation medium for the specified DB engine.
--
-- Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@
--
-- /Note:/ Consider using 'engineInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimEngineInstallationMediaPath :: Lens.Lens' ImportInstallationMedia Types.String
iimEngineInstallationMediaPath = Lens.field @"engineInstallationMediaPath"
{-# DEPRECATED iimEngineInstallationMediaPath "Use generic-lens or generic-optics with 'engineInstallationMediaPath' instead." #-}

-- | The path to the installation medium for the operating system associated with the specified DB engine.
--
-- Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@
--
-- /Note:/ Consider using 'oSInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimOSInstallationMediaPath :: Lens.Lens' ImportInstallationMedia Types.String
iimOSInstallationMediaPath = Lens.field @"oSInstallationMediaPath"
{-# DEPRECATED iimOSInstallationMediaPath "Use generic-lens or generic-optics with 'oSInstallationMediaPath' instead." #-}

instance Core.AWSRequest ImportInstallationMedia where
  type Rs ImportInstallationMedia = Types.InstallationMedia
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ImportInstallationMedia")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "CustomAvailabilityZoneId"
                            customAvailabilityZoneId
                        )
                Core.<> (Core.toQueryValue "Engine" engine)
                Core.<> (Core.toQueryValue "EngineVersion" engineVersion)
                Core.<> ( Core.toQueryValue
                            "EngineInstallationMediaPath"
                            engineInstallationMediaPath
                        )
                Core.<> ( Core.toQueryValue
                            "OSInstallationMediaPath"
                            oSInstallationMediaPath
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ImportInstallationMediaResult"
      (\s h x -> Core.parseXML x)

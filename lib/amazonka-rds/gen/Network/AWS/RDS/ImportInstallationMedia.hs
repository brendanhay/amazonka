{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ImportInstallationMedia (..)
    , mkImportInstallationMedia
    -- ** Request lenses
    , iimCustomAvailabilityZoneId
    , iimEngine
    , iimEngineVersion
    , iimEngineInstallationMediaPath
    , iimOSInstallationMediaPath

     -- * Destructuring the response
    , Types.InstallationMedia (..)
    , Types.mkInstallationMedia
    -- ** Response lenses
    , Types.imCustomAvailabilityZoneId
    , Types.imEngine
    , Types.imEngineInstallationMediaPath
    , Types.imEngineVersion
    , Types.imFailureCause
    , Types.imInstallationMediaId
    , Types.imOSInstallationMediaPath
    , Types.imStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportInstallationMedia' smart constructor.
data ImportInstallationMedia = ImportInstallationMedia'
  { customAvailabilityZoneId :: Core.Text
    -- ^ The identifier of the custom Availability Zone (AZ) to import the installation media to.
  , engine :: Core.Text
    -- ^ The name of the database engine to be used for this instance. 
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
  , engineVersion :: Core.Text
    -- ^ The version number of the database engine to use.
--
-- For a list of valid engine versions, call 'DescribeDBEngineVersions' .
-- The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license.
-- __Microsoft SQL Server__ 
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./ 
  , engineInstallationMediaPath :: Core.Text
    -- ^ The path to the installation medium for the specified DB engine.
--
-- Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@ 
  , oSInstallationMediaPath :: Core.Text
    -- ^ The path to the installation medium for the operating system associated with the specified DB engine.
--
-- Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportInstallationMedia' value with any optional fields omitted.
mkImportInstallationMedia
    :: Core.Text -- ^ 'customAvailabilityZoneId'
    -> Core.Text -- ^ 'engine'
    -> Core.Text -- ^ 'engineVersion'
    -> Core.Text -- ^ 'engineInstallationMediaPath'
    -> Core.Text -- ^ 'oSInstallationMediaPath'
    -> ImportInstallationMedia
mkImportInstallationMedia customAvailabilityZoneId engine
  engineVersion engineInstallationMediaPath oSInstallationMediaPath
  = ImportInstallationMedia'{customAvailabilityZoneId, engine,
                             engineVersion, engineInstallationMediaPath,
                             oSInstallationMediaPath}

-- | The identifier of the custom Availability Zone (AZ) to import the installation media to.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimCustomAvailabilityZoneId :: Lens.Lens' ImportInstallationMedia Core.Text
iimCustomAvailabilityZoneId = Lens.field @"customAvailabilityZoneId"
{-# INLINEABLE iimCustomAvailabilityZoneId #-}
{-# DEPRECATED customAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead"  #-}

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
iimEngine :: Lens.Lens' ImportInstallationMedia Core.Text
iimEngine = Lens.field @"engine"
{-# INLINEABLE iimEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The version number of the database engine to use.
--
-- For a list of valid engine versions, call 'DescribeDBEngineVersions' .
-- The following are the database engines and links to information about the major and minor versions. The list only includes DB engines that require an on-premises customer provided license.
-- __Microsoft SQL Server__ 
-- See <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_SQLServer.html#SQLServer.Concepts.General.VersionSupport Microsoft SQL Server Versions on Amazon RDS> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimEngineVersion :: Lens.Lens' ImportInstallationMedia Core.Text
iimEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE iimEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The path to the installation medium for the specified DB engine.
--
-- Example: @SQLServerISO/en_sql_server_2016_enterprise_x64_dvd_8701793.iso@ 
--
-- /Note:/ Consider using 'engineInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimEngineInstallationMediaPath :: Lens.Lens' ImportInstallationMedia Core.Text
iimEngineInstallationMediaPath = Lens.field @"engineInstallationMediaPath"
{-# INLINEABLE iimEngineInstallationMediaPath #-}
{-# DEPRECATED engineInstallationMediaPath "Use generic-lens or generic-optics with 'engineInstallationMediaPath' instead"  #-}

-- | The path to the installation medium for the operating system associated with the specified DB engine.
--
-- Example: @WindowsISO/en_windows_server_2016_x64_dvd_9327751.iso@ 
--
-- /Note:/ Consider using 'oSInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iimOSInstallationMediaPath :: Lens.Lens' ImportInstallationMedia Core.Text
iimOSInstallationMediaPath = Lens.field @"oSInstallationMediaPath"
{-# INLINEABLE iimOSInstallationMediaPath #-}
{-# DEPRECATED oSInstallationMediaPath "Use generic-lens or generic-optics with 'oSInstallationMediaPath' instead"  #-}

instance Core.ToQuery ImportInstallationMedia where
        toQuery ImportInstallationMedia{..}
          = Core.toQueryPair "Action"
              ("ImportInstallationMedia" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "CustomAvailabilityZoneId"
                customAvailabilityZoneId
              Core.<> Core.toQueryPair "Engine" engine
              Core.<> Core.toQueryPair "EngineVersion" engineVersion
              Core.<>
              Core.toQueryPair "EngineInstallationMediaPath"
                engineInstallationMediaPath
              Core.<>
              Core.toQueryPair "OSInstallationMediaPath" oSInstallationMediaPath

instance Core.ToHeaders ImportInstallationMedia where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ImportInstallationMedia where
        type Rs ImportInstallationMedia = Types.InstallationMedia
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ImportInstallationMediaResult"
              (\ s h x -> Core.parseXML x)
        
        {-# INLINE parseResponse #-}

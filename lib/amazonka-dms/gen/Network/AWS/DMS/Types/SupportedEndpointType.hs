{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.SupportedEndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SupportedEndpointType
  ( SupportedEndpointType (..),

    -- * Smart constructor
    mkSupportedEndpointType,

    -- * Lenses
    setEndpointType,
    setEngineDisplayName,
    setEngineName,
    setReplicationInstanceEngineMinimumVersion,
    setSupportsCDC,
  )
where

import qualified Network.AWS.DMS.Types.ReplicationEndpointTypeValue as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about types of supported endpoints in response to a request by the @DescribeEndpointTypes@ operation. This information includes the type of endpoint, the database engine name, and whether change data capture (CDC) is supported.
--
-- /See:/ 'mkSupportedEndpointType' smart constructor.
data SupportedEndpointType = SupportedEndpointType'
  { -- | The type of endpoint. Valid values are @source@ and @target@ .
    endpointType :: Core.Maybe Types.ReplicationEndpointTypeValue,
    -- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
    engineDisplayName :: Core.Maybe Types.String,
    -- | The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
    engineName :: Core.Maybe Types.String,
    -- | The earliest AWS DMS engine version that supports this endpoint engine. Note that endpoint engines released with AWS DMS versions earlier than 3.1.1 do not return a value for this parameter.
    replicationInstanceEngineMinimumVersion :: Core.Maybe Types.String,
    -- | Indicates if Change Data Capture (CDC) is supported.
    supportsCDC :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SupportedEndpointType' value with any optional fields omitted.
mkSupportedEndpointType ::
  SupportedEndpointType
mkSupportedEndpointType =
  SupportedEndpointType'
    { endpointType = Core.Nothing,
      engineDisplayName = Core.Nothing,
      engineName = Core.Nothing,
      replicationInstanceEngineMinimumVersion = Core.Nothing,
      supportsCDC = Core.Nothing
    }

-- | The type of endpoint. Valid values are @source@ and @target@ .
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setEndpointType :: Lens.Lens' SupportedEndpointType (Core.Maybe Types.ReplicationEndpointTypeValue)
setEndpointType = Lens.field @"endpointType"
{-# DEPRECATED setEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
--
-- /Note:/ Consider using 'engineDisplayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setEngineDisplayName :: Lens.Lens' SupportedEndpointType (Core.Maybe Types.String)
setEngineDisplayName = Lens.field @"engineDisplayName"
{-# DEPRECATED setEngineDisplayName "Use generic-lens or generic-optics with 'engineDisplayName' instead." #-}

-- | The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setEngineName :: Lens.Lens' SupportedEndpointType (Core.Maybe Types.String)
setEngineName = Lens.field @"engineName"
{-# DEPRECATED setEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | The earliest AWS DMS engine version that supports this endpoint engine. Note that endpoint engines released with AWS DMS versions earlier than 3.1.1 do not return a value for this parameter.
--
-- /Note:/ Consider using 'replicationInstanceEngineMinimumVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setReplicationInstanceEngineMinimumVersion :: Lens.Lens' SupportedEndpointType (Core.Maybe Types.String)
setReplicationInstanceEngineMinimumVersion = Lens.field @"replicationInstanceEngineMinimumVersion"
{-# DEPRECATED setReplicationInstanceEngineMinimumVersion "Use generic-lens or generic-optics with 'replicationInstanceEngineMinimumVersion' instead." #-}

-- | Indicates if Change Data Capture (CDC) is supported.
--
-- /Note:/ Consider using 'supportsCDC' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setSupportsCDC :: Lens.Lens' SupportedEndpointType (Core.Maybe Core.Bool)
setSupportsCDC = Lens.field @"supportsCDC"
{-# DEPRECATED setSupportsCDC "Use generic-lens or generic-optics with 'supportsCDC' instead." #-}

instance Core.FromJSON SupportedEndpointType where
  parseJSON =
    Core.withObject "SupportedEndpointType" Core.$
      \x ->
        SupportedEndpointType'
          Core.<$> (x Core..:? "EndpointType")
          Core.<*> (x Core..:? "EngineDisplayName")
          Core.<*> (x Core..:? "EngineName")
          Core.<*> (x Core..:? "ReplicationInstanceEngineMinimumVersion")
          Core.<*> (x Core..:? "SupportsCDC")

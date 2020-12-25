{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBEngineVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBEngineVersion
  ( DBEngineVersion (..),

    -- * Smart constructor
    mkDBEngineVersion,

    -- * Lenses
    dbevDBEngineDescription,
    dbevDBEngineVersionDescription,
    dbevDBParameterGroupFamily,
    dbevDefaultCharacterSet,
    dbevEngine,
    dbevEngineVersion,
    dbevExportableLogTypes,
    dbevStatus,
    dbevSupportedCharacterSets,
    dbevSupportedEngineModes,
    dbevSupportedFeatureNames,
    dbevSupportedNcharCharacterSets,
    dbevSupportedTimezones,
    dbevSupportsGlobalDatabases,
    dbevSupportsLogExportsToCloudwatchLogs,
    dbevSupportsParallelQuery,
    dbevSupportsReadReplica,
    dbevValidUpgradeTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.CharacterSet as Types
import qualified Network.AWS.RDS.Types.DBEngineDescription as Types
import qualified Network.AWS.RDS.Types.DBEngineVersionDescription as Types
import qualified Network.AWS.RDS.Types.DBParameterGroupFamily as Types
import qualified Network.AWS.RDS.Types.Engine as Types
import qualified Network.AWS.RDS.Types.EngineVersion as Types
import qualified Network.AWS.RDS.Types.Status as Types
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.Timezone as Types
import qualified Network.AWS.RDS.Types.UpgradeTarget as Types

-- | This data type is used as a response element in the action @DescribeDBEngineVersions@ .
--
-- /See:/ 'mkDBEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { -- | The description of the database engine.
    dBEngineDescription :: Core.Maybe Types.DBEngineDescription,
    -- | The description of the database engine version.
    dBEngineVersionDescription :: Core.Maybe Types.DBEngineVersionDescription,
    -- | The name of the DB parameter group family for the database engine.
    dBParameterGroupFamily :: Core.Maybe Types.DBParameterGroupFamily,
    -- | The default character set for new instances of this engine version, if the @CharacterSetName@ parameter of the CreateDBInstance API isn't specified.
    defaultCharacterSet :: Core.Maybe Types.CharacterSet,
    -- | The name of the database engine.
    engine :: Core.Maybe Types.Engine,
    -- | The version number of the database engine.
    engineVersion :: Core.Maybe Types.EngineVersion,
    -- | The types of logs that the database engine has available for export to CloudWatch Logs.
    exportableLogTypes :: Core.Maybe [Types.String],
    -- | The status of the DB engine version, either @available@ or @deprecated@ .
    status :: Core.Maybe Types.Status,
    -- | A list of the character sets supported by this engine for the @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedCharacterSets :: Core.Maybe [Types.CharacterSet],
    -- | A list of the supported DB engine modes.
    supportedEngineModes :: Core.Maybe [Types.String],
    -- | A list of features supported by the DB engine. Supported feature names include the following.
    --
    --
    --     * s3Import
    supportedFeatureNames :: Core.Maybe [Types.String],
    -- | A list of the character sets supported by the Oracle DB engine for the @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
    supportedNcharCharacterSets :: Core.Maybe [Types.CharacterSet],
    -- | A list of the time zones supported by this engine for the @Timezone@ parameter of the @CreateDBInstance@ action.
    supportedTimezones :: Core.Maybe [Types.Timezone],
    -- | A value that indicates whether you can use Aurora global databases with a specific DB engine version.
    supportsGlobalDatabases :: Core.Maybe Core.Bool,
    -- | A value that indicates whether the engine version supports exporting the log types specified by ExportableLogTypes to CloudWatch Logs.
    supportsLogExportsToCloudwatchLogs :: Core.Maybe Core.Bool,
    -- | A value that indicates whether you can use Aurora parallel query with a specific DB engine version.
    supportsParallelQuery :: Core.Maybe Core.Bool,
    -- | Indicates whether the database engine version supports read replicas.
    supportsReadReplica :: Core.Maybe Core.Bool,
    -- | A list of engine versions that this database engine version can be upgraded to.
    validUpgradeTarget :: Core.Maybe [Types.UpgradeTarget]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DBEngineVersion' value with any optional fields omitted.
mkDBEngineVersion ::
  DBEngineVersion
mkDBEngineVersion =
  DBEngineVersion'
    { dBEngineDescription = Core.Nothing,
      dBEngineVersionDescription = Core.Nothing,
      dBParameterGroupFamily = Core.Nothing,
      defaultCharacterSet = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      exportableLogTypes = Core.Nothing,
      status = Core.Nothing,
      supportedCharacterSets = Core.Nothing,
      supportedEngineModes = Core.Nothing,
      supportedFeatureNames = Core.Nothing,
      supportedNcharCharacterSets = Core.Nothing,
      supportedTimezones = Core.Nothing,
      supportsGlobalDatabases = Core.Nothing,
      supportsLogExportsToCloudwatchLogs = Core.Nothing,
      supportsParallelQuery = Core.Nothing,
      supportsReadReplica = Core.Nothing,
      validUpgradeTarget = Core.Nothing
    }

-- | The description of the database engine.
--
-- /Note:/ Consider using 'dBEngineDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevDBEngineDescription :: Lens.Lens' DBEngineVersion (Core.Maybe Types.DBEngineDescription)
dbevDBEngineDescription = Lens.field @"dBEngineDescription"
{-# DEPRECATED dbevDBEngineDescription "Use generic-lens or generic-optics with 'dBEngineDescription' instead." #-}

-- | The description of the database engine version.
--
-- /Note:/ Consider using 'dBEngineVersionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevDBEngineVersionDescription :: Lens.Lens' DBEngineVersion (Core.Maybe Types.DBEngineVersionDescription)
dbevDBEngineVersionDescription = Lens.field @"dBEngineVersionDescription"
{-# DEPRECATED dbevDBEngineVersionDescription "Use generic-lens or generic-optics with 'dBEngineVersionDescription' instead." #-}

-- | The name of the DB parameter group family for the database engine.
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevDBParameterGroupFamily :: Lens.Lens' DBEngineVersion (Core.Maybe Types.DBParameterGroupFamily)
dbevDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# DEPRECATED dbevDBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead." #-}

-- | The default character set for new instances of this engine version, if the @CharacterSetName@ parameter of the CreateDBInstance API isn't specified.
--
-- /Note:/ Consider using 'defaultCharacterSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevDefaultCharacterSet :: Lens.Lens' DBEngineVersion (Core.Maybe Types.CharacterSet)
dbevDefaultCharacterSet = Lens.field @"defaultCharacterSet"
{-# DEPRECATED dbevDefaultCharacterSet "Use generic-lens or generic-optics with 'defaultCharacterSet' instead." #-}

-- | The name of the database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevEngine :: Lens.Lens' DBEngineVersion (Core.Maybe Types.Engine)
dbevEngine = Lens.field @"engine"
{-# DEPRECATED dbevEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version number of the database engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevEngineVersion :: Lens.Lens' DBEngineVersion (Core.Maybe Types.EngineVersion)
dbevEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED dbevEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The types of logs that the database engine has available for export to CloudWatch Logs.
--
-- /Note:/ Consider using 'exportableLogTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevExportableLogTypes :: Lens.Lens' DBEngineVersion (Core.Maybe [Types.String])
dbevExportableLogTypes = Lens.field @"exportableLogTypes"
{-# DEPRECATED dbevExportableLogTypes "Use generic-lens or generic-optics with 'exportableLogTypes' instead." #-}

-- | The status of the DB engine version, either @available@ or @deprecated@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevStatus :: Lens.Lens' DBEngineVersion (Core.Maybe Types.Status)
dbevStatus = Lens.field @"status"
{-# DEPRECATED dbevStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of the character sets supported by this engine for the @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- /Note:/ Consider using 'supportedCharacterSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportedCharacterSets :: Lens.Lens' DBEngineVersion (Core.Maybe [Types.CharacterSet])
dbevSupportedCharacterSets = Lens.field @"supportedCharacterSets"
{-# DEPRECATED dbevSupportedCharacterSets "Use generic-lens or generic-optics with 'supportedCharacterSets' instead." #-}

-- | A list of the supported DB engine modes.
--
-- /Note:/ Consider using 'supportedEngineModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportedEngineModes :: Lens.Lens' DBEngineVersion (Core.Maybe [Types.String])
dbevSupportedEngineModes = Lens.field @"supportedEngineModes"
{-# DEPRECATED dbevSupportedEngineModes "Use generic-lens or generic-optics with 'supportedEngineModes' instead." #-}

-- | A list of features supported by the DB engine. Supported feature names include the following.
--
--
--     * s3Import
--
--
--
-- /Note:/ Consider using 'supportedFeatureNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportedFeatureNames :: Lens.Lens' DBEngineVersion (Core.Maybe [Types.String])
dbevSupportedFeatureNames = Lens.field @"supportedFeatureNames"
{-# DEPRECATED dbevSupportedFeatureNames "Use generic-lens or generic-optics with 'supportedFeatureNames' instead." #-}

-- | A list of the character sets supported by the Oracle DB engine for the @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- /Note:/ Consider using 'supportedNcharCharacterSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportedNcharCharacterSets :: Lens.Lens' DBEngineVersion (Core.Maybe [Types.CharacterSet])
dbevSupportedNcharCharacterSets = Lens.field @"supportedNcharCharacterSets"
{-# DEPRECATED dbevSupportedNcharCharacterSets "Use generic-lens or generic-optics with 'supportedNcharCharacterSets' instead." #-}

-- | A list of the time zones supported by this engine for the @Timezone@ parameter of the @CreateDBInstance@ action.
--
-- /Note:/ Consider using 'supportedTimezones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportedTimezones :: Lens.Lens' DBEngineVersion (Core.Maybe [Types.Timezone])
dbevSupportedTimezones = Lens.field @"supportedTimezones"
{-# DEPRECATED dbevSupportedTimezones "Use generic-lens or generic-optics with 'supportedTimezones' instead." #-}

-- | A value that indicates whether you can use Aurora global databases with a specific DB engine version.
--
-- /Note:/ Consider using 'supportsGlobalDatabases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportsGlobalDatabases :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Bool)
dbevSupportsGlobalDatabases = Lens.field @"supportsGlobalDatabases"
{-# DEPRECATED dbevSupportsGlobalDatabases "Use generic-lens or generic-optics with 'supportsGlobalDatabases' instead." #-}

-- | A value that indicates whether the engine version supports exporting the log types specified by ExportableLogTypes to CloudWatch Logs.
--
-- /Note:/ Consider using 'supportsLogExportsToCloudwatchLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportsLogExportsToCloudwatchLogs :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Bool)
dbevSupportsLogExportsToCloudwatchLogs = Lens.field @"supportsLogExportsToCloudwatchLogs"
{-# DEPRECATED dbevSupportsLogExportsToCloudwatchLogs "Use generic-lens or generic-optics with 'supportsLogExportsToCloudwatchLogs' instead." #-}

-- | A value that indicates whether you can use Aurora parallel query with a specific DB engine version.
--
-- /Note:/ Consider using 'supportsParallelQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportsParallelQuery :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Bool)
dbevSupportsParallelQuery = Lens.field @"supportsParallelQuery"
{-# DEPRECATED dbevSupportsParallelQuery "Use generic-lens or generic-optics with 'supportsParallelQuery' instead." #-}

-- | Indicates whether the database engine version supports read replicas.
--
-- /Note:/ Consider using 'supportsReadReplica' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevSupportsReadReplica :: Lens.Lens' DBEngineVersion (Core.Maybe Core.Bool)
dbevSupportsReadReplica = Lens.field @"supportsReadReplica"
{-# DEPRECATED dbevSupportsReadReplica "Use generic-lens or generic-optics with 'supportsReadReplica' instead." #-}

-- | A list of engine versions that this database engine version can be upgraded to.
--
-- /Note:/ Consider using 'validUpgradeTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbevValidUpgradeTarget :: Lens.Lens' DBEngineVersion (Core.Maybe [Types.UpgradeTarget])
dbevValidUpgradeTarget = Lens.field @"validUpgradeTarget"
{-# DEPRECATED dbevValidUpgradeTarget "Use generic-lens or generic-optics with 'validUpgradeTarget' instead." #-}

instance Core.FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      Core.<$> (x Core..@? "DBEngineDescription")
      Core.<*> (x Core..@? "DBEngineVersionDescription")
      Core.<*> (x Core..@? "DBParameterGroupFamily")
      Core.<*> (x Core..@? "DefaultCharacterSet")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> ( x Core..@? "ExportableLogTypes"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "Status")
      Core.<*> ( x Core..@? "SupportedCharacterSets"
                   Core..<@> Core.parseXMLList "CharacterSet"
               )
      Core.<*> ( x Core..@? "SupportedEngineModes"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "SupportedFeatureNames"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> ( x Core..@? "SupportedNcharCharacterSets"
                   Core..<@> Core.parseXMLList "CharacterSet"
               )
      Core.<*> ( x Core..@? "SupportedTimezones"
                   Core..<@> Core.parseXMLList "Timezone"
               )
      Core.<*> (x Core..@? "SupportsGlobalDatabases")
      Core.<*> (x Core..@? "SupportsLogExportsToCloudwatchLogs")
      Core.<*> (x Core..@? "SupportsParallelQuery")
      Core.<*> (x Core..@? "SupportsReadReplica")
      Core.<*> ( x Core..@? "ValidUpgradeTarget"
                   Core..<@> Core.parseXMLList "UpgradeTarget"
               )

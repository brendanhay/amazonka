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
    devEngineVersion,
    devStatus,
    devDBEngineVersionDescription,
    devSupportedEngineModes,
    devDefaultCharacterSet,
    devEngine,
    devDBParameterGroupFamily,
    devSupportedCharacterSets,
    devDBEngineDescription,
    devSupportsGlobalDatabases,
    devValidUpgradeTarget,
    devSupportsParallelQuery,
    devSupportedNcharCharacterSets,
    devSupportsLogExportsToCloudwatchLogs,
    devSupportsReadReplica,
    devSupportedFeatureNames,
    devSupportedTimezones,
    devExportableLogTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.CharacterSet
import Network.AWS.RDS.Types.Timezone
import Network.AWS.RDS.Types.UpgradeTarget

-- | This data type is used as a response element in the action @DescribeDBEngineVersions@ .
--
-- /See:/ 'mkDBEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    dbEngineVersionDescription :: Lude.Maybe Lude.Text,
    supportedEngineModes :: Lude.Maybe [Lude.Text],
    defaultCharacterSet :: Lude.Maybe CharacterSet,
    engine :: Lude.Maybe Lude.Text,
    dbParameterGroupFamily :: Lude.Maybe Lude.Text,
    supportedCharacterSets :: Lude.Maybe [CharacterSet],
    dbEngineDescription :: Lude.Maybe Lude.Text,
    supportsGlobalDatabases :: Lude.Maybe Lude.Bool,
    validUpgradeTarget :: Lude.Maybe [UpgradeTarget],
    supportsParallelQuery :: Lude.Maybe Lude.Bool,
    supportedNcharCharacterSets :: Lude.Maybe [CharacterSet],
    supportsLogExportsToCloudwatchLogs :: Lude.Maybe Lude.Bool,
    supportsReadReplica :: Lude.Maybe Lude.Bool,
    supportedFeatureNames :: Lude.Maybe [Lude.Text],
    supportedTimezones :: Lude.Maybe [Timezone],
    exportableLogTypes :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBEngineVersion' with the minimum fields required to make a request.
--
-- * 'dbEngineDescription' - The description of the database engine.
-- * 'dbEngineVersionDescription' - The description of the database engine version.
-- * 'dbParameterGroupFamily' - The name of the DB parameter group family for the database engine.
-- * 'defaultCharacterSet' - The default character set for new instances of this engine version, if the @CharacterSetName@ parameter of the CreateDBInstance API isn't specified.
-- * 'engine' - The name of the database engine.
-- * 'engineVersion' - The version number of the database engine.
-- * 'exportableLogTypes' - The types of logs that the database engine has available for export to CloudWatch Logs.
-- * 'status' - The status of the DB engine version, either @available@ or @deprecated@ .
-- * 'supportedCharacterSets' - A list of the character sets supported by this engine for the @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
-- * 'supportedEngineModes' - A list of the supported DB engine modes.
-- * 'supportedFeatureNames' - A list of features supported by the DB engine. Supported feature names include the following.
--
--
--     * s3Import
--
--
-- * 'supportedNcharCharacterSets' - A list of the character sets supported by the Oracle DB engine for the @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
-- * 'supportedTimezones' - A list of the time zones supported by this engine for the @Timezone@ parameter of the @CreateDBInstance@ action.
-- * 'supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with a specific DB engine version.
-- * 'supportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the log types specified by ExportableLogTypes to CloudWatch Logs.
-- * 'supportsParallelQuery' - A value that indicates whether you can use Aurora parallel query with a specific DB engine version.
-- * 'supportsReadReplica' - Indicates whether the database engine version supports read replicas.
-- * 'validUpgradeTarget' - A list of engine versions that this database engine version can be upgraded to.
mkDBEngineVersion ::
  DBEngineVersion
mkDBEngineVersion =
  DBEngineVersion'
    { engineVersion = Lude.Nothing,
      status = Lude.Nothing,
      dbEngineVersionDescription = Lude.Nothing,
      supportedEngineModes = Lude.Nothing,
      defaultCharacterSet = Lude.Nothing,
      engine = Lude.Nothing,
      dbParameterGroupFamily = Lude.Nothing,
      supportedCharacterSets = Lude.Nothing,
      dbEngineDescription = Lude.Nothing,
      supportsGlobalDatabases = Lude.Nothing,
      validUpgradeTarget = Lude.Nothing,
      supportsParallelQuery = Lude.Nothing,
      supportedNcharCharacterSets = Lude.Nothing,
      supportsLogExportsToCloudwatchLogs = Lude.Nothing,
      supportsReadReplica = Lude.Nothing,
      supportedFeatureNames = Lude.Nothing,
      supportedTimezones = Lude.Nothing,
      exportableLogTypes = Lude.Nothing
    }

-- | The version number of the database engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devEngineVersion :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Text)
devEngineVersion = Lens.lens (engineVersion :: DBEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DBEngineVersion)
{-# DEPRECATED devEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The status of the DB engine version, either @available@ or @deprecated@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devStatus :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Text)
devStatus = Lens.lens (status :: DBEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBEngineVersion)
{-# DEPRECATED devStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The description of the database engine version.
--
-- /Note:/ Consider using 'dbEngineVersionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devDBEngineVersionDescription :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Text)
devDBEngineVersionDescription = Lens.lens (dbEngineVersionDescription :: DBEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {dbEngineVersionDescription = a} :: DBEngineVersion)
{-# DEPRECATED devDBEngineVersionDescription "Use generic-lens or generic-optics with 'dbEngineVersionDescription' instead." #-}

-- | A list of the supported DB engine modes.
--
-- /Note:/ Consider using 'supportedEngineModes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportedEngineModes :: Lens.Lens' DBEngineVersion (Lude.Maybe [Lude.Text])
devSupportedEngineModes = Lens.lens (supportedEngineModes :: DBEngineVersion -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedEngineModes = a} :: DBEngineVersion)
{-# DEPRECATED devSupportedEngineModes "Use generic-lens or generic-optics with 'supportedEngineModes' instead." #-}

-- | The default character set for new instances of this engine version, if the @CharacterSetName@ parameter of the CreateDBInstance API isn't specified.
--
-- /Note:/ Consider using 'defaultCharacterSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devDefaultCharacterSet :: Lens.Lens' DBEngineVersion (Lude.Maybe CharacterSet)
devDefaultCharacterSet = Lens.lens (defaultCharacterSet :: DBEngineVersion -> Lude.Maybe CharacterSet) (\s a -> s {defaultCharacterSet = a} :: DBEngineVersion)
{-# DEPRECATED devDefaultCharacterSet "Use generic-lens or generic-optics with 'defaultCharacterSet' instead." #-}

-- | The name of the database engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devEngine :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Text)
devEngine = Lens.lens (engine :: DBEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DBEngineVersion)
{-# DEPRECATED devEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The name of the DB parameter group family for the database engine.
--
-- /Note:/ Consider using 'dbParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devDBParameterGroupFamily :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Text)
devDBParameterGroupFamily = Lens.lens (dbParameterGroupFamily :: DBEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupFamily = a} :: DBEngineVersion)
{-# DEPRECATED devDBParameterGroupFamily "Use generic-lens or generic-optics with 'dbParameterGroupFamily' instead." #-}

-- | A list of the character sets supported by this engine for the @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- /Note:/ Consider using 'supportedCharacterSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportedCharacterSets :: Lens.Lens' DBEngineVersion (Lude.Maybe [CharacterSet])
devSupportedCharacterSets = Lens.lens (supportedCharacterSets :: DBEngineVersion -> Lude.Maybe [CharacterSet]) (\s a -> s {supportedCharacterSets = a} :: DBEngineVersion)
{-# DEPRECATED devSupportedCharacterSets "Use generic-lens or generic-optics with 'supportedCharacterSets' instead." #-}

-- | The description of the database engine.
--
-- /Note:/ Consider using 'dbEngineDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devDBEngineDescription :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Text)
devDBEngineDescription = Lens.lens (dbEngineDescription :: DBEngineVersion -> Lude.Maybe Lude.Text) (\s a -> s {dbEngineDescription = a} :: DBEngineVersion)
{-# DEPRECATED devDBEngineDescription "Use generic-lens or generic-optics with 'dbEngineDescription' instead." #-}

-- | A value that indicates whether you can use Aurora global databases with a specific DB engine version.
--
-- /Note:/ Consider using 'supportsGlobalDatabases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportsGlobalDatabases :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Bool)
devSupportsGlobalDatabases = Lens.lens (supportsGlobalDatabases :: DBEngineVersion -> Lude.Maybe Lude.Bool) (\s a -> s {supportsGlobalDatabases = a} :: DBEngineVersion)
{-# DEPRECATED devSupportsGlobalDatabases "Use generic-lens or generic-optics with 'supportsGlobalDatabases' instead." #-}

-- | A list of engine versions that this database engine version can be upgraded to.
--
-- /Note:/ Consider using 'validUpgradeTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devValidUpgradeTarget :: Lens.Lens' DBEngineVersion (Lude.Maybe [UpgradeTarget])
devValidUpgradeTarget = Lens.lens (validUpgradeTarget :: DBEngineVersion -> Lude.Maybe [UpgradeTarget]) (\s a -> s {validUpgradeTarget = a} :: DBEngineVersion)
{-# DEPRECATED devValidUpgradeTarget "Use generic-lens or generic-optics with 'validUpgradeTarget' instead." #-}

-- | A value that indicates whether you can use Aurora parallel query with a specific DB engine version.
--
-- /Note:/ Consider using 'supportsParallelQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportsParallelQuery :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Bool)
devSupportsParallelQuery = Lens.lens (supportsParallelQuery :: DBEngineVersion -> Lude.Maybe Lude.Bool) (\s a -> s {supportsParallelQuery = a} :: DBEngineVersion)
{-# DEPRECATED devSupportsParallelQuery "Use generic-lens or generic-optics with 'supportsParallelQuery' instead." #-}

-- | A list of the character sets supported by the Oracle DB engine for the @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- /Note:/ Consider using 'supportedNcharCharacterSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportedNcharCharacterSets :: Lens.Lens' DBEngineVersion (Lude.Maybe [CharacterSet])
devSupportedNcharCharacterSets = Lens.lens (supportedNcharCharacterSets :: DBEngineVersion -> Lude.Maybe [CharacterSet]) (\s a -> s {supportedNcharCharacterSets = a} :: DBEngineVersion)
{-# DEPRECATED devSupportedNcharCharacterSets "Use generic-lens or generic-optics with 'supportedNcharCharacterSets' instead." #-}

-- | A value that indicates whether the engine version supports exporting the log types specified by ExportableLogTypes to CloudWatch Logs.
--
-- /Note:/ Consider using 'supportsLogExportsToCloudwatchLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportsLogExportsToCloudwatchLogs :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Bool)
devSupportsLogExportsToCloudwatchLogs = Lens.lens (supportsLogExportsToCloudwatchLogs :: DBEngineVersion -> Lude.Maybe Lude.Bool) (\s a -> s {supportsLogExportsToCloudwatchLogs = a} :: DBEngineVersion)
{-# DEPRECATED devSupportsLogExportsToCloudwatchLogs "Use generic-lens or generic-optics with 'supportsLogExportsToCloudwatchLogs' instead." #-}

-- | Indicates whether the database engine version supports read replicas.
--
-- /Note:/ Consider using 'supportsReadReplica' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportsReadReplica :: Lens.Lens' DBEngineVersion (Lude.Maybe Lude.Bool)
devSupportsReadReplica = Lens.lens (supportsReadReplica :: DBEngineVersion -> Lude.Maybe Lude.Bool) (\s a -> s {supportsReadReplica = a} :: DBEngineVersion)
{-# DEPRECATED devSupportsReadReplica "Use generic-lens or generic-optics with 'supportsReadReplica' instead." #-}

-- | A list of features supported by the DB engine. Supported feature names include the following.
--
--
--     * s3Import
--
--
--
-- /Note:/ Consider using 'supportedFeatureNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportedFeatureNames :: Lens.Lens' DBEngineVersion (Lude.Maybe [Lude.Text])
devSupportedFeatureNames = Lens.lens (supportedFeatureNames :: DBEngineVersion -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedFeatureNames = a} :: DBEngineVersion)
{-# DEPRECATED devSupportedFeatureNames "Use generic-lens or generic-optics with 'supportedFeatureNames' instead." #-}

-- | A list of the time zones supported by this engine for the @Timezone@ parameter of the @CreateDBInstance@ action.
--
-- /Note:/ Consider using 'supportedTimezones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devSupportedTimezones :: Lens.Lens' DBEngineVersion (Lude.Maybe [Timezone])
devSupportedTimezones = Lens.lens (supportedTimezones :: DBEngineVersion -> Lude.Maybe [Timezone]) (\s a -> s {supportedTimezones = a} :: DBEngineVersion)
{-# DEPRECATED devSupportedTimezones "Use generic-lens or generic-optics with 'supportedTimezones' instead." #-}

-- | The types of logs that the database engine has available for export to CloudWatch Logs.
--
-- /Note:/ Consider using 'exportableLogTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
devExportableLogTypes :: Lens.Lens' DBEngineVersion (Lude.Maybe [Lude.Text])
devExportableLogTypes = Lens.lens (exportableLogTypes :: DBEngineVersion -> Lude.Maybe [Lude.Text]) (\s a -> s {exportableLogTypes = a} :: DBEngineVersion)
{-# DEPRECATED devExportableLogTypes "Use generic-lens or generic-optics with 'exportableLogTypes' instead." #-}

instance Lude.FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DBEngineVersionDescription")
      Lude.<*> ( x Lude..@? "SupportedEngineModes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "DefaultCharacterSet")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "DBParameterGroupFamily")
      Lude.<*> ( x Lude..@? "SupportedCharacterSets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CharacterSet")
               )
      Lude.<*> (x Lude..@? "DBEngineDescription")
      Lude.<*> (x Lude..@? "SupportsGlobalDatabases")
      Lude.<*> ( x Lude..@? "ValidUpgradeTarget" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "UpgradeTarget")
               )
      Lude.<*> (x Lude..@? "SupportsParallelQuery")
      Lude.<*> ( x Lude..@? "SupportedNcharCharacterSets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CharacterSet")
               )
      Lude.<*> (x Lude..@? "SupportsLogExportsToCloudwatchLogs")
      Lude.<*> (x Lude..@? "SupportsReadReplica")
      Lude.<*> ( x Lude..@? "SupportedFeatureNames" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "SupportedTimezones" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Timezone")
               )
      Lude.<*> ( x Lude..@? "ExportableLogTypes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )

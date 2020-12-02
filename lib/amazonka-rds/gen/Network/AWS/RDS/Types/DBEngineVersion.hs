{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBEngineVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBEngineVersion where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.CharacterSet
import Network.AWS.RDS.Types.Timezone
import Network.AWS.RDS.Types.UpgradeTarget

-- | This data type is used as a response element in the action @DescribeDBEngineVersions@ .
--
--
--
-- /See:/ 'dbEngineVersion' smart constructor.
data DBEngineVersion = DBEngineVersion'
  { _devEngineVersion ::
      !(Maybe Text),
    _devStatus :: !(Maybe Text),
    _devDBEngineVersionDescription :: !(Maybe Text),
    _devSupportedEngineModes :: !(Maybe [Text]),
    _devDefaultCharacterSet :: !(Maybe CharacterSet),
    _devEngine :: !(Maybe Text),
    _devDBParameterGroupFamily :: !(Maybe Text),
    _devSupportedCharacterSets :: !(Maybe [CharacterSet]),
    _devDBEngineDescription :: !(Maybe Text),
    _devSupportsGlobalDatabases :: !(Maybe Bool),
    _devValidUpgradeTarget :: !(Maybe [UpgradeTarget]),
    _devSupportsParallelQuery :: !(Maybe Bool),
    _devSupportedNcharCharacterSets :: !(Maybe [CharacterSet]),
    _devSupportsLogExportsToCloudwatchLogs :: !(Maybe Bool),
    _devSupportsReadReplica :: !(Maybe Bool),
    _devSupportedFeatureNames :: !(Maybe [Text]),
    _devSupportedTimezones :: !(Maybe [Timezone]),
    _devExportableLogTypes :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBEngineVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'devEngineVersion' - The version number of the database engine.
--
-- * 'devStatus' - The status of the DB engine version, either @available@ or @deprecated@ .
--
-- * 'devDBEngineVersionDescription' - The description of the database engine version.
--
-- * 'devSupportedEngineModes' - A list of the supported DB engine modes.
--
-- * 'devDefaultCharacterSet' - The default character set for new instances of this engine version, if the @CharacterSetName@ parameter of the CreateDBInstance API isn't specified.
--
-- * 'devEngine' - The name of the database engine.
--
-- * 'devDBParameterGroupFamily' - The name of the DB parameter group family for the database engine.
--
-- * 'devSupportedCharacterSets' - A list of the character sets supported by this engine for the @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- * 'devDBEngineDescription' - The description of the database engine.
--
-- * 'devSupportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with a specific DB engine version.
--
-- * 'devValidUpgradeTarget' - A list of engine versions that this database engine version can be upgraded to.
--
-- * 'devSupportsParallelQuery' - A value that indicates whether you can use Aurora parallel query with a specific DB engine version.
--
-- * 'devSupportedNcharCharacterSets' - A list of the character sets supported by the Oracle DB engine for the @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
--
-- * 'devSupportsLogExportsToCloudwatchLogs' - A value that indicates whether the engine version supports exporting the log types specified by ExportableLogTypes to CloudWatch Logs.
--
-- * 'devSupportsReadReplica' - Indicates whether the database engine version supports read replicas.
--
-- * 'devSupportedFeatureNames' - A list of features supported by the DB engine. Supported feature names include the following.      * s3Import
--
-- * 'devSupportedTimezones' - A list of the time zones supported by this engine for the @Timezone@ parameter of the @CreateDBInstance@ action.
--
-- * 'devExportableLogTypes' - The types of logs that the database engine has available for export to CloudWatch Logs.
dbEngineVersion ::
  DBEngineVersion
dbEngineVersion =
  DBEngineVersion'
    { _devEngineVersion = Nothing,
      _devStatus = Nothing,
      _devDBEngineVersionDescription = Nothing,
      _devSupportedEngineModes = Nothing,
      _devDefaultCharacterSet = Nothing,
      _devEngine = Nothing,
      _devDBParameterGroupFamily = Nothing,
      _devSupportedCharacterSets = Nothing,
      _devDBEngineDescription = Nothing,
      _devSupportsGlobalDatabases = Nothing,
      _devValidUpgradeTarget = Nothing,
      _devSupportsParallelQuery = Nothing,
      _devSupportedNcharCharacterSets = Nothing,
      _devSupportsLogExportsToCloudwatchLogs = Nothing,
      _devSupportsReadReplica = Nothing,
      _devSupportedFeatureNames = Nothing,
      _devSupportedTimezones = Nothing,
      _devExportableLogTypes = Nothing
    }

-- | The version number of the database engine.
devEngineVersion :: Lens' DBEngineVersion (Maybe Text)
devEngineVersion = lens _devEngineVersion (\s a -> s {_devEngineVersion = a})

-- | The status of the DB engine version, either @available@ or @deprecated@ .
devStatus :: Lens' DBEngineVersion (Maybe Text)
devStatus = lens _devStatus (\s a -> s {_devStatus = a})

-- | The description of the database engine version.
devDBEngineVersionDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineVersionDescription = lens _devDBEngineVersionDescription (\s a -> s {_devDBEngineVersionDescription = a})

-- | A list of the supported DB engine modes.
devSupportedEngineModes :: Lens' DBEngineVersion [Text]
devSupportedEngineModes = lens _devSupportedEngineModes (\s a -> s {_devSupportedEngineModes = a}) . _Default . _Coerce

-- | The default character set for new instances of this engine version, if the @CharacterSetName@ parameter of the CreateDBInstance API isn't specified.
devDefaultCharacterSet :: Lens' DBEngineVersion (Maybe CharacterSet)
devDefaultCharacterSet = lens _devDefaultCharacterSet (\s a -> s {_devDefaultCharacterSet = a})

-- | The name of the database engine.
devEngine :: Lens' DBEngineVersion (Maybe Text)
devEngine = lens _devEngine (\s a -> s {_devEngine = a})

-- | The name of the DB parameter group family for the database engine.
devDBParameterGroupFamily :: Lens' DBEngineVersion (Maybe Text)
devDBParameterGroupFamily = lens _devDBParameterGroupFamily (\s a -> s {_devDBParameterGroupFamily = a})

-- | A list of the character sets supported by this engine for the @CharacterSetName@ parameter of the @CreateDBInstance@ operation.
devSupportedCharacterSets :: Lens' DBEngineVersion [CharacterSet]
devSupportedCharacterSets = lens _devSupportedCharacterSets (\s a -> s {_devSupportedCharacterSets = a}) . _Default . _Coerce

-- | The description of the database engine.
devDBEngineDescription :: Lens' DBEngineVersion (Maybe Text)
devDBEngineDescription = lens _devDBEngineDescription (\s a -> s {_devDBEngineDescription = a})

-- | A value that indicates whether you can use Aurora global databases with a specific DB engine version.
devSupportsGlobalDatabases :: Lens' DBEngineVersion (Maybe Bool)
devSupportsGlobalDatabases = lens _devSupportsGlobalDatabases (\s a -> s {_devSupportsGlobalDatabases = a})

-- | A list of engine versions that this database engine version can be upgraded to.
devValidUpgradeTarget :: Lens' DBEngineVersion [UpgradeTarget]
devValidUpgradeTarget = lens _devValidUpgradeTarget (\s a -> s {_devValidUpgradeTarget = a}) . _Default . _Coerce

-- | A value that indicates whether you can use Aurora parallel query with a specific DB engine version.
devSupportsParallelQuery :: Lens' DBEngineVersion (Maybe Bool)
devSupportsParallelQuery = lens _devSupportsParallelQuery (\s a -> s {_devSupportsParallelQuery = a})

-- | A list of the character sets supported by the Oracle DB engine for the @NcharCharacterSetName@ parameter of the @CreateDBInstance@ operation.
devSupportedNcharCharacterSets :: Lens' DBEngineVersion [CharacterSet]
devSupportedNcharCharacterSets = lens _devSupportedNcharCharacterSets (\s a -> s {_devSupportedNcharCharacterSets = a}) . _Default . _Coerce

-- | A value that indicates whether the engine version supports exporting the log types specified by ExportableLogTypes to CloudWatch Logs.
devSupportsLogExportsToCloudwatchLogs :: Lens' DBEngineVersion (Maybe Bool)
devSupportsLogExportsToCloudwatchLogs = lens _devSupportsLogExportsToCloudwatchLogs (\s a -> s {_devSupportsLogExportsToCloudwatchLogs = a})

-- | Indicates whether the database engine version supports read replicas.
devSupportsReadReplica :: Lens' DBEngineVersion (Maybe Bool)
devSupportsReadReplica = lens _devSupportsReadReplica (\s a -> s {_devSupportsReadReplica = a})

-- | A list of features supported by the DB engine. Supported feature names include the following.      * s3Import
devSupportedFeatureNames :: Lens' DBEngineVersion [Text]
devSupportedFeatureNames = lens _devSupportedFeatureNames (\s a -> s {_devSupportedFeatureNames = a}) . _Default . _Coerce

-- | A list of the time zones supported by this engine for the @Timezone@ parameter of the @CreateDBInstance@ action.
devSupportedTimezones :: Lens' DBEngineVersion [Timezone]
devSupportedTimezones = lens _devSupportedTimezones (\s a -> s {_devSupportedTimezones = a}) . _Default . _Coerce

-- | The types of logs that the database engine has available for export to CloudWatch Logs.
devExportableLogTypes :: Lens' DBEngineVersion [Text]
devExportableLogTypes = lens _devExportableLogTypes (\s a -> s {_devExportableLogTypes = a}) . _Default . _Coerce

instance FromXML DBEngineVersion where
  parseXML x =
    DBEngineVersion'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "Status")
      <*> (x .@? "DBEngineVersionDescription")
      <*> ( x .@? "SupportedEngineModes" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "DefaultCharacterSet")
      <*> (x .@? "Engine")
      <*> (x .@? "DBParameterGroupFamily")
      <*> ( x .@? "SupportedCharacterSets" .!@ mempty
              >>= may (parseXMLList "CharacterSet")
          )
      <*> (x .@? "DBEngineDescription")
      <*> (x .@? "SupportsGlobalDatabases")
      <*> ( x .@? "ValidUpgradeTarget" .!@ mempty
              >>= may (parseXMLList "UpgradeTarget")
          )
      <*> (x .@? "SupportsParallelQuery")
      <*> ( x .@? "SupportedNcharCharacterSets" .!@ mempty
              >>= may (parseXMLList "CharacterSet")
          )
      <*> (x .@? "SupportsLogExportsToCloudwatchLogs")
      <*> (x .@? "SupportsReadReplica")
      <*> ( x .@? "SupportedFeatureNames" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> ( x .@? "SupportedTimezones" .!@ mempty
              >>= may (parseXMLList "Timezone")
          )
      <*> ( x .@? "ExportableLogTypes" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable DBEngineVersion

instance NFData DBEngineVersion

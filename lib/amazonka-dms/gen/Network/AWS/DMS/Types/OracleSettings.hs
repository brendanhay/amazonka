{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.OracleSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.OracleSettings where

import Network.AWS.DMS.Types.CharLengthSemantics
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines an Oracle endpoint.
--
--
--
-- /See:/ 'oracleSettings' smart constructor.
data OracleSettings = OracleSettings'
  { _osFailTasksOnLobTruncation ::
      !(Maybe Bool),
    _osServerName :: !(Maybe Text),
    _osDirectPathNoLog :: !(Maybe Bool),
    _osSecurityDBEncryptionName :: !(Maybe Text),
    _osOraclePathPrefix :: !(Maybe Text),
    _osUsername :: !(Maybe Text),
    _osAllowSelectNestedTables :: !(Maybe Bool),
    _osReadAheadBlocks :: !(Maybe Int),
    _osArchivedLogDestId :: !(Maybe Int),
    _osReplacePathPrefix :: !(Maybe Bool),
    _osAccessAlternateDirectly :: !(Maybe Bool),
    _osSecurityDBEncryption :: !(Maybe (Sensitive Text)),
    _osReadTableSpaceName :: !(Maybe Bool),
    _osRetryInterval :: !(Maybe Int),
    _osPassword :: !(Maybe (Sensitive Text)),
    _osDatabaseName :: !(Maybe Text),
    _osAddSupplementalLogging :: !(Maybe Bool),
    _osAsmServer :: !(Maybe Text),
    _osCharLengthSemantics :: !(Maybe CharLengthSemantics),
    _osArchivedLogsOnly :: !(Maybe Bool),
    _osDirectPathParallelLoad :: !(Maybe Bool),
    _osAdditionalArchivedLogDestId :: !(Maybe Int),
    _osAsmPassword :: !(Maybe (Sensitive Text)),
    _osEnableHomogenousTablespace :: !(Maybe Bool),
    _osParallelAsmReadThreads :: !(Maybe Int),
    _osNumberDatatypeScale :: !(Maybe Int),
    _osUsePathPrefix :: !(Maybe Text),
    _osAsmUser :: !(Maybe Text),
    _osUseAlternateFolderForOnline :: !(Maybe Bool),
    _osPort :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'OracleSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osFailTasksOnLobTruncation' - When set to @true@ , this attribute causes a task to fail if the actual size of an LOB column is greater than the specified @LobMaxSize@ . If a task is set to limited LOB mode and this option is set to @true@ , the task fails instead of truncating the LOB data.
--
-- * 'osServerName' - Fully qualified domain name of the endpoint.
--
-- * 'osDirectPathNoLog' - When set to @true@ , this attribute helps to increase the commit rate on the Oracle target database by writing directly to tables and not writing a trail to database logs.
--
-- * 'osSecurityDBEncryptionName' - For an Oracle source endpoint, the name of a key used for the transparent data encryption (TDE) of the columns and tablespaces in an Oracle source database that is encrypted using TDE. The key value is the value of the @SecurityDbEncryption@ setting. For more information on setting the key name value of @SecurityDbEncryptionName@ , see the information and example for setting the @securityDbEncryptionName@ extra connection attribute in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- * 'osOraclePathPrefix' - Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the default Oracle root used to access the redo logs.
--
-- * 'osUsername' - Endpoint connection user name.
--
-- * 'osAllowSelectNestedTables' - Set this attribute to @true@ to enable replication of Oracle tables containing columns that are nested tables or defined types.
--
-- * 'osReadAheadBlocks' - Set this attribute to change the number of read-ahead blocks that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 1000 (the default) and 200,000 (the maximum).
--
-- * 'osArchivedLogDestId' - Specifies the destination of the archived redo logs. The value should be the same as the DEST_ID number in the v$archived_log table. When working with multiple log destinations (DEST_ID), we recommend that you to specify an archived redo logs location identifier. Doing this improves performance by ensuring that the correct logs are accessed from the outset.
--
-- * 'osReplacePathPrefix' - Set this attribute to true in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This setting tells DMS instance to replace the default Oracle root with the specified @usePathPrefix@ setting to access the redo logs.
--
-- * 'osAccessAlternateDirectly' - Set this attribute to @false@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to not access redo logs through any specified path prefix replacement using direct file access.
--
-- * 'osSecurityDBEncryption' - For an Oracle source endpoint, the transparent data encryption (TDE) password required by AWM DMS to access Oracle redo logs encrypted by TDE using Binary Reader. It is also the @/TDE_Password/ @ part of the comma-separated value you set to the @Password@ request parameter when you create the endpoint. The @SecurityDbEncryptian@ setting is related to this @SecurityDbEncryptionName@ setting. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- * 'osReadTableSpaceName' - When set to @true@ , this attribute supports tablespace replication.
--
-- * 'osRetryInterval' - Specifies the number of seconds that the system waits before resending a query. Example: @retryInterval=6;@
--
-- * 'osPassword' - Endpoint connection password.
--
-- * 'osDatabaseName' - Database name for the endpoint.
--
-- * 'osAddSupplementalLogging' - Set this attribute to set up table-level supplemental logging for the Oracle database. This attribute enables PRIMARY KEY supplemental logging on all tables selected for a migration task. If you use this option, you still need to enable database-level supplemental logging.
--
-- * 'osAsmServer' - For an Oracle source endpoint, your ASM server address. You can set this value from the @asm_server@ value. You set @asm_server@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- * 'osCharLengthSemantics' - Specifies whether the length of a character column is in bytes or in characters. To indicate that the character column length is in characters, set this attribute to @CHAR@ . Otherwise, the character column length is in bytes. Example: @charLengthSemantics=CHAR;@
--
-- * 'osArchivedLogsOnly' - When this field is set to @Y@ , AWS DMS only accesses the archived redo logs. If the archived redo logs are stored on Oracle ASM only, the AWS DMS user account needs to be granted ASM privileges.
--
-- * 'osDirectPathParallelLoad' - When set to @true@ , this attribute specifies a parallel load when @useDirectPathFullLoad@ is set to @Y@ . This attribute also only applies when you use the AWS DMS parallel load feature. Note that the target table cannot have any constraints or indexes.
--
-- * 'osAdditionalArchivedLogDestId' - Set this attribute with @archivedLogDestId@ in a primary/ standby setup. This attribute is useful in the case of a switchover. In this case, AWS DMS needs to know which destination to get archive redo logs from to read changes. This need arises because the previous primary instance is now a standby instance after switchover.
--
-- * 'osAsmPassword' - For an Oracle source endpoint, your Oracle Automatic Storage Management (ASM) password. You can set this value from the @/asm_user_password/ @ value. You set this value as part of the comma-separated value that you set to the @Password@ request parameter when you create the endpoint to access transaction logs using Binary Reader. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- * 'osEnableHomogenousTablespace' - Set this attribute to enable homogenous tablespace replication and create existing tables or indexes under the same tablespace on the target.
--
-- * 'osParallelAsmReadThreads' - Set this attribute to change the number of threads that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 2 (the default) and 8 (the maximum). Use this attribute together with the @readAheadBlocks@ attribute.
--
-- * 'osNumberDatatypeScale' - Specifies the number scale. You can select a scale up to 38, or you can select FLOAT. By default, the NUMBER data type is converted to precision 38, scale 10. Example: @numberDataTypeScale=12@
--
-- * 'osUsePathPrefix' - Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the path prefix used to replace the default Oracle root to access the redo logs.
--
-- * 'osAsmUser' - For an Oracle source endpoint, your ASM user name. You can set this value from the @asm_user@ value. You set @asm_user@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- * 'osUseAlternateFolderForOnline' - Set this attribute to @true@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to use any specified prefix replacement to access all online redo logs.
--
-- * 'osPort' - Endpoint TCP port.
oracleSettings ::
  OracleSettings
oracleSettings =
  OracleSettings'
    { _osFailTasksOnLobTruncation = Nothing,
      _osServerName = Nothing,
      _osDirectPathNoLog = Nothing,
      _osSecurityDBEncryptionName = Nothing,
      _osOraclePathPrefix = Nothing,
      _osUsername = Nothing,
      _osAllowSelectNestedTables = Nothing,
      _osReadAheadBlocks = Nothing,
      _osArchivedLogDestId = Nothing,
      _osReplacePathPrefix = Nothing,
      _osAccessAlternateDirectly = Nothing,
      _osSecurityDBEncryption = Nothing,
      _osReadTableSpaceName = Nothing,
      _osRetryInterval = Nothing,
      _osPassword = Nothing,
      _osDatabaseName = Nothing,
      _osAddSupplementalLogging = Nothing,
      _osAsmServer = Nothing,
      _osCharLengthSemantics = Nothing,
      _osArchivedLogsOnly = Nothing,
      _osDirectPathParallelLoad = Nothing,
      _osAdditionalArchivedLogDestId = Nothing,
      _osAsmPassword = Nothing,
      _osEnableHomogenousTablespace = Nothing,
      _osParallelAsmReadThreads = Nothing,
      _osNumberDatatypeScale = Nothing,
      _osUsePathPrefix = Nothing,
      _osAsmUser = Nothing,
      _osUseAlternateFolderForOnline = Nothing,
      _osPort = Nothing
    }

-- | When set to @true@ , this attribute causes a task to fail if the actual size of an LOB column is greater than the specified @LobMaxSize@ . If a task is set to limited LOB mode and this option is set to @true@ , the task fails instead of truncating the LOB data.
osFailTasksOnLobTruncation :: Lens' OracleSettings (Maybe Bool)
osFailTasksOnLobTruncation = lens _osFailTasksOnLobTruncation (\s a -> s {_osFailTasksOnLobTruncation = a})

-- | Fully qualified domain name of the endpoint.
osServerName :: Lens' OracleSettings (Maybe Text)
osServerName = lens _osServerName (\s a -> s {_osServerName = a})

-- | When set to @true@ , this attribute helps to increase the commit rate on the Oracle target database by writing directly to tables and not writing a trail to database logs.
osDirectPathNoLog :: Lens' OracleSettings (Maybe Bool)
osDirectPathNoLog = lens _osDirectPathNoLog (\s a -> s {_osDirectPathNoLog = a})

-- | For an Oracle source endpoint, the name of a key used for the transparent data encryption (TDE) of the columns and tablespaces in an Oracle source database that is encrypted using TDE. The key value is the value of the @SecurityDbEncryption@ setting. For more information on setting the key name value of @SecurityDbEncryptionName@ , see the information and example for setting the @securityDbEncryptionName@ extra connection attribute in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
osSecurityDBEncryptionName :: Lens' OracleSettings (Maybe Text)
osSecurityDBEncryptionName = lens _osSecurityDBEncryptionName (\s a -> s {_osSecurityDBEncryptionName = a})

-- | Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the default Oracle root used to access the redo logs.
osOraclePathPrefix :: Lens' OracleSettings (Maybe Text)
osOraclePathPrefix = lens _osOraclePathPrefix (\s a -> s {_osOraclePathPrefix = a})

-- | Endpoint connection user name.
osUsername :: Lens' OracleSettings (Maybe Text)
osUsername = lens _osUsername (\s a -> s {_osUsername = a})

-- | Set this attribute to @true@ to enable replication of Oracle tables containing columns that are nested tables or defined types.
osAllowSelectNestedTables :: Lens' OracleSettings (Maybe Bool)
osAllowSelectNestedTables = lens _osAllowSelectNestedTables (\s a -> s {_osAllowSelectNestedTables = a})

-- | Set this attribute to change the number of read-ahead blocks that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 1000 (the default) and 200,000 (the maximum).
osReadAheadBlocks :: Lens' OracleSettings (Maybe Int)
osReadAheadBlocks = lens _osReadAheadBlocks (\s a -> s {_osReadAheadBlocks = a})

-- | Specifies the destination of the archived redo logs. The value should be the same as the DEST_ID number in the v$archived_log table. When working with multiple log destinations (DEST_ID), we recommend that you to specify an archived redo logs location identifier. Doing this improves performance by ensuring that the correct logs are accessed from the outset.
osArchivedLogDestId :: Lens' OracleSettings (Maybe Int)
osArchivedLogDestId = lens _osArchivedLogDestId (\s a -> s {_osArchivedLogDestId = a})

-- | Set this attribute to true in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This setting tells DMS instance to replace the default Oracle root with the specified @usePathPrefix@ setting to access the redo logs.
osReplacePathPrefix :: Lens' OracleSettings (Maybe Bool)
osReplacePathPrefix = lens _osReplacePathPrefix (\s a -> s {_osReplacePathPrefix = a})

-- | Set this attribute to @false@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to not access redo logs through any specified path prefix replacement using direct file access.
osAccessAlternateDirectly :: Lens' OracleSettings (Maybe Bool)
osAccessAlternateDirectly = lens _osAccessAlternateDirectly (\s a -> s {_osAccessAlternateDirectly = a})

-- | For an Oracle source endpoint, the transparent data encryption (TDE) password required by AWM DMS to access Oracle redo logs encrypted by TDE using Binary Reader. It is also the @/TDE_Password/ @ part of the comma-separated value you set to the @Password@ request parameter when you create the endpoint. The @SecurityDbEncryptian@ setting is related to this @SecurityDbEncryptionName@ setting. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
osSecurityDBEncryption :: Lens' OracleSettings (Maybe Text)
osSecurityDBEncryption = lens _osSecurityDBEncryption (\s a -> s {_osSecurityDBEncryption = a}) . mapping _Sensitive

-- | When set to @true@ , this attribute supports tablespace replication.
osReadTableSpaceName :: Lens' OracleSettings (Maybe Bool)
osReadTableSpaceName = lens _osReadTableSpaceName (\s a -> s {_osReadTableSpaceName = a})

-- | Specifies the number of seconds that the system waits before resending a query. Example: @retryInterval=6;@
osRetryInterval :: Lens' OracleSettings (Maybe Int)
osRetryInterval = lens _osRetryInterval (\s a -> s {_osRetryInterval = a})

-- | Endpoint connection password.
osPassword :: Lens' OracleSettings (Maybe Text)
osPassword = lens _osPassword (\s a -> s {_osPassword = a}) . mapping _Sensitive

-- | Database name for the endpoint.
osDatabaseName :: Lens' OracleSettings (Maybe Text)
osDatabaseName = lens _osDatabaseName (\s a -> s {_osDatabaseName = a})

-- | Set this attribute to set up table-level supplemental logging for the Oracle database. This attribute enables PRIMARY KEY supplemental logging on all tables selected for a migration task. If you use this option, you still need to enable database-level supplemental logging.
osAddSupplementalLogging :: Lens' OracleSettings (Maybe Bool)
osAddSupplementalLogging = lens _osAddSupplementalLogging (\s a -> s {_osAddSupplementalLogging = a})

-- | For an Oracle source endpoint, your ASM server address. You can set this value from the @asm_server@ value. You set @asm_server@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
osAsmServer :: Lens' OracleSettings (Maybe Text)
osAsmServer = lens _osAsmServer (\s a -> s {_osAsmServer = a})

-- | Specifies whether the length of a character column is in bytes or in characters. To indicate that the character column length is in characters, set this attribute to @CHAR@ . Otherwise, the character column length is in bytes. Example: @charLengthSemantics=CHAR;@
osCharLengthSemantics :: Lens' OracleSettings (Maybe CharLengthSemantics)
osCharLengthSemantics = lens _osCharLengthSemantics (\s a -> s {_osCharLengthSemantics = a})

-- | When this field is set to @Y@ , AWS DMS only accesses the archived redo logs. If the archived redo logs are stored on Oracle ASM only, the AWS DMS user account needs to be granted ASM privileges.
osArchivedLogsOnly :: Lens' OracleSettings (Maybe Bool)
osArchivedLogsOnly = lens _osArchivedLogsOnly (\s a -> s {_osArchivedLogsOnly = a})

-- | When set to @true@ , this attribute specifies a parallel load when @useDirectPathFullLoad@ is set to @Y@ . This attribute also only applies when you use the AWS DMS parallel load feature. Note that the target table cannot have any constraints or indexes.
osDirectPathParallelLoad :: Lens' OracleSettings (Maybe Bool)
osDirectPathParallelLoad = lens _osDirectPathParallelLoad (\s a -> s {_osDirectPathParallelLoad = a})

-- | Set this attribute with @archivedLogDestId@ in a primary/ standby setup. This attribute is useful in the case of a switchover. In this case, AWS DMS needs to know which destination to get archive redo logs from to read changes. This need arises because the previous primary instance is now a standby instance after switchover.
osAdditionalArchivedLogDestId :: Lens' OracleSettings (Maybe Int)
osAdditionalArchivedLogDestId = lens _osAdditionalArchivedLogDestId (\s a -> s {_osAdditionalArchivedLogDestId = a})

-- | For an Oracle source endpoint, your Oracle Automatic Storage Management (ASM) password. You can set this value from the @/asm_user_password/ @ value. You set this value as part of the comma-separated value that you set to the @Password@ request parameter when you create the endpoint to access transaction logs using Binary Reader. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
osAsmPassword :: Lens' OracleSettings (Maybe Text)
osAsmPassword = lens _osAsmPassword (\s a -> s {_osAsmPassword = a}) . mapping _Sensitive

-- | Set this attribute to enable homogenous tablespace replication and create existing tables or indexes under the same tablespace on the target.
osEnableHomogenousTablespace :: Lens' OracleSettings (Maybe Bool)
osEnableHomogenousTablespace = lens _osEnableHomogenousTablespace (\s a -> s {_osEnableHomogenousTablespace = a})

-- | Set this attribute to change the number of threads that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 2 (the default) and 8 (the maximum). Use this attribute together with the @readAheadBlocks@ attribute.
osParallelAsmReadThreads :: Lens' OracleSettings (Maybe Int)
osParallelAsmReadThreads = lens _osParallelAsmReadThreads (\s a -> s {_osParallelAsmReadThreads = a})

-- | Specifies the number scale. You can select a scale up to 38, or you can select FLOAT. By default, the NUMBER data type is converted to precision 38, scale 10. Example: @numberDataTypeScale=12@
osNumberDatatypeScale :: Lens' OracleSettings (Maybe Int)
osNumberDatatypeScale = lens _osNumberDatatypeScale (\s a -> s {_osNumberDatatypeScale = a})

-- | Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the path prefix used to replace the default Oracle root to access the redo logs.
osUsePathPrefix :: Lens' OracleSettings (Maybe Text)
osUsePathPrefix = lens _osUsePathPrefix (\s a -> s {_osUsePathPrefix = a})

-- | For an Oracle source endpoint, your ASM user name. You can set this value from the @asm_user@ value. You set @asm_user@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
osAsmUser :: Lens' OracleSettings (Maybe Text)
osAsmUser = lens _osAsmUser (\s a -> s {_osAsmUser = a})

-- | Set this attribute to @true@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to use any specified prefix replacement to access all online redo logs.
osUseAlternateFolderForOnline :: Lens' OracleSettings (Maybe Bool)
osUseAlternateFolderForOnline = lens _osUseAlternateFolderForOnline (\s a -> s {_osUseAlternateFolderForOnline = a})

-- | Endpoint TCP port.
osPort :: Lens' OracleSettings (Maybe Int)
osPort = lens _osPort (\s a -> s {_osPort = a})

instance FromJSON OracleSettings where
  parseJSON =
    withObject
      "OracleSettings"
      ( \x ->
          OracleSettings'
            <$> (x .:? "FailTasksOnLobTruncation")
            <*> (x .:? "ServerName")
            <*> (x .:? "DirectPathNoLog")
            <*> (x .:? "SecurityDbEncryptionName")
            <*> (x .:? "OraclePathPrefix")
            <*> (x .:? "Username")
            <*> (x .:? "AllowSelectNestedTables")
            <*> (x .:? "ReadAheadBlocks")
            <*> (x .:? "ArchivedLogDestId")
            <*> (x .:? "ReplacePathPrefix")
            <*> (x .:? "AccessAlternateDirectly")
            <*> (x .:? "SecurityDbEncryption")
            <*> (x .:? "ReadTableSpaceName")
            <*> (x .:? "RetryInterval")
            <*> (x .:? "Password")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "AddSupplementalLogging")
            <*> (x .:? "AsmServer")
            <*> (x .:? "CharLengthSemantics")
            <*> (x .:? "ArchivedLogsOnly")
            <*> (x .:? "DirectPathParallelLoad")
            <*> (x .:? "AdditionalArchivedLogDestId")
            <*> (x .:? "AsmPassword")
            <*> (x .:? "EnableHomogenousTablespace")
            <*> (x .:? "ParallelAsmReadThreads")
            <*> (x .:? "NumberDatatypeScale")
            <*> (x .:? "UsePathPrefix")
            <*> (x .:? "AsmUser")
            <*> (x .:? "UseAlternateFolderForOnline")
            <*> (x .:? "Port")
      )

instance Hashable OracleSettings

instance NFData OracleSettings

instance ToJSON OracleSettings where
  toJSON OracleSettings' {..} =
    object
      ( catMaybes
          [ ("FailTasksOnLobTruncation" .=) <$> _osFailTasksOnLobTruncation,
            ("ServerName" .=) <$> _osServerName,
            ("DirectPathNoLog" .=) <$> _osDirectPathNoLog,
            ("SecurityDbEncryptionName" .=) <$> _osSecurityDBEncryptionName,
            ("OraclePathPrefix" .=) <$> _osOraclePathPrefix,
            ("Username" .=) <$> _osUsername,
            ("AllowSelectNestedTables" .=) <$> _osAllowSelectNestedTables,
            ("ReadAheadBlocks" .=) <$> _osReadAheadBlocks,
            ("ArchivedLogDestId" .=) <$> _osArchivedLogDestId,
            ("ReplacePathPrefix" .=) <$> _osReplacePathPrefix,
            ("AccessAlternateDirectly" .=) <$> _osAccessAlternateDirectly,
            ("SecurityDbEncryption" .=) <$> _osSecurityDBEncryption,
            ("ReadTableSpaceName" .=) <$> _osReadTableSpaceName,
            ("RetryInterval" .=) <$> _osRetryInterval,
            ("Password" .=) <$> _osPassword,
            ("DatabaseName" .=) <$> _osDatabaseName,
            ("AddSupplementalLogging" .=) <$> _osAddSupplementalLogging,
            ("AsmServer" .=) <$> _osAsmServer,
            ("CharLengthSemantics" .=) <$> _osCharLengthSemantics,
            ("ArchivedLogsOnly" .=) <$> _osArchivedLogsOnly,
            ("DirectPathParallelLoad" .=) <$> _osDirectPathParallelLoad,
            ("AdditionalArchivedLogDestId" .=)
              <$> _osAdditionalArchivedLogDestId,
            ("AsmPassword" .=) <$> _osAsmPassword,
            ("EnableHomogenousTablespace" .=)
              <$> _osEnableHomogenousTablespace,
            ("ParallelAsmReadThreads" .=) <$> _osParallelAsmReadThreads,
            ("NumberDatatypeScale" .=) <$> _osNumberDatatypeScale,
            ("UsePathPrefix" .=) <$> _osUsePathPrefix,
            ("AsmUser" .=) <$> _osAsmUser,
            ("UseAlternateFolderForOnline" .=)
              <$> _osUseAlternateFolderForOnline,
            ("Port" .=) <$> _osPort
          ]
      )

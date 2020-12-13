{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.OracleSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.OracleSettings
  ( OracleSettings (..),

    -- * Smart constructor
    mkOracleSettings,

    -- * Lenses
    osFailTasksOnLobTruncation,
    osServerName,
    osDirectPathNoLog,
    osSecurityDBEncryptionName,
    osOraclePathPrefix,
    osUsername,
    osAllowSelectNestedTables,
    osReadAheadBlocks,
    osArchivedLogDestId,
    osReplacePathPrefix,
    osAccessAlternateDirectly,
    osSecurityDBEncryption,
    osReadTableSpaceName,
    osRetryInterval,
    osPassword,
    osDatabaseName,
    osAddSupplementalLogging,
    osAsmServer,
    osCharLengthSemantics,
    osArchivedLogsOnly,
    osDirectPathParallelLoad,
    osAdditionalArchivedLogDestId,
    osAsmPassword,
    osEnableHomogenousTablespace,
    osParallelAsmReadThreads,
    osNumberDatatypeScale,
    osUsePathPrefix,
    osAsmUser,
    osUseAlternateFolderForOnline,
    osPort,
  )
where

import Network.AWS.DMS.Types.CharLengthSemantics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines an Oracle endpoint.
--
-- /See:/ 'mkOracleSettings' smart constructor.
data OracleSettings = OracleSettings'
  { -- | When set to @true@ , this attribute causes a task to fail if the actual size of an LOB column is greater than the specified @LobMaxSize@ .
    --
    -- If a task is set to limited LOB mode and this option is set to @true@ , the task fails instead of truncating the LOB data.
    failTasksOnLobTruncation :: Lude.Maybe Lude.Bool,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Lude.Maybe Lude.Text,
    -- | When set to @true@ , this attribute helps to increase the commit rate on the Oracle target database by writing directly to tables and not writing a trail to database logs.
    directPathNoLog :: Lude.Maybe Lude.Bool,
    -- | For an Oracle source endpoint, the name of a key used for the transparent data encryption (TDE) of the columns and tablespaces in an Oracle source database that is encrypted using TDE. The key value is the value of the @SecurityDbEncryption@ setting. For more information on setting the key name value of @SecurityDbEncryptionName@ , see the information and example for setting the @securityDbEncryptionName@ extra connection attribute in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
    securityDBEncryptionName :: Lude.Maybe Lude.Text,
    -- | Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the default Oracle root used to access the redo logs.
    oraclePathPrefix :: Lude.Maybe Lude.Text,
    -- | Endpoint connection user name.
    username :: Lude.Maybe Lude.Text,
    -- | Set this attribute to @true@ to enable replication of Oracle tables containing columns that are nested tables or defined types.
    allowSelectNestedTables :: Lude.Maybe Lude.Bool,
    -- | Set this attribute to change the number of read-ahead blocks that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 1000 (the default) and 200,000 (the maximum).
    readAheadBlocks :: Lude.Maybe Lude.Int,
    -- | Specifies the destination of the archived redo logs. The value should be the same as the DEST_ID number in the v$archived_log table. When working with multiple log destinations (DEST_ID), we recommend that you to specify an archived redo logs location identifier. Doing this improves performance by ensuring that the correct logs are accessed from the outset.
    archivedLogDestId :: Lude.Maybe Lude.Int,
    -- | Set this attribute to true in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This setting tells DMS instance to replace the default Oracle root with the specified @usePathPrefix@ setting to access the redo logs.
    replacePathPrefix :: Lude.Maybe Lude.Bool,
    -- | Set this attribute to @false@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to not access redo logs through any specified path prefix replacement using direct file access.
    accessAlternateDirectly :: Lude.Maybe Lude.Bool,
    -- | For an Oracle source endpoint, the transparent data encryption (TDE) password required by AWM DMS to access Oracle redo logs encrypted by TDE using Binary Reader. It is also the @/TDE_Password/ @ part of the comma-separated value you set to the @Password@ request parameter when you create the endpoint. The @SecurityDbEncryptian@ setting is related to this @SecurityDbEncryptionName@ setting. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
    securityDBEncryption :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | When set to @true@ , this attribute supports tablespace replication.
    readTableSpaceName :: Lude.Maybe Lude.Bool,
    -- | Specifies the number of seconds that the system waits before resending a query.
    --
    -- Example: @retryInterval=6;@
    retryInterval :: Lude.Maybe Lude.Int,
    -- | Endpoint connection password.
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Database name for the endpoint.
    databaseName :: Lude.Maybe Lude.Text,
    -- | Set this attribute to set up table-level supplemental logging for the Oracle database. This attribute enables PRIMARY KEY supplemental logging on all tables selected for a migration task.
    --
    -- If you use this option, you still need to enable database-level supplemental logging.
    addSupplementalLogging :: Lude.Maybe Lude.Bool,
    -- | For an Oracle source endpoint, your ASM server address. You can set this value from the @asm_server@ value. You set @asm_server@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
    asmServer :: Lude.Maybe Lude.Text,
    -- | Specifies whether the length of a character column is in bytes or in characters. To indicate that the character column length is in characters, set this attribute to @CHAR@ . Otherwise, the character column length is in bytes.
    --
    -- Example: @charLengthSemantics=CHAR;@
    charLengthSemantics :: Lude.Maybe CharLengthSemantics,
    -- | When this field is set to @Y@ , AWS DMS only accesses the archived redo logs. If the archived redo logs are stored on Oracle ASM only, the AWS DMS user account needs to be granted ASM privileges.
    archivedLogsOnly :: Lude.Maybe Lude.Bool,
    -- | When set to @true@ , this attribute specifies a parallel load when @useDirectPathFullLoad@ is set to @Y@ . This attribute also only applies when you use the AWS DMS parallel load feature. Note that the target table cannot have any constraints or indexes.
    directPathParallelLoad :: Lude.Maybe Lude.Bool,
    -- | Set this attribute with @archivedLogDestId@ in a primary/ standby setup. This attribute is useful in the case of a switchover. In this case, AWS DMS needs to know which destination to get archive redo logs from to read changes. This need arises because the previous primary instance is now a standby instance after switchover.
    additionalArchivedLogDestId :: Lude.Maybe Lude.Int,
    -- | For an Oracle source endpoint, your Oracle Automatic Storage Management (ASM) password. You can set this value from the @/asm_user_password/ @ value. You set this value as part of the comma-separated value that you set to the @Password@ request parameter when you create the endpoint to access transaction logs using Binary Reader. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
    asmPassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Set this attribute to enable homogenous tablespace replication and create existing tables or indexes under the same tablespace on the target.
    enableHomogenousTablespace :: Lude.Maybe Lude.Bool,
    -- | Set this attribute to change the number of threads that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 2 (the default) and 8 (the maximum). Use this attribute together with the @readAheadBlocks@ attribute.
    parallelAsmReadThreads :: Lude.Maybe Lude.Int,
    -- | Specifies the number scale. You can select a scale up to 38, or you can select FLOAT. By default, the NUMBER data type is converted to precision 38, scale 10.
    --
    -- Example: @numberDataTypeScale=12@
    numberDatatypeScale :: Lude.Maybe Lude.Int,
    -- | Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the path prefix used to replace the default Oracle root to access the redo logs.
    usePathPrefix :: Lude.Maybe Lude.Text,
    -- | For an Oracle source endpoint, your ASM user name. You can set this value from the @asm_user@ value. You set @asm_user@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
    asmUser :: Lude.Maybe Lude.Text,
    -- | Set this attribute to @true@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to use any specified prefix replacement to access all online redo logs.
    useAlternateFolderForOnline :: Lude.Maybe Lude.Bool,
    -- | Endpoint TCP port.
    port :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OracleSettings' with the minimum fields required to make a request.
--
-- * 'failTasksOnLobTruncation' - When set to @true@ , this attribute causes a task to fail if the actual size of an LOB column is greater than the specified @LobMaxSize@ .
--
-- If a task is set to limited LOB mode and this option is set to @true@ , the task fails instead of truncating the LOB data.
-- * 'serverName' - Fully qualified domain name of the endpoint.
-- * 'directPathNoLog' - When set to @true@ , this attribute helps to increase the commit rate on the Oracle target database by writing directly to tables and not writing a trail to database logs.
-- * 'securityDBEncryptionName' - For an Oracle source endpoint, the name of a key used for the transparent data encryption (TDE) of the columns and tablespaces in an Oracle source database that is encrypted using TDE. The key value is the value of the @SecurityDbEncryption@ setting. For more information on setting the key name value of @SecurityDbEncryptionName@ , see the information and example for setting the @securityDbEncryptionName@ extra connection attribute in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
-- * 'oraclePathPrefix' - Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the default Oracle root used to access the redo logs.
-- * 'username' - Endpoint connection user name.
-- * 'allowSelectNestedTables' - Set this attribute to @true@ to enable replication of Oracle tables containing columns that are nested tables or defined types.
-- * 'readAheadBlocks' - Set this attribute to change the number of read-ahead blocks that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 1000 (the default) and 200,000 (the maximum).
-- * 'archivedLogDestId' - Specifies the destination of the archived redo logs. The value should be the same as the DEST_ID number in the v$archived_log table. When working with multiple log destinations (DEST_ID), we recommend that you to specify an archived redo logs location identifier. Doing this improves performance by ensuring that the correct logs are accessed from the outset.
-- * 'replacePathPrefix' - Set this attribute to true in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This setting tells DMS instance to replace the default Oracle root with the specified @usePathPrefix@ setting to access the redo logs.
-- * 'accessAlternateDirectly' - Set this attribute to @false@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to not access redo logs through any specified path prefix replacement using direct file access.
-- * 'securityDBEncryption' - For an Oracle source endpoint, the transparent data encryption (TDE) password required by AWM DMS to access Oracle redo logs encrypted by TDE using Binary Reader. It is also the @/TDE_Password/ @ part of the comma-separated value you set to the @Password@ request parameter when you create the endpoint. The @SecurityDbEncryptian@ setting is related to this @SecurityDbEncryptionName@ setting. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
-- * 'readTableSpaceName' - When set to @true@ , this attribute supports tablespace replication.
-- * 'retryInterval' - Specifies the number of seconds that the system waits before resending a query.
--
-- Example: @retryInterval=6;@
-- * 'password' - Endpoint connection password.
-- * 'databaseName' - Database name for the endpoint.
-- * 'addSupplementalLogging' - Set this attribute to set up table-level supplemental logging for the Oracle database. This attribute enables PRIMARY KEY supplemental logging on all tables selected for a migration task.
--
-- If you use this option, you still need to enable database-level supplemental logging.
-- * 'asmServer' - For an Oracle source endpoint, your ASM server address. You can set this value from the @asm_server@ value. You set @asm_server@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
-- * 'charLengthSemantics' - Specifies whether the length of a character column is in bytes or in characters. To indicate that the character column length is in characters, set this attribute to @CHAR@ . Otherwise, the character column length is in bytes.
--
-- Example: @charLengthSemantics=CHAR;@
-- * 'archivedLogsOnly' - When this field is set to @Y@ , AWS DMS only accesses the archived redo logs. If the archived redo logs are stored on Oracle ASM only, the AWS DMS user account needs to be granted ASM privileges.
-- * 'directPathParallelLoad' - When set to @true@ , this attribute specifies a parallel load when @useDirectPathFullLoad@ is set to @Y@ . This attribute also only applies when you use the AWS DMS parallel load feature. Note that the target table cannot have any constraints or indexes.
-- * 'additionalArchivedLogDestId' - Set this attribute with @archivedLogDestId@ in a primary/ standby setup. This attribute is useful in the case of a switchover. In this case, AWS DMS needs to know which destination to get archive redo logs from to read changes. This need arises because the previous primary instance is now a standby instance after switchover.
-- * 'asmPassword' - For an Oracle source endpoint, your Oracle Automatic Storage Management (ASM) password. You can set this value from the @/asm_user_password/ @ value. You set this value as part of the comma-separated value that you set to the @Password@ request parameter when you create the endpoint to access transaction logs using Binary Reader. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
-- * 'enableHomogenousTablespace' - Set this attribute to enable homogenous tablespace replication and create existing tables or indexes under the same tablespace on the target.
-- * 'parallelAsmReadThreads' - Set this attribute to change the number of threads that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 2 (the default) and 8 (the maximum). Use this attribute together with the @readAheadBlocks@ attribute.
-- * 'numberDatatypeScale' - Specifies the number scale. You can select a scale up to 38, or you can select FLOAT. By default, the NUMBER data type is converted to precision 38, scale 10.
--
-- Example: @numberDataTypeScale=12@
-- * 'usePathPrefix' - Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the path prefix used to replace the default Oracle root to access the redo logs.
-- * 'asmUser' - For an Oracle source endpoint, your ASM user name. You can set this value from the @asm_user@ value. You set @asm_user@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
-- * 'useAlternateFolderForOnline' - Set this attribute to @true@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to use any specified prefix replacement to access all online redo logs.
-- * 'port' - Endpoint TCP port.
mkOracleSettings ::
  OracleSettings
mkOracleSettings =
  OracleSettings'
    { failTasksOnLobTruncation = Lude.Nothing,
      serverName = Lude.Nothing,
      directPathNoLog = Lude.Nothing,
      securityDBEncryptionName = Lude.Nothing,
      oraclePathPrefix = Lude.Nothing,
      username = Lude.Nothing,
      allowSelectNestedTables = Lude.Nothing,
      readAheadBlocks = Lude.Nothing,
      archivedLogDestId = Lude.Nothing,
      replacePathPrefix = Lude.Nothing,
      accessAlternateDirectly = Lude.Nothing,
      securityDBEncryption = Lude.Nothing,
      readTableSpaceName = Lude.Nothing,
      retryInterval = Lude.Nothing,
      password = Lude.Nothing,
      databaseName = Lude.Nothing,
      addSupplementalLogging = Lude.Nothing,
      asmServer = Lude.Nothing,
      charLengthSemantics = Lude.Nothing,
      archivedLogsOnly = Lude.Nothing,
      directPathParallelLoad = Lude.Nothing,
      additionalArchivedLogDestId = Lude.Nothing,
      asmPassword = Lude.Nothing,
      enableHomogenousTablespace = Lude.Nothing,
      parallelAsmReadThreads = Lude.Nothing,
      numberDatatypeScale = Lude.Nothing,
      usePathPrefix = Lude.Nothing,
      asmUser = Lude.Nothing,
      useAlternateFolderForOnline = Lude.Nothing,
      port = Lude.Nothing
    }

-- | When set to @true@ , this attribute causes a task to fail if the actual size of an LOB column is greater than the specified @LobMaxSize@ .
--
-- If a task is set to limited LOB mode and this option is set to @true@ , the task fails instead of truncating the LOB data.
--
-- /Note:/ Consider using 'failTasksOnLobTruncation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osFailTasksOnLobTruncation :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osFailTasksOnLobTruncation = Lens.lens (failTasksOnLobTruncation :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {failTasksOnLobTruncation = a} :: OracleSettings)
{-# DEPRECATED osFailTasksOnLobTruncation "Use generic-lens or generic-optics with 'failTasksOnLobTruncation' instead." #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osServerName :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Text)
osServerName = Lens.lens (serverName :: OracleSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: OracleSettings)
{-# DEPRECATED osServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | When set to @true@ , this attribute helps to increase the commit rate on the Oracle target database by writing directly to tables and not writing a trail to database logs.
--
-- /Note:/ Consider using 'directPathNoLog' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDirectPathNoLog :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osDirectPathNoLog = Lens.lens (directPathNoLog :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {directPathNoLog = a} :: OracleSettings)
{-# DEPRECATED osDirectPathNoLog "Use generic-lens or generic-optics with 'directPathNoLog' instead." #-}

-- | For an Oracle source endpoint, the name of a key used for the transparent data encryption (TDE) of the columns and tablespaces in an Oracle source database that is encrypted using TDE. The key value is the value of the @SecurityDbEncryption@ setting. For more information on setting the key name value of @SecurityDbEncryptionName@ , see the information and example for setting the @securityDbEncryptionName@ extra connection attribute in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- /Note:/ Consider using 'securityDBEncryptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSecurityDBEncryptionName :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Text)
osSecurityDBEncryptionName = Lens.lens (securityDBEncryptionName :: OracleSettings -> Lude.Maybe Lude.Text) (\s a -> s {securityDBEncryptionName = a} :: OracleSettings)
{-# DEPRECATED osSecurityDBEncryptionName "Use generic-lens or generic-optics with 'securityDBEncryptionName' instead." #-}

-- | Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the default Oracle root used to access the redo logs.
--
-- /Note:/ Consider using 'oraclePathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOraclePathPrefix :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Text)
osOraclePathPrefix = Lens.lens (oraclePathPrefix :: OracleSettings -> Lude.Maybe Lude.Text) (\s a -> s {oraclePathPrefix = a} :: OracleSettings)
{-# DEPRECATED osOraclePathPrefix "Use generic-lens or generic-optics with 'oraclePathPrefix' instead." #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUsername :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Text)
osUsername = Lens.lens (username :: OracleSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: OracleSettings)
{-# DEPRECATED osUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Set this attribute to @true@ to enable replication of Oracle tables containing columns that are nested tables or defined types.
--
-- /Note:/ Consider using 'allowSelectNestedTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAllowSelectNestedTables :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osAllowSelectNestedTables = Lens.lens (allowSelectNestedTables :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {allowSelectNestedTables = a} :: OracleSettings)
{-# DEPRECATED osAllowSelectNestedTables "Use generic-lens or generic-optics with 'allowSelectNestedTables' instead." #-}

-- | Set this attribute to change the number of read-ahead blocks that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 1000 (the default) and 200,000 (the maximum).
--
-- /Note:/ Consider using 'readAheadBlocks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReadAheadBlocks :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Int)
osReadAheadBlocks = Lens.lens (readAheadBlocks :: OracleSettings -> Lude.Maybe Lude.Int) (\s a -> s {readAheadBlocks = a} :: OracleSettings)
{-# DEPRECATED osReadAheadBlocks "Use generic-lens or generic-optics with 'readAheadBlocks' instead." #-}

-- | Specifies the destination of the archived redo logs. The value should be the same as the DEST_ID number in the v$archived_log table. When working with multiple log destinations (DEST_ID), we recommend that you to specify an archived redo logs location identifier. Doing this improves performance by ensuring that the correct logs are accessed from the outset.
--
-- /Note:/ Consider using 'archivedLogDestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osArchivedLogDestId :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Int)
osArchivedLogDestId = Lens.lens (archivedLogDestId :: OracleSettings -> Lude.Maybe Lude.Int) (\s a -> s {archivedLogDestId = a} :: OracleSettings)
{-# DEPRECATED osArchivedLogDestId "Use generic-lens or generic-optics with 'archivedLogDestId' instead." #-}

-- | Set this attribute to true in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This setting tells DMS instance to replace the default Oracle root with the specified @usePathPrefix@ setting to access the redo logs.
--
-- /Note:/ Consider using 'replacePathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReplacePathPrefix :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osReplacePathPrefix = Lens.lens (replacePathPrefix :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {replacePathPrefix = a} :: OracleSettings)
{-# DEPRECATED osReplacePathPrefix "Use generic-lens or generic-optics with 'replacePathPrefix' instead." #-}

-- | Set this attribute to @false@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to not access redo logs through any specified path prefix replacement using direct file access.
--
-- /Note:/ Consider using 'accessAlternateDirectly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAccessAlternateDirectly :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osAccessAlternateDirectly = Lens.lens (accessAlternateDirectly :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {accessAlternateDirectly = a} :: OracleSettings)
{-# DEPRECATED osAccessAlternateDirectly "Use generic-lens or generic-optics with 'accessAlternateDirectly' instead." #-}

-- | For an Oracle source endpoint, the transparent data encryption (TDE) password required by AWM DMS to access Oracle redo logs encrypted by TDE using Binary Reader. It is also the @/TDE_Password/ @ part of the comma-separated value you set to the @Password@ request parameter when you create the endpoint. The @SecurityDbEncryptian@ setting is related to this @SecurityDbEncryptionName@ setting. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- /Note:/ Consider using 'securityDBEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSecurityDBEncryption :: Lens.Lens' OracleSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
osSecurityDBEncryption = Lens.lens (securityDBEncryption :: OracleSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {securityDBEncryption = a} :: OracleSettings)
{-# DEPRECATED osSecurityDBEncryption "Use generic-lens or generic-optics with 'securityDBEncryption' instead." #-}

-- | When set to @true@ , this attribute supports tablespace replication.
--
-- /Note:/ Consider using 'readTableSpaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReadTableSpaceName :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osReadTableSpaceName = Lens.lens (readTableSpaceName :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {readTableSpaceName = a} :: OracleSettings)
{-# DEPRECATED osReadTableSpaceName "Use generic-lens or generic-optics with 'readTableSpaceName' instead." #-}

-- | Specifies the number of seconds that the system waits before resending a query.
--
-- Example: @retryInterval=6;@
--
-- /Note:/ Consider using 'retryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osRetryInterval :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Int)
osRetryInterval = Lens.lens (retryInterval :: OracleSettings -> Lude.Maybe Lude.Int) (\s a -> s {retryInterval = a} :: OracleSettings)
{-# DEPRECATED osRetryInterval "Use generic-lens or generic-optics with 'retryInterval' instead." #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPassword :: Lens.Lens' OracleSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
osPassword = Lens.lens (password :: OracleSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: OracleSettings)
{-# DEPRECATED osPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDatabaseName :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Text)
osDatabaseName = Lens.lens (databaseName :: OracleSettings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: OracleSettings)
{-# DEPRECATED osDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Set this attribute to set up table-level supplemental logging for the Oracle database. This attribute enables PRIMARY KEY supplemental logging on all tables selected for a migration task.
--
-- If you use this option, you still need to enable database-level supplemental logging.
--
-- /Note:/ Consider using 'addSupplementalLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAddSupplementalLogging :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osAddSupplementalLogging = Lens.lens (addSupplementalLogging :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {addSupplementalLogging = a} :: OracleSettings)
{-# DEPRECATED osAddSupplementalLogging "Use generic-lens or generic-optics with 'addSupplementalLogging' instead." #-}

-- | For an Oracle source endpoint, your ASM server address. You can set this value from the @asm_server@ value. You set @asm_server@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- /Note:/ Consider using 'asmServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAsmServer :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Text)
osAsmServer = Lens.lens (asmServer :: OracleSettings -> Lude.Maybe Lude.Text) (\s a -> s {asmServer = a} :: OracleSettings)
{-# DEPRECATED osAsmServer "Use generic-lens or generic-optics with 'asmServer' instead." #-}

-- | Specifies whether the length of a character column is in bytes or in characters. To indicate that the character column length is in characters, set this attribute to @CHAR@ . Otherwise, the character column length is in bytes.
--
-- Example: @charLengthSemantics=CHAR;@
--
-- /Note:/ Consider using 'charLengthSemantics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCharLengthSemantics :: Lens.Lens' OracleSettings (Lude.Maybe CharLengthSemantics)
osCharLengthSemantics = Lens.lens (charLengthSemantics :: OracleSettings -> Lude.Maybe CharLengthSemantics) (\s a -> s {charLengthSemantics = a} :: OracleSettings)
{-# DEPRECATED osCharLengthSemantics "Use generic-lens or generic-optics with 'charLengthSemantics' instead." #-}

-- | When this field is set to @Y@ , AWS DMS only accesses the archived redo logs. If the archived redo logs are stored on Oracle ASM only, the AWS DMS user account needs to be granted ASM privileges.
--
-- /Note:/ Consider using 'archivedLogsOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osArchivedLogsOnly :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osArchivedLogsOnly = Lens.lens (archivedLogsOnly :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {archivedLogsOnly = a} :: OracleSettings)
{-# DEPRECATED osArchivedLogsOnly "Use generic-lens or generic-optics with 'archivedLogsOnly' instead." #-}

-- | When set to @true@ , this attribute specifies a parallel load when @useDirectPathFullLoad@ is set to @Y@ . This attribute also only applies when you use the AWS DMS parallel load feature. Note that the target table cannot have any constraints or indexes.
--
-- /Note:/ Consider using 'directPathParallelLoad' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDirectPathParallelLoad :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osDirectPathParallelLoad = Lens.lens (directPathParallelLoad :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {directPathParallelLoad = a} :: OracleSettings)
{-# DEPRECATED osDirectPathParallelLoad "Use generic-lens or generic-optics with 'directPathParallelLoad' instead." #-}

-- | Set this attribute with @archivedLogDestId@ in a primary/ standby setup. This attribute is useful in the case of a switchover. In this case, AWS DMS needs to know which destination to get archive redo logs from to read changes. This need arises because the previous primary instance is now a standby instance after switchover.
--
-- /Note:/ Consider using 'additionalArchivedLogDestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAdditionalArchivedLogDestId :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Int)
osAdditionalArchivedLogDestId = Lens.lens (additionalArchivedLogDestId :: OracleSettings -> Lude.Maybe Lude.Int) (\s a -> s {additionalArchivedLogDestId = a} :: OracleSettings)
{-# DEPRECATED osAdditionalArchivedLogDestId "Use generic-lens or generic-optics with 'additionalArchivedLogDestId' instead." #-}

-- | For an Oracle source endpoint, your Oracle Automatic Storage Management (ASM) password. You can set this value from the @/asm_user_password/ @ value. You set this value as part of the comma-separated value that you set to the @Password@ request parameter when you create the endpoint to access transaction logs using Binary Reader. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- /Note:/ Consider using 'asmPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAsmPassword :: Lens.Lens' OracleSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
osAsmPassword = Lens.lens (asmPassword :: OracleSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {asmPassword = a} :: OracleSettings)
{-# DEPRECATED osAsmPassword "Use generic-lens or generic-optics with 'asmPassword' instead." #-}

-- | Set this attribute to enable homogenous tablespace replication and create existing tables or indexes under the same tablespace on the target.
--
-- /Note:/ Consider using 'enableHomogenousTablespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osEnableHomogenousTablespace :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osEnableHomogenousTablespace = Lens.lens (enableHomogenousTablespace :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {enableHomogenousTablespace = a} :: OracleSettings)
{-# DEPRECATED osEnableHomogenousTablespace "Use generic-lens or generic-optics with 'enableHomogenousTablespace' instead." #-}

-- | Set this attribute to change the number of threads that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 2 (the default) and 8 (the maximum). Use this attribute together with the @readAheadBlocks@ attribute.
--
-- /Note:/ Consider using 'parallelAsmReadThreads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osParallelAsmReadThreads :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Int)
osParallelAsmReadThreads = Lens.lens (parallelAsmReadThreads :: OracleSettings -> Lude.Maybe Lude.Int) (\s a -> s {parallelAsmReadThreads = a} :: OracleSettings)
{-# DEPRECATED osParallelAsmReadThreads "Use generic-lens or generic-optics with 'parallelAsmReadThreads' instead." #-}

-- | Specifies the number scale. You can select a scale up to 38, or you can select FLOAT. By default, the NUMBER data type is converted to precision 38, scale 10.
--
-- Example: @numberDataTypeScale=12@
--
-- /Note:/ Consider using 'numberDatatypeScale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osNumberDatatypeScale :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Int)
osNumberDatatypeScale = Lens.lens (numberDatatypeScale :: OracleSettings -> Lude.Maybe Lude.Int) (\s a -> s {numberDatatypeScale = a} :: OracleSettings)
{-# DEPRECATED osNumberDatatypeScale "Use generic-lens or generic-optics with 'numberDatatypeScale' instead." #-}

-- | Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the path prefix used to replace the default Oracle root to access the redo logs.
--
-- /Note:/ Consider using 'usePathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUsePathPrefix :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Text)
osUsePathPrefix = Lens.lens (usePathPrefix :: OracleSettings -> Lude.Maybe Lude.Text) (\s a -> s {usePathPrefix = a} :: OracleSettings)
{-# DEPRECATED osUsePathPrefix "Use generic-lens or generic-optics with 'usePathPrefix' instead." #-}

-- | For an Oracle source endpoint, your ASM user name. You can set this value from the @asm_user@ value. You set @asm_user@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- /Note:/ Consider using 'asmUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAsmUser :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Text)
osAsmUser = Lens.lens (asmUser :: OracleSettings -> Lude.Maybe Lude.Text) (\s a -> s {asmUser = a} :: OracleSettings)
{-# DEPRECATED osAsmUser "Use generic-lens or generic-optics with 'asmUser' instead." #-}

-- | Set this attribute to @true@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to use any specified prefix replacement to access all online redo logs.
--
-- /Note:/ Consider using 'useAlternateFolderForOnline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUseAlternateFolderForOnline :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Bool)
osUseAlternateFolderForOnline = Lens.lens (useAlternateFolderForOnline :: OracleSettings -> Lude.Maybe Lude.Bool) (\s a -> s {useAlternateFolderForOnline = a} :: OracleSettings)
{-# DEPRECATED osUseAlternateFolderForOnline "Use generic-lens or generic-optics with 'useAlternateFolderForOnline' instead." #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPort :: Lens.Lens' OracleSettings (Lude.Maybe Lude.Int)
osPort = Lens.lens (port :: OracleSettings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: OracleSettings)
{-# DEPRECATED osPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON OracleSettings where
  parseJSON =
    Lude.withObject
      "OracleSettings"
      ( \x ->
          OracleSettings'
            Lude.<$> (x Lude..:? "FailTasksOnLobTruncation")
            Lude.<*> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "DirectPathNoLog")
            Lude.<*> (x Lude..:? "SecurityDbEncryptionName")
            Lude.<*> (x Lude..:? "OraclePathPrefix")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "AllowSelectNestedTables")
            Lude.<*> (x Lude..:? "ReadAheadBlocks")
            Lude.<*> (x Lude..:? "ArchivedLogDestId")
            Lude.<*> (x Lude..:? "ReplacePathPrefix")
            Lude.<*> (x Lude..:? "AccessAlternateDirectly")
            Lude.<*> (x Lude..:? "SecurityDbEncryption")
            Lude.<*> (x Lude..:? "ReadTableSpaceName")
            Lude.<*> (x Lude..:? "RetryInterval")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "AddSupplementalLogging")
            Lude.<*> (x Lude..:? "AsmServer")
            Lude.<*> (x Lude..:? "CharLengthSemantics")
            Lude.<*> (x Lude..:? "ArchivedLogsOnly")
            Lude.<*> (x Lude..:? "DirectPathParallelLoad")
            Lude.<*> (x Lude..:? "AdditionalArchivedLogDestId")
            Lude.<*> (x Lude..:? "AsmPassword")
            Lude.<*> (x Lude..:? "EnableHomogenousTablespace")
            Lude.<*> (x Lude..:? "ParallelAsmReadThreads")
            Lude.<*> (x Lude..:? "NumberDatatypeScale")
            Lude.<*> (x Lude..:? "UsePathPrefix")
            Lude.<*> (x Lude..:? "AsmUser")
            Lude.<*> (x Lude..:? "UseAlternateFolderForOnline")
            Lude.<*> (x Lude..:? "Port")
      )

instance Lude.ToJSON OracleSettings where
  toJSON OracleSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FailTasksOnLobTruncation" Lude..=)
              Lude.<$> failTasksOnLobTruncation,
            ("ServerName" Lude..=) Lude.<$> serverName,
            ("DirectPathNoLog" Lude..=) Lude.<$> directPathNoLog,
            ("SecurityDbEncryptionName" Lude..=)
              Lude.<$> securityDBEncryptionName,
            ("OraclePathPrefix" Lude..=) Lude.<$> oraclePathPrefix,
            ("Username" Lude..=) Lude.<$> username,
            ("AllowSelectNestedTables" Lude..=)
              Lude.<$> allowSelectNestedTables,
            ("ReadAheadBlocks" Lude..=) Lude.<$> readAheadBlocks,
            ("ArchivedLogDestId" Lude..=) Lude.<$> archivedLogDestId,
            ("ReplacePathPrefix" Lude..=) Lude.<$> replacePathPrefix,
            ("AccessAlternateDirectly" Lude..=)
              Lude.<$> accessAlternateDirectly,
            ("SecurityDbEncryption" Lude..=) Lude.<$> securityDBEncryption,
            ("ReadTableSpaceName" Lude..=) Lude.<$> readTableSpaceName,
            ("RetryInterval" Lude..=) Lude.<$> retryInterval,
            ("Password" Lude..=) Lude.<$> password,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("AddSupplementalLogging" Lude..=) Lude.<$> addSupplementalLogging,
            ("AsmServer" Lude..=) Lude.<$> asmServer,
            ("CharLengthSemantics" Lude..=) Lude.<$> charLengthSemantics,
            ("ArchivedLogsOnly" Lude..=) Lude.<$> archivedLogsOnly,
            ("DirectPathParallelLoad" Lude..=) Lude.<$> directPathParallelLoad,
            ("AdditionalArchivedLogDestId" Lude..=)
              Lude.<$> additionalArchivedLogDestId,
            ("AsmPassword" Lude..=) Lude.<$> asmPassword,
            ("EnableHomogenousTablespace" Lude..=)
              Lude.<$> enableHomogenousTablespace,
            ("ParallelAsmReadThreads" Lude..=) Lude.<$> parallelAsmReadThreads,
            ("NumberDatatypeScale" Lude..=) Lude.<$> numberDatatypeScale,
            ("UsePathPrefix" Lude..=) Lude.<$> usePathPrefix,
            ("AsmUser" Lude..=) Lude.<$> asmUser,
            ("UseAlternateFolderForOnline" Lude..=)
              Lude.<$> useAlternateFolderForOnline,
            ("Port" Lude..=) Lude.<$> port
          ]
      )

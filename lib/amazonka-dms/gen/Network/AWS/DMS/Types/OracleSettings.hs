{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.OracleSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.OracleSettings
  ( OracleSettings (..)
  -- * Smart constructor
  , mkOracleSettings
  -- * Lenses
  , osAccessAlternateDirectly
  , osAddSupplementalLogging
  , osAdditionalArchivedLogDestId
  , osAllowSelectNestedTables
  , osArchivedLogDestId
  , osArchivedLogsOnly
  , osAsmPassword
  , osAsmServer
  , osAsmUser
  , osCharLengthSemantics
  , osDatabaseName
  , osDirectPathNoLog
  , osDirectPathParallelLoad
  , osEnableHomogenousTablespace
  , osFailTasksOnLobTruncation
  , osNumberDatatypeScale
  , osOraclePathPrefix
  , osParallelAsmReadThreads
  , osPassword
  , osPort
  , osReadAheadBlocks
  , osReadTableSpaceName
  , osReplacePathPrefix
  , osRetryInterval
  , osSecurityDbEncryption
  , osSecurityDbEncryptionName
  , osServerName
  , osUseAlternateFolderForOnline
  , osUsePathPrefix
  , osUsername
  ) where

import qualified Network.AWS.DMS.Types.AsmPassword as Types
import qualified Network.AWS.DMS.Types.CharLengthSemantics as Types
import qualified Network.AWS.DMS.Types.Password as Types
import qualified Network.AWS.DMS.Types.SecurityDbEncryption as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that defines an Oracle endpoint.
--
-- /See:/ 'mkOracleSettings' smart constructor.
data OracleSettings = OracleSettings'
  { accessAlternateDirectly :: Core.Maybe Core.Bool
    -- ^ Set this attribute to @false@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to not access redo logs through any specified path prefix replacement using direct file access.
  , addSupplementalLogging :: Core.Maybe Core.Bool
    -- ^ Set this attribute to set up table-level supplemental logging for the Oracle database. This attribute enables PRIMARY KEY supplemental logging on all tables selected for a migration task.
--
-- If you use this option, you still need to enable database-level supplemental logging.
  , additionalArchivedLogDestId :: Core.Maybe Core.Int
    -- ^ Set this attribute with @archivedLogDestId@ in a primary/ standby setup. This attribute is useful in the case of a switchover. In this case, AWS DMS needs to know which destination to get archive redo logs from to read changes. This need arises because the previous primary instance is now a standby instance after switchover.
  , allowSelectNestedTables :: Core.Maybe Core.Bool
    -- ^ Set this attribute to @true@ to enable replication of Oracle tables containing columns that are nested tables or defined types.
  , archivedLogDestId :: Core.Maybe Core.Int
    -- ^ Specifies the destination of the archived redo logs. The value should be the same as the DEST_ID number in the v$archived_log table. When working with multiple log destinations (DEST_ID), we recommend that you to specify an archived redo logs location identifier. Doing this improves performance by ensuring that the correct logs are accessed from the outset.
  , archivedLogsOnly :: Core.Maybe Core.Bool
    -- ^ When this field is set to @Y@ , AWS DMS only accesses the archived redo logs. If the archived redo logs are stored on Oracle ASM only, the AWS DMS user account needs to be granted ASM privileges.
  , asmPassword :: Core.Maybe Types.AsmPassword
    -- ^ For an Oracle source endpoint, your Oracle Automatic Storage Management (ASM) password. You can set this value from the @/asm_user_password/ @ value. You set this value as part of the comma-separated value that you set to the @Password@ request parameter when you create the endpoint to access transaction logs using Binary Reader. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
  , asmServer :: Core.Maybe Core.Text
    -- ^ For an Oracle source endpoint, your ASM server address. You can set this value from the @asm_server@ value. You set @asm_server@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
  , asmUser :: Core.Maybe Core.Text
    -- ^ For an Oracle source endpoint, your ASM user name. You can set this value from the @asm_user@ value. You set @asm_user@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
  , charLengthSemantics :: Core.Maybe Types.CharLengthSemantics
    -- ^ Specifies whether the length of a character column is in bytes or in characters. To indicate that the character column length is in characters, set this attribute to @CHAR@ . Otherwise, the character column length is in bytes.
--
-- Example: @charLengthSemantics=CHAR;@ 
  , databaseName :: Core.Maybe Core.Text
    -- ^ Database name for the endpoint.
  , directPathNoLog :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , this attribute helps to increase the commit rate on the Oracle target database by writing directly to tables and not writing a trail to database logs.
  , directPathParallelLoad :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , this attribute specifies a parallel load when @useDirectPathFullLoad@ is set to @Y@ . This attribute also only applies when you use the AWS DMS parallel load feature. Note that the target table cannot have any constraints or indexes.
  , enableHomogenousTablespace :: Core.Maybe Core.Bool
    -- ^ Set this attribute to enable homogenous tablespace replication and create existing tables or indexes under the same tablespace on the target.
  , failTasksOnLobTruncation :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , this attribute causes a task to fail if the actual size of an LOB column is greater than the specified @LobMaxSize@ .
--
-- If a task is set to limited LOB mode and this option is set to @true@ , the task fails instead of truncating the LOB data.
  , numberDatatypeScale :: Core.Maybe Core.Int
    -- ^ Specifies the number scale. You can select a scale up to 38, or you can select FLOAT. By default, the NUMBER data type is converted to precision 38, scale 10.
--
-- Example: @numberDataTypeScale=12@ 
  , oraclePathPrefix :: Core.Maybe Core.Text
    -- ^ Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the default Oracle root used to access the redo logs.
  , parallelAsmReadThreads :: Core.Maybe Core.Int
    -- ^ Set this attribute to change the number of threads that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 2 (the default) and 8 (the maximum). Use this attribute together with the @readAheadBlocks@ attribute.
  , password :: Core.Maybe Types.Password
    -- ^ Endpoint connection password.
  , port :: Core.Maybe Core.Int
    -- ^ Endpoint TCP port.
  , readAheadBlocks :: Core.Maybe Core.Int
    -- ^ Set this attribute to change the number of read-ahead blocks that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 1000 (the default) and 200,000 (the maximum).
  , readTableSpaceName :: Core.Maybe Core.Bool
    -- ^ When set to @true@ , this attribute supports tablespace replication.
  , replacePathPrefix :: Core.Maybe Core.Bool
    -- ^ Set this attribute to true in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This setting tells DMS instance to replace the default Oracle root with the specified @usePathPrefix@ setting to access the redo logs.
  , retryInterval :: Core.Maybe Core.Int
    -- ^ Specifies the number of seconds that the system waits before resending a query.
--
-- Example: @retryInterval=6;@ 
  , securityDbEncryption :: Core.Maybe Types.SecurityDbEncryption
    -- ^ For an Oracle source endpoint, the transparent data encryption (TDE) password required by AWM DMS to access Oracle redo logs encrypted by TDE using Binary Reader. It is also the @/TDE_Password/ @ part of the comma-separated value you set to the @Password@ request parameter when you create the endpoint. The @SecurityDbEncryptian@ setting is related to this @SecurityDbEncryptionName@ setting. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ . 
  , securityDbEncryptionName :: Core.Maybe Core.Text
    -- ^ For an Oracle source endpoint, the name of a key used for the transparent data encryption (TDE) of the columns and tablespaces in an Oracle source database that is encrypted using TDE. The key value is the value of the @SecurityDbEncryption@ setting. For more information on setting the key name value of @SecurityDbEncryptionName@ , see the information and example for setting the @securityDbEncryptionName@ extra connection attribute in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
  , serverName :: Core.Maybe Core.Text
    -- ^ Fully qualified domain name of the endpoint.
  , useAlternateFolderForOnline :: Core.Maybe Core.Bool
    -- ^ Set this attribute to @true@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to use any specified prefix replacement to access all online redo logs.
  , usePathPrefix :: Core.Maybe Core.Text
    -- ^ Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the path prefix used to replace the default Oracle root to access the redo logs.
  , username :: Core.Maybe Core.Text
    -- ^ Endpoint connection user name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OracleSettings' value with any optional fields omitted.
mkOracleSettings
    :: OracleSettings
mkOracleSettings
  = OracleSettings'{accessAlternateDirectly = Core.Nothing,
                    addSupplementalLogging = Core.Nothing,
                    additionalArchivedLogDestId = Core.Nothing,
                    allowSelectNestedTables = Core.Nothing,
                    archivedLogDestId = Core.Nothing, archivedLogsOnly = Core.Nothing,
                    asmPassword = Core.Nothing, asmServer = Core.Nothing,
                    asmUser = Core.Nothing, charLengthSemantics = Core.Nothing,
                    databaseName = Core.Nothing, directPathNoLog = Core.Nothing,
                    directPathParallelLoad = Core.Nothing,
                    enableHomogenousTablespace = Core.Nothing,
                    failTasksOnLobTruncation = Core.Nothing,
                    numberDatatypeScale = Core.Nothing,
                    oraclePathPrefix = Core.Nothing,
                    parallelAsmReadThreads = Core.Nothing, password = Core.Nothing,
                    port = Core.Nothing, readAheadBlocks = Core.Nothing,
                    readTableSpaceName = Core.Nothing,
                    replacePathPrefix = Core.Nothing, retryInterval = Core.Nothing,
                    securityDbEncryption = Core.Nothing,
                    securityDbEncryptionName = Core.Nothing, serverName = Core.Nothing,
                    useAlternateFolderForOnline = Core.Nothing,
                    usePathPrefix = Core.Nothing, username = Core.Nothing}

-- | Set this attribute to @false@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to not access redo logs through any specified path prefix replacement using direct file access.
--
-- /Note:/ Consider using 'accessAlternateDirectly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAccessAlternateDirectly :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osAccessAlternateDirectly = Lens.field @"accessAlternateDirectly"
{-# INLINEABLE osAccessAlternateDirectly #-}
{-# DEPRECATED accessAlternateDirectly "Use generic-lens or generic-optics with 'accessAlternateDirectly' instead"  #-}

-- | Set this attribute to set up table-level supplemental logging for the Oracle database. This attribute enables PRIMARY KEY supplemental logging on all tables selected for a migration task.
--
-- If you use this option, you still need to enable database-level supplemental logging.
--
-- /Note:/ Consider using 'addSupplementalLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAddSupplementalLogging :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osAddSupplementalLogging = Lens.field @"addSupplementalLogging"
{-# INLINEABLE osAddSupplementalLogging #-}
{-# DEPRECATED addSupplementalLogging "Use generic-lens or generic-optics with 'addSupplementalLogging' instead"  #-}

-- | Set this attribute with @archivedLogDestId@ in a primary/ standby setup. This attribute is useful in the case of a switchover. In this case, AWS DMS needs to know which destination to get archive redo logs from to read changes. This need arises because the previous primary instance is now a standby instance after switchover.
--
-- /Note:/ Consider using 'additionalArchivedLogDestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAdditionalArchivedLogDestId :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
osAdditionalArchivedLogDestId = Lens.field @"additionalArchivedLogDestId"
{-# INLINEABLE osAdditionalArchivedLogDestId #-}
{-# DEPRECATED additionalArchivedLogDestId "Use generic-lens or generic-optics with 'additionalArchivedLogDestId' instead"  #-}

-- | Set this attribute to @true@ to enable replication of Oracle tables containing columns that are nested tables or defined types.
--
-- /Note:/ Consider using 'allowSelectNestedTables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAllowSelectNestedTables :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osAllowSelectNestedTables = Lens.field @"allowSelectNestedTables"
{-# INLINEABLE osAllowSelectNestedTables #-}
{-# DEPRECATED allowSelectNestedTables "Use generic-lens or generic-optics with 'allowSelectNestedTables' instead"  #-}

-- | Specifies the destination of the archived redo logs. The value should be the same as the DEST_ID number in the v$archived_log table. When working with multiple log destinations (DEST_ID), we recommend that you to specify an archived redo logs location identifier. Doing this improves performance by ensuring that the correct logs are accessed from the outset.
--
-- /Note:/ Consider using 'archivedLogDestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osArchivedLogDestId :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
osArchivedLogDestId = Lens.field @"archivedLogDestId"
{-# INLINEABLE osArchivedLogDestId #-}
{-# DEPRECATED archivedLogDestId "Use generic-lens or generic-optics with 'archivedLogDestId' instead"  #-}

-- | When this field is set to @Y@ , AWS DMS only accesses the archived redo logs. If the archived redo logs are stored on Oracle ASM only, the AWS DMS user account needs to be granted ASM privileges.
--
-- /Note:/ Consider using 'archivedLogsOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osArchivedLogsOnly :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osArchivedLogsOnly = Lens.field @"archivedLogsOnly"
{-# INLINEABLE osArchivedLogsOnly #-}
{-# DEPRECATED archivedLogsOnly "Use generic-lens or generic-optics with 'archivedLogsOnly' instead"  #-}

-- | For an Oracle source endpoint, your Oracle Automatic Storage Management (ASM) password. You can set this value from the @/asm_user_password/ @ value. You set this value as part of the comma-separated value that you set to the @Password@ request parameter when you create the endpoint to access transaction logs using Binary Reader. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- /Note:/ Consider using 'asmPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAsmPassword :: Lens.Lens' OracleSettings (Core.Maybe Types.AsmPassword)
osAsmPassword = Lens.field @"asmPassword"
{-# INLINEABLE osAsmPassword #-}
{-# DEPRECATED asmPassword "Use generic-lens or generic-optics with 'asmPassword' instead"  #-}

-- | For an Oracle source endpoint, your ASM server address. You can set this value from the @asm_server@ value. You set @asm_server@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- /Note:/ Consider using 'asmServer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAsmServer :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
osAsmServer = Lens.field @"asmServer"
{-# INLINEABLE osAsmServer #-}
{-# DEPRECATED asmServer "Use generic-lens or generic-optics with 'asmServer' instead"  #-}

-- | For an Oracle source endpoint, your ASM user name. You can set this value from the @asm_user@ value. You set @asm_user@ as part of the extra connection attribute string to access an Oracle server with Binary Reader that uses ASM. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database> .
--
-- /Note:/ Consider using 'asmUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osAsmUser :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
osAsmUser = Lens.field @"asmUser"
{-# INLINEABLE osAsmUser #-}
{-# DEPRECATED asmUser "Use generic-lens or generic-optics with 'asmUser' instead"  #-}

-- | Specifies whether the length of a character column is in bytes or in characters. To indicate that the character column length is in characters, set this attribute to @CHAR@ . Otherwise, the character column length is in bytes.
--
-- Example: @charLengthSemantics=CHAR;@ 
--
-- /Note:/ Consider using 'charLengthSemantics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osCharLengthSemantics :: Lens.Lens' OracleSettings (Core.Maybe Types.CharLengthSemantics)
osCharLengthSemantics = Lens.field @"charLengthSemantics"
{-# INLINEABLE osCharLengthSemantics #-}
{-# DEPRECATED charLengthSemantics "Use generic-lens or generic-optics with 'charLengthSemantics' instead"  #-}

-- | Database name for the endpoint.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDatabaseName :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
osDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE osDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | When set to @true@ , this attribute helps to increase the commit rate on the Oracle target database by writing directly to tables and not writing a trail to database logs.
--
-- /Note:/ Consider using 'directPathNoLog' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDirectPathNoLog :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osDirectPathNoLog = Lens.field @"directPathNoLog"
{-# INLINEABLE osDirectPathNoLog #-}
{-# DEPRECATED directPathNoLog "Use generic-lens or generic-optics with 'directPathNoLog' instead"  #-}

-- | When set to @true@ , this attribute specifies a parallel load when @useDirectPathFullLoad@ is set to @Y@ . This attribute also only applies when you use the AWS DMS parallel load feature. Note that the target table cannot have any constraints or indexes.
--
-- /Note:/ Consider using 'directPathParallelLoad' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osDirectPathParallelLoad :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osDirectPathParallelLoad = Lens.field @"directPathParallelLoad"
{-# INLINEABLE osDirectPathParallelLoad #-}
{-# DEPRECATED directPathParallelLoad "Use generic-lens or generic-optics with 'directPathParallelLoad' instead"  #-}

-- | Set this attribute to enable homogenous tablespace replication and create existing tables or indexes under the same tablespace on the target.
--
-- /Note:/ Consider using 'enableHomogenousTablespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osEnableHomogenousTablespace :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osEnableHomogenousTablespace = Lens.field @"enableHomogenousTablespace"
{-# INLINEABLE osEnableHomogenousTablespace #-}
{-# DEPRECATED enableHomogenousTablespace "Use generic-lens or generic-optics with 'enableHomogenousTablespace' instead"  #-}

-- | When set to @true@ , this attribute causes a task to fail if the actual size of an LOB column is greater than the specified @LobMaxSize@ .
--
-- If a task is set to limited LOB mode and this option is set to @true@ , the task fails instead of truncating the LOB data.
--
-- /Note:/ Consider using 'failTasksOnLobTruncation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osFailTasksOnLobTruncation :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osFailTasksOnLobTruncation = Lens.field @"failTasksOnLobTruncation"
{-# INLINEABLE osFailTasksOnLobTruncation #-}
{-# DEPRECATED failTasksOnLobTruncation "Use generic-lens or generic-optics with 'failTasksOnLobTruncation' instead"  #-}

-- | Specifies the number scale. You can select a scale up to 38, or you can select FLOAT. By default, the NUMBER data type is converted to precision 38, scale 10.
--
-- Example: @numberDataTypeScale=12@ 
--
-- /Note:/ Consider using 'numberDatatypeScale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osNumberDatatypeScale :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
osNumberDatatypeScale = Lens.field @"numberDatatypeScale"
{-# INLINEABLE osNumberDatatypeScale #-}
{-# DEPRECATED numberDatatypeScale "Use generic-lens or generic-optics with 'numberDatatypeScale' instead"  #-}

-- | Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the default Oracle root used to access the redo logs.
--
-- /Note:/ Consider using 'oraclePathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOraclePathPrefix :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
osOraclePathPrefix = Lens.field @"oraclePathPrefix"
{-# INLINEABLE osOraclePathPrefix #-}
{-# DEPRECATED oraclePathPrefix "Use generic-lens or generic-optics with 'oraclePathPrefix' instead"  #-}

-- | Set this attribute to change the number of threads that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 2 (the default) and 8 (the maximum). Use this attribute together with the @readAheadBlocks@ attribute.
--
-- /Note:/ Consider using 'parallelAsmReadThreads' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osParallelAsmReadThreads :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
osParallelAsmReadThreads = Lens.field @"parallelAsmReadThreads"
{-# INLINEABLE osParallelAsmReadThreads #-}
{-# DEPRECATED parallelAsmReadThreads "Use generic-lens or generic-optics with 'parallelAsmReadThreads' instead"  #-}

-- | Endpoint connection password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPassword :: Lens.Lens' OracleSettings (Core.Maybe Types.Password)
osPassword = Lens.field @"password"
{-# INLINEABLE osPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | Endpoint TCP port.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osPort :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
osPort = Lens.field @"port"
{-# INLINEABLE osPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | Set this attribute to change the number of read-ahead blocks that DMS configures to perform a Change Data Capture (CDC) load using Oracle Automatic Storage Management (ASM). You can specify an integer value between 1000 (the default) and 200,000 (the maximum).
--
-- /Note:/ Consider using 'readAheadBlocks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReadAheadBlocks :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
osReadAheadBlocks = Lens.field @"readAheadBlocks"
{-# INLINEABLE osReadAheadBlocks #-}
{-# DEPRECATED readAheadBlocks "Use generic-lens or generic-optics with 'readAheadBlocks' instead"  #-}

-- | When set to @true@ , this attribute supports tablespace replication.
--
-- /Note:/ Consider using 'readTableSpaceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReadTableSpaceName :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osReadTableSpaceName = Lens.field @"readTableSpaceName"
{-# INLINEABLE osReadTableSpaceName #-}
{-# DEPRECATED readTableSpaceName "Use generic-lens or generic-optics with 'readTableSpaceName' instead"  #-}

-- | Set this attribute to true in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This setting tells DMS instance to replace the default Oracle root with the specified @usePathPrefix@ setting to access the redo logs.
--
-- /Note:/ Consider using 'replacePathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osReplacePathPrefix :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osReplacePathPrefix = Lens.field @"replacePathPrefix"
{-# INLINEABLE osReplacePathPrefix #-}
{-# DEPRECATED replacePathPrefix "Use generic-lens or generic-optics with 'replacePathPrefix' instead"  #-}

-- | Specifies the number of seconds that the system waits before resending a query.
--
-- Example: @retryInterval=6;@ 
--
-- /Note:/ Consider using 'retryInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osRetryInterval :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
osRetryInterval = Lens.field @"retryInterval"
{-# INLINEABLE osRetryInterval #-}
{-# DEPRECATED retryInterval "Use generic-lens or generic-optics with 'retryInterval' instead"  #-}

-- | For an Oracle source endpoint, the transparent data encryption (TDE) password required by AWM DMS to access Oracle redo logs encrypted by TDE using Binary Reader. It is also the @/TDE_Password/ @ part of the comma-separated value you set to the @Password@ request parameter when you create the endpoint. The @SecurityDbEncryptian@ setting is related to this @SecurityDbEncryptionName@ setting. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ . 
--
-- /Note:/ Consider using 'securityDbEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSecurityDbEncryption :: Lens.Lens' OracleSettings (Core.Maybe Types.SecurityDbEncryption)
osSecurityDbEncryption = Lens.field @"securityDbEncryption"
{-# INLINEABLE osSecurityDbEncryption #-}
{-# DEPRECATED securityDbEncryption "Use generic-lens or generic-optics with 'securityDbEncryption' instead"  #-}

-- | For an Oracle source endpoint, the name of a key used for the transparent data encryption (TDE) of the columns and tablespaces in an Oracle source database that is encrypted using TDE. The key value is the value of the @SecurityDbEncryption@ setting. For more information on setting the key name value of @SecurityDbEncryptionName@ , see the information and example for setting the @securityDbEncryptionName@ extra connection attribute in <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS> in the /AWS Database Migration Service User Guide/ .
--
-- /Note:/ Consider using 'securityDbEncryptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSecurityDbEncryptionName :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
osSecurityDbEncryptionName = Lens.field @"securityDbEncryptionName"
{-# INLINEABLE osSecurityDbEncryptionName #-}
{-# DEPRECATED securityDbEncryptionName "Use generic-lens or generic-optics with 'securityDbEncryptionName' instead"  #-}

-- | Fully qualified domain name of the endpoint.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osServerName :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
osServerName = Lens.field @"serverName"
{-# INLINEABLE osServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

-- | Set this attribute to @true@ in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This tells the DMS instance to use any specified prefix replacement to access all online redo logs.
--
-- /Note:/ Consider using 'useAlternateFolderForOnline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUseAlternateFolderForOnline :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
osUseAlternateFolderForOnline = Lens.field @"useAlternateFolderForOnline"
{-# INLINEABLE osUseAlternateFolderForOnline #-}
{-# DEPRECATED useAlternateFolderForOnline "Use generic-lens or generic-optics with 'useAlternateFolderForOnline' instead"  #-}

-- | Set this string attribute to the required value in order to use the Binary Reader to capture change data for an Amazon RDS for Oracle as the source. This value specifies the path prefix used to replace the default Oracle root to access the redo logs.
--
-- /Note:/ Consider using 'usePathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUsePathPrefix :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
osUsePathPrefix = Lens.field @"usePathPrefix"
{-# INLINEABLE osUsePathPrefix #-}
{-# DEPRECATED usePathPrefix "Use generic-lens or generic-optics with 'usePathPrefix' instead"  #-}

-- | Endpoint connection user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osUsername :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
osUsername = Lens.field @"username"
{-# INLINEABLE osUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON OracleSettings where
        toJSON OracleSettings{..}
          = Core.object
              (Core.catMaybes
                 [("AccessAlternateDirectly" Core..=) Core.<$>
                    accessAlternateDirectly,
                  ("AddSupplementalLogging" Core..=) Core.<$> addSupplementalLogging,
                  ("AdditionalArchivedLogDestId" Core..=) Core.<$>
                    additionalArchivedLogDestId,
                  ("AllowSelectNestedTables" Core..=) Core.<$>
                    allowSelectNestedTables,
                  ("ArchivedLogDestId" Core..=) Core.<$> archivedLogDestId,
                  ("ArchivedLogsOnly" Core..=) Core.<$> archivedLogsOnly,
                  ("AsmPassword" Core..=) Core.<$> asmPassword,
                  ("AsmServer" Core..=) Core.<$> asmServer,
                  ("AsmUser" Core..=) Core.<$> asmUser,
                  ("CharLengthSemantics" Core..=) Core.<$> charLengthSemantics,
                  ("DatabaseName" Core..=) Core.<$> databaseName,
                  ("DirectPathNoLog" Core..=) Core.<$> directPathNoLog,
                  ("DirectPathParallelLoad" Core..=) Core.<$> directPathParallelLoad,
                  ("EnableHomogenousTablespace" Core..=) Core.<$>
                    enableHomogenousTablespace,
                  ("FailTasksOnLobTruncation" Core..=) Core.<$>
                    failTasksOnLobTruncation,
                  ("NumberDatatypeScale" Core..=) Core.<$> numberDatatypeScale,
                  ("OraclePathPrefix" Core..=) Core.<$> oraclePathPrefix,
                  ("ParallelAsmReadThreads" Core..=) Core.<$> parallelAsmReadThreads,
                  ("Password" Core..=) Core.<$> password,
                  ("Port" Core..=) Core.<$> port,
                  ("ReadAheadBlocks" Core..=) Core.<$> readAheadBlocks,
                  ("ReadTableSpaceName" Core..=) Core.<$> readTableSpaceName,
                  ("ReplacePathPrefix" Core..=) Core.<$> replacePathPrefix,
                  ("RetryInterval" Core..=) Core.<$> retryInterval,
                  ("SecurityDbEncryption" Core..=) Core.<$> securityDbEncryption,
                  ("SecurityDbEncryptionName" Core..=) Core.<$>
                    securityDbEncryptionName,
                  ("ServerName" Core..=) Core.<$> serverName,
                  ("UseAlternateFolderForOnline" Core..=) Core.<$>
                    useAlternateFolderForOnline,
                  ("UsePathPrefix" Core..=) Core.<$> usePathPrefix,
                  ("Username" Core..=) Core.<$> username])

instance Core.FromJSON OracleSettings where
        parseJSON
          = Core.withObject "OracleSettings" Core.$
              \ x ->
                OracleSettings' Core.<$>
                  (x Core..:? "AccessAlternateDirectly") Core.<*>
                    x Core..:? "AddSupplementalLogging"
                    Core.<*> x Core..:? "AdditionalArchivedLogDestId"
                    Core.<*> x Core..:? "AllowSelectNestedTables"
                    Core.<*> x Core..:? "ArchivedLogDestId"
                    Core.<*> x Core..:? "ArchivedLogsOnly"
                    Core.<*> x Core..:? "AsmPassword"
                    Core.<*> x Core..:? "AsmServer"
                    Core.<*> x Core..:? "AsmUser"
                    Core.<*> x Core..:? "CharLengthSemantics"
                    Core.<*> x Core..:? "DatabaseName"
                    Core.<*> x Core..:? "DirectPathNoLog"
                    Core.<*> x Core..:? "DirectPathParallelLoad"
                    Core.<*> x Core..:? "EnableHomogenousTablespace"
                    Core.<*> x Core..:? "FailTasksOnLobTruncation"
                    Core.<*> x Core..:? "NumberDatatypeScale"
                    Core.<*> x Core..:? "OraclePathPrefix"
                    Core.<*> x Core..:? "ParallelAsmReadThreads"
                    Core.<*> x Core..:? "Password"
                    Core.<*> x Core..:? "Port"
                    Core.<*> x Core..:? "ReadAheadBlocks"
                    Core.<*> x Core..:? "ReadTableSpaceName"
                    Core.<*> x Core..:? "ReplacePathPrefix"
                    Core.<*> x Core..:? "RetryInterval"
                    Core.<*> x Core..:? "SecurityDbEncryption"
                    Core.<*> x Core..:? "SecurityDbEncryptionName"
                    Core.<*> x Core..:? "ServerName"
                    Core.<*> x Core..:? "UseAlternateFolderForOnline"
                    Core.<*> x Core..:? "UsePathPrefix"
                    Core.<*> x Core..:? "Username"

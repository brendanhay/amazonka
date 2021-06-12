{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.OracleSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.OracleSettings where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types.CharLengthSemantics
import qualified Network.AWS.Lens as Lens

-- | Provides information that defines an Oracle endpoint.
--
-- /See:/ 'newOracleSettings' smart constructor.
data OracleSettings = OracleSettings'
  { -- | When set to @true@, this attribute causes a task to fail if the actual
    -- size of an LOB column is greater than the specified @LobMaxSize@.
    --
    -- If a task is set to limited LOB mode and this option is set to @true@,
    -- the task fails instead of truncating the LOB data.
    failTasksOnLobTruncation :: Core.Maybe Core.Bool,
    -- | Specifies the number of seconds that the system waits before resending a
    -- query.
    --
    -- Example: @retryInterval=6;@
    retryInterval :: Core.Maybe Core.Int,
    -- | Required only if your Oracle endpoint uses Advanced Storage Manager
    -- (ASM). The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerOracleAsmSecret@ that contains the Oracle ASM connection
    -- details for the Oracle endpoint.
    secretsManagerOracleAsmSecretId :: Core.Maybe Core.Text,
    -- | Set this attribute to @false@ in order to use the Binary Reader to
    -- capture change data for an Amazon RDS for Oracle as the source. This
    -- tells the DMS instance to not access redo logs through any specified
    -- path prefix replacement using direct file access.
    accessAlternateDirectly :: Core.Maybe Core.Bool,
    -- | Set this attribute to @true@ in order to use the Binary Reader to
    -- capture change data for an Amazon RDS for Oracle as the source. This
    -- tells the DMS instance to use any specified prefix replacement to access
    -- all online redo logs.
    useAlternateFolderForOnline :: Core.Maybe Core.Bool,
    -- | Specifies the number scale. You can select a scale up to 38, or you can
    -- select FLOAT. By default, the NUMBER data type is converted to precision
    -- 38, scale 10.
    --
    -- Example: @numberDataTypeScale=12@
    numberDatatypeScale :: Core.Maybe Core.Int,
    -- | Set this string attribute to the required value in order to use the
    -- Binary Reader to capture change data for an Amazon RDS for Oracle as the
    -- source. This value specifies the default Oracle root used to access the
    -- redo logs.
    oraclePathPrefix :: Core.Maybe Core.Text,
    -- | For an Oracle source endpoint, the name of a key used for the
    -- transparent data encryption (TDE) of the columns and tablespaces in an
    -- Oracle source database that is encrypted using TDE. The key value is the
    -- value of the @SecurityDbEncryption@ setting. For more information on
    -- setting the key name value of @SecurityDbEncryptionName@, see the
    -- information and example for setting the @securityDbEncryptionName@ extra
    -- connection attribute in
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
    -- in the /AWS Database Migration Service User Guide/.
    securityDbEncryptionName :: Core.Maybe Core.Text,
    -- | Set this attribute with @archivedLogDestId@ in a primary\/ standby
    -- setup. This attribute is useful in the case of a switchover. In this
    -- case, AWS DMS needs to know which destination to get archive redo logs
    -- from to read changes. This need arises because the previous primary
    -- instance is now a standby instance after switchover.
    additionalArchivedLogDestId :: Core.Maybe Core.Int,
    -- | For an Oracle source endpoint, your Oracle Automatic Storage Management
    -- (ASM) password. You can set this value from the @ asm_user_password @
    -- value. You set this value as part of the comma-separated value that you
    -- set to the @Password@ request parameter when you create the endpoint to
    -- access transaction logs using Binary Reader. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
    asmPassword :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the Oracle endpoint connection
    -- details.
    secretsManagerSecretId :: Core.Maybe Core.Text,
    -- | When this field is set to @Y@, AWS DMS only accesses the archived redo
    -- logs. If the archived redo logs are stored on Oracle ASM only, the AWS
    -- DMS user account needs to be granted ASM privileges.
    archivedLogsOnly :: Core.Maybe Core.Bool,
    -- | When set to @true@, this attribute specifies a parallel load when
    -- @useDirectPathFullLoad@ is set to @Y@. This attribute also only applies
    -- when you use the AWS DMS parallel load feature. Note that the target
    -- table cannot have any constraints or indexes.
    directPathParallelLoad :: Core.Maybe Core.Bool,
    -- | When set to @true@, this attribute helps to increase the commit rate on
    -- the Oracle target database by writing directly to tables and not writing
    -- a trail to database logs.
    directPathNoLog :: Core.Maybe Core.Bool,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Core.Maybe Core.Text,
    -- | For an Oracle source endpoint, your ASM server address. You can set this
    -- value from the @asm_server@ value. You set @asm_server@ as part of the
    -- extra connection attribute string to access an Oracle server with Binary
    -- Reader that uses ASM. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
    asmServer :: Core.Maybe Core.Text,
    -- | For an Oracle source endpoint, the transparent data encryption (TDE)
    -- password required by AWM DMS to access Oracle redo logs encrypted by TDE
    -- using Binary Reader. It is also the @ TDE_Password @ part of the
    -- comma-separated value you set to the @Password@ request parameter when
    -- you create the endpoint. The @SecurityDbEncryptian@ setting is related
    -- to this @SecurityDbEncryptionName@ setting. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
    -- in the /AWS Database Migration Service User Guide/.
    securityDbEncryption :: Core.Maybe (Core.Sensitive Core.Text),
    -- | When set to @true@, this attribute supports tablespace replication.
    readTableSpaceName :: Core.Maybe Core.Bool,
    -- | Endpoint connection password.
    password :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Set this attribute to @true@ to enable replication of Oracle tables
    -- containing columns that are nested tables or defined types.
    allowSelectNestedTables :: Core.Maybe Core.Bool,
    -- | Specifies the destination of the archived redo logs. The value should be
    -- the same as the DEST_ID number in the v$archived_log table. When working
    -- with multiple log destinations (DEST_ID), we recommend that you to
    -- specify an archived redo logs location identifier. Doing this improves
    -- performance by ensuring that the correct logs are accessed from the
    -- outset.
    archivedLogDestId :: Core.Maybe Core.Int,
    -- | Set this attribute to true in order to use the Binary Reader to capture
    -- change data for an Amazon RDS for Oracle as the source. This setting
    -- tells DMS instance to replace the default Oracle root with the specified
    -- @usePathPrefix@ setting to access the redo logs.
    replacePathPrefix :: Core.Maybe Core.Bool,
    -- | Endpoint TCP port.
    port :: Core.Maybe Core.Int,
    -- | Set this attribute to change the number of read-ahead blocks that DMS
    -- configures to perform a Change Data Capture (CDC) load using Oracle
    -- Automatic Storage Management (ASM). You can specify an integer value
    -- between 1000 (the default) and 200,000 (the maximum).
    readAheadBlocks :: Core.Maybe Core.Int,
    -- | Set this string attribute to the required value in order to use the
    -- Binary Reader to capture change data for an Amazon RDS for Oracle as the
    -- source. This value specifies the path prefix used to replace the default
    -- Oracle root to access the redo logs.
    usePathPrefix :: Core.Maybe Core.Text,
    -- | For an Oracle source endpoint, your ASM user name. You can set this
    -- value from the @asm_user@ value. You set @asm_user@ as part of the extra
    -- connection attribute string to access an Oracle server with Binary
    -- Reader that uses ASM. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
    asmUser :: Core.Maybe Core.Text,
    -- | Endpoint connection user name.
    username :: Core.Maybe Core.Text,
    -- | Set this attribute to enable homogenous tablespace replication and
    -- create existing tables or indexes under the same tablespace on the
    -- target.
    enableHomogenousTablespace :: Core.Maybe Core.Bool,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
    -- DMS as the trusted entity and grants the required permissions to access
    -- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
    -- value of the AWS Secrets Manager secret that allows access to the Oracle
    -- endpoint.
    --
    -- You can specify one of two sets of values for these permissions. You can
    -- specify the values for this setting and @SecretsManagerSecretId@. Or you
    -- can specify clear-text values for @UserName@, @Password@, @ServerName@,
    -- and @Port@. You can\'t specify both. For more information on creating
    -- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
    -- @SecretsManagerSecretId@ required to access it, see
    -- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
    -- in the /AWS Database Migration Service User Guide/.
    secretsManagerAccessRoleArn :: Core.Maybe Core.Text,
    -- | Set this attribute to change the number of threads that DMS configures
    -- to perform a Change Data Capture (CDC) load using Oracle Automatic
    -- Storage Management (ASM). You can specify an integer value between 2
    -- (the default) and 8 (the maximum). Use this attribute together with the
    -- @readAheadBlocks@ attribute.
    parallelAsmReadThreads :: Core.Maybe Core.Int,
    -- | Specifies whether the length of a character column is in bytes or in
    -- characters. To indicate that the character column length is in
    -- characters, set this attribute to @CHAR@. Otherwise, the character
    -- column length is in bytes.
    --
    -- Example: @charLengthSemantics=CHAR;@
    charLengthSemantics :: Core.Maybe CharLengthSemantics,
    -- | Set this attribute to set up table-level supplemental logging for the
    -- Oracle database. This attribute enables PRIMARY KEY supplemental logging
    -- on all tables selected for a migration task.
    --
    -- If you use this option, you still need to enable database-level
    -- supplemental logging.
    addSupplementalLogging :: Core.Maybe Core.Bool,
    -- | Required only if your Oracle endpoint uses Advanced Storage Manager
    -- (ASM). The full ARN of the IAM role that specifies AWS DMS as the
    -- trusted entity and grants the required permissions to access the
    -- @SecretsManagerOracleAsmSecret@. This @SecretsManagerOracleAsmSecret@
    -- has the secret value that allows access to the Oracle ASM of the
    -- endpoint.
    --
    -- You can specify one of two sets of values for these permissions. You can
    -- specify the values for this setting and
    -- @SecretsManagerOracleAsmSecretId@. Or you can specify clear-text values
    -- for @AsmUserName@, @AsmPassword@, and @AsmServerName@. You can\'t
    -- specify both. For more information on creating this
    -- @SecretsManagerOracleAsmSecret@ and the
    -- @SecretsManagerOracleAsmAccessRoleArn@ and
    -- @SecretsManagerOracleAsmSecretId@ required to access it, see
    -- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
    -- in the /AWS Database Migration Service User Guide/.
    secretsManagerOracleAsmAccessRoleArn :: Core.Maybe Core.Text,
    -- | Database name for the endpoint.
    databaseName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'OracleSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failTasksOnLobTruncation', 'oracleSettings_failTasksOnLobTruncation' - When set to @true@, this attribute causes a task to fail if the actual
-- size of an LOB column is greater than the specified @LobMaxSize@.
--
-- If a task is set to limited LOB mode and this option is set to @true@,
-- the task fails instead of truncating the LOB data.
--
-- 'retryInterval', 'oracleSettings_retryInterval' - Specifies the number of seconds that the system waits before resending a
-- query.
--
-- Example: @retryInterval=6;@
--
-- 'secretsManagerOracleAsmSecretId', 'oracleSettings_secretsManagerOracleAsmSecretId' - Required only if your Oracle endpoint uses Advanced Storage Manager
-- (ASM). The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerOracleAsmSecret@ that contains the Oracle ASM connection
-- details for the Oracle endpoint.
--
-- 'accessAlternateDirectly', 'oracleSettings_accessAlternateDirectly' - Set this attribute to @false@ in order to use the Binary Reader to
-- capture change data for an Amazon RDS for Oracle as the source. This
-- tells the DMS instance to not access redo logs through any specified
-- path prefix replacement using direct file access.
--
-- 'useAlternateFolderForOnline', 'oracleSettings_useAlternateFolderForOnline' - Set this attribute to @true@ in order to use the Binary Reader to
-- capture change data for an Amazon RDS for Oracle as the source. This
-- tells the DMS instance to use any specified prefix replacement to access
-- all online redo logs.
--
-- 'numberDatatypeScale', 'oracleSettings_numberDatatypeScale' - Specifies the number scale. You can select a scale up to 38, or you can
-- select FLOAT. By default, the NUMBER data type is converted to precision
-- 38, scale 10.
--
-- Example: @numberDataTypeScale=12@
--
-- 'oraclePathPrefix', 'oracleSettings_oraclePathPrefix' - Set this string attribute to the required value in order to use the
-- Binary Reader to capture change data for an Amazon RDS for Oracle as the
-- source. This value specifies the default Oracle root used to access the
-- redo logs.
--
-- 'securityDbEncryptionName', 'oracleSettings_securityDbEncryptionName' - For an Oracle source endpoint, the name of a key used for the
-- transparent data encryption (TDE) of the columns and tablespaces in an
-- Oracle source database that is encrypted using TDE. The key value is the
-- value of the @SecurityDbEncryption@ setting. For more information on
-- setting the key name value of @SecurityDbEncryptionName@, see the
-- information and example for setting the @securityDbEncryptionName@ extra
-- connection attribute in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
-- in the /AWS Database Migration Service User Guide/.
--
-- 'additionalArchivedLogDestId', 'oracleSettings_additionalArchivedLogDestId' - Set this attribute with @archivedLogDestId@ in a primary\/ standby
-- setup. This attribute is useful in the case of a switchover. In this
-- case, AWS DMS needs to know which destination to get archive redo logs
-- from to read changes. This need arises because the previous primary
-- instance is now a standby instance after switchover.
--
-- 'asmPassword', 'oracleSettings_asmPassword' - For an Oracle source endpoint, your Oracle Automatic Storage Management
-- (ASM) password. You can set this value from the @ asm_user_password @
-- value. You set this value as part of the comma-separated value that you
-- set to the @Password@ request parameter when you create the endpoint to
-- access transaction logs using Binary Reader. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
--
-- 'secretsManagerSecretId', 'oracleSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Oracle endpoint connection
-- details.
--
-- 'archivedLogsOnly', 'oracleSettings_archivedLogsOnly' - When this field is set to @Y@, AWS DMS only accesses the archived redo
-- logs. If the archived redo logs are stored on Oracle ASM only, the AWS
-- DMS user account needs to be granted ASM privileges.
--
-- 'directPathParallelLoad', 'oracleSettings_directPathParallelLoad' - When set to @true@, this attribute specifies a parallel load when
-- @useDirectPathFullLoad@ is set to @Y@. This attribute also only applies
-- when you use the AWS DMS parallel load feature. Note that the target
-- table cannot have any constraints or indexes.
--
-- 'directPathNoLog', 'oracleSettings_directPathNoLog' - When set to @true@, this attribute helps to increase the commit rate on
-- the Oracle target database by writing directly to tables and not writing
-- a trail to database logs.
--
-- 'serverName', 'oracleSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'asmServer', 'oracleSettings_asmServer' - For an Oracle source endpoint, your ASM server address. You can set this
-- value from the @asm_server@ value. You set @asm_server@ as part of the
-- extra connection attribute string to access an Oracle server with Binary
-- Reader that uses ASM. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
--
-- 'securityDbEncryption', 'oracleSettings_securityDbEncryption' - For an Oracle source endpoint, the transparent data encryption (TDE)
-- password required by AWM DMS to access Oracle redo logs encrypted by TDE
-- using Binary Reader. It is also the @ TDE_Password @ part of the
-- comma-separated value you set to the @Password@ request parameter when
-- you create the endpoint. The @SecurityDbEncryptian@ setting is related
-- to this @SecurityDbEncryptionName@ setting. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
-- in the /AWS Database Migration Service User Guide/.
--
-- 'readTableSpaceName', 'oracleSettings_readTableSpaceName' - When set to @true@, this attribute supports tablespace replication.
--
-- 'password', 'oracleSettings_password' - Endpoint connection password.
--
-- 'allowSelectNestedTables', 'oracleSettings_allowSelectNestedTables' - Set this attribute to @true@ to enable replication of Oracle tables
-- containing columns that are nested tables or defined types.
--
-- 'archivedLogDestId', 'oracleSettings_archivedLogDestId' - Specifies the destination of the archived redo logs. The value should be
-- the same as the DEST_ID number in the v$archived_log table. When working
-- with multiple log destinations (DEST_ID), we recommend that you to
-- specify an archived redo logs location identifier. Doing this improves
-- performance by ensuring that the correct logs are accessed from the
-- outset.
--
-- 'replacePathPrefix', 'oracleSettings_replacePathPrefix' - Set this attribute to true in order to use the Binary Reader to capture
-- change data for an Amazon RDS for Oracle as the source. This setting
-- tells DMS instance to replace the default Oracle root with the specified
-- @usePathPrefix@ setting to access the redo logs.
--
-- 'port', 'oracleSettings_port' - Endpoint TCP port.
--
-- 'readAheadBlocks', 'oracleSettings_readAheadBlocks' - Set this attribute to change the number of read-ahead blocks that DMS
-- configures to perform a Change Data Capture (CDC) load using Oracle
-- Automatic Storage Management (ASM). You can specify an integer value
-- between 1000 (the default) and 200,000 (the maximum).
--
-- 'usePathPrefix', 'oracleSettings_usePathPrefix' - Set this string attribute to the required value in order to use the
-- Binary Reader to capture change data for an Amazon RDS for Oracle as the
-- source. This value specifies the path prefix used to replace the default
-- Oracle root to access the redo logs.
--
-- 'asmUser', 'oracleSettings_asmUser' - For an Oracle source endpoint, your ASM user name. You can set this
-- value from the @asm_user@ value. You set @asm_user@ as part of the extra
-- connection attribute string to access an Oracle server with Binary
-- Reader that uses ASM. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
--
-- 'username', 'oracleSettings_username' - Endpoint connection user name.
--
-- 'enableHomogenousTablespace', 'oracleSettings_enableHomogenousTablespace' - Set this attribute to enable homogenous tablespace replication and
-- create existing tables or indexes under the same tablespace on the
-- target.
--
-- 'secretsManagerAccessRoleArn', 'oracleSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the Oracle
-- endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
--
-- 'parallelAsmReadThreads', 'oracleSettings_parallelAsmReadThreads' - Set this attribute to change the number of threads that DMS configures
-- to perform a Change Data Capture (CDC) load using Oracle Automatic
-- Storage Management (ASM). You can specify an integer value between 2
-- (the default) and 8 (the maximum). Use this attribute together with the
-- @readAheadBlocks@ attribute.
--
-- 'charLengthSemantics', 'oracleSettings_charLengthSemantics' - Specifies whether the length of a character column is in bytes or in
-- characters. To indicate that the character column length is in
-- characters, set this attribute to @CHAR@. Otherwise, the character
-- column length is in bytes.
--
-- Example: @charLengthSemantics=CHAR;@
--
-- 'addSupplementalLogging', 'oracleSettings_addSupplementalLogging' - Set this attribute to set up table-level supplemental logging for the
-- Oracle database. This attribute enables PRIMARY KEY supplemental logging
-- on all tables selected for a migration task.
--
-- If you use this option, you still need to enable database-level
-- supplemental logging.
--
-- 'secretsManagerOracleAsmAccessRoleArn', 'oracleSettings_secretsManagerOracleAsmAccessRoleArn' - Required only if your Oracle endpoint uses Advanced Storage Manager
-- (ASM). The full ARN of the IAM role that specifies AWS DMS as the
-- trusted entity and grants the required permissions to access the
-- @SecretsManagerOracleAsmSecret@. This @SecretsManagerOracleAsmSecret@
-- has the secret value that allows access to the Oracle ASM of the
-- endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and
-- @SecretsManagerOracleAsmSecretId@. Or you can specify clear-text values
-- for @AsmUserName@, @AsmPassword@, and @AsmServerName@. You can\'t
-- specify both. For more information on creating this
-- @SecretsManagerOracleAsmSecret@ and the
-- @SecretsManagerOracleAsmAccessRoleArn@ and
-- @SecretsManagerOracleAsmSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
--
-- 'databaseName', 'oracleSettings_databaseName' - Database name for the endpoint.
newOracleSettings ::
  OracleSettings
newOracleSettings =
  OracleSettings'
    { failTasksOnLobTruncation =
        Core.Nothing,
      retryInterval = Core.Nothing,
      secretsManagerOracleAsmSecretId = Core.Nothing,
      accessAlternateDirectly = Core.Nothing,
      useAlternateFolderForOnline = Core.Nothing,
      numberDatatypeScale = Core.Nothing,
      oraclePathPrefix = Core.Nothing,
      securityDbEncryptionName = Core.Nothing,
      additionalArchivedLogDestId = Core.Nothing,
      asmPassword = Core.Nothing,
      secretsManagerSecretId = Core.Nothing,
      archivedLogsOnly = Core.Nothing,
      directPathParallelLoad = Core.Nothing,
      directPathNoLog = Core.Nothing,
      serverName = Core.Nothing,
      asmServer = Core.Nothing,
      securityDbEncryption = Core.Nothing,
      readTableSpaceName = Core.Nothing,
      password = Core.Nothing,
      allowSelectNestedTables = Core.Nothing,
      archivedLogDestId = Core.Nothing,
      replacePathPrefix = Core.Nothing,
      port = Core.Nothing,
      readAheadBlocks = Core.Nothing,
      usePathPrefix = Core.Nothing,
      asmUser = Core.Nothing,
      username = Core.Nothing,
      enableHomogenousTablespace = Core.Nothing,
      secretsManagerAccessRoleArn = Core.Nothing,
      parallelAsmReadThreads = Core.Nothing,
      charLengthSemantics = Core.Nothing,
      addSupplementalLogging = Core.Nothing,
      secretsManagerOracleAsmAccessRoleArn = Core.Nothing,
      databaseName = Core.Nothing
    }

-- | When set to @true@, this attribute causes a task to fail if the actual
-- size of an LOB column is greater than the specified @LobMaxSize@.
--
-- If a task is set to limited LOB mode and this option is set to @true@,
-- the task fails instead of truncating the LOB data.
oracleSettings_failTasksOnLobTruncation :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_failTasksOnLobTruncation = Lens.lens (\OracleSettings' {failTasksOnLobTruncation} -> failTasksOnLobTruncation) (\s@OracleSettings' {} a -> s {failTasksOnLobTruncation = a} :: OracleSettings)

-- | Specifies the number of seconds that the system waits before resending a
-- query.
--
-- Example: @retryInterval=6;@
oracleSettings_retryInterval :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
oracleSettings_retryInterval = Lens.lens (\OracleSettings' {retryInterval} -> retryInterval) (\s@OracleSettings' {} a -> s {retryInterval = a} :: OracleSettings)

-- | Required only if your Oracle endpoint uses Advanced Storage Manager
-- (ASM). The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerOracleAsmSecret@ that contains the Oracle ASM connection
-- details for the Oracle endpoint.
oracleSettings_secretsManagerOracleAsmSecretId :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_secretsManagerOracleAsmSecretId = Lens.lens (\OracleSettings' {secretsManagerOracleAsmSecretId} -> secretsManagerOracleAsmSecretId) (\s@OracleSettings' {} a -> s {secretsManagerOracleAsmSecretId = a} :: OracleSettings)

-- | Set this attribute to @false@ in order to use the Binary Reader to
-- capture change data for an Amazon RDS for Oracle as the source. This
-- tells the DMS instance to not access redo logs through any specified
-- path prefix replacement using direct file access.
oracleSettings_accessAlternateDirectly :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_accessAlternateDirectly = Lens.lens (\OracleSettings' {accessAlternateDirectly} -> accessAlternateDirectly) (\s@OracleSettings' {} a -> s {accessAlternateDirectly = a} :: OracleSettings)

-- | Set this attribute to @true@ in order to use the Binary Reader to
-- capture change data for an Amazon RDS for Oracle as the source. This
-- tells the DMS instance to use any specified prefix replacement to access
-- all online redo logs.
oracleSettings_useAlternateFolderForOnline :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_useAlternateFolderForOnline = Lens.lens (\OracleSettings' {useAlternateFolderForOnline} -> useAlternateFolderForOnline) (\s@OracleSettings' {} a -> s {useAlternateFolderForOnline = a} :: OracleSettings)

-- | Specifies the number scale. You can select a scale up to 38, or you can
-- select FLOAT. By default, the NUMBER data type is converted to precision
-- 38, scale 10.
--
-- Example: @numberDataTypeScale=12@
oracleSettings_numberDatatypeScale :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
oracleSettings_numberDatatypeScale = Lens.lens (\OracleSettings' {numberDatatypeScale} -> numberDatatypeScale) (\s@OracleSettings' {} a -> s {numberDatatypeScale = a} :: OracleSettings)

-- | Set this string attribute to the required value in order to use the
-- Binary Reader to capture change data for an Amazon RDS for Oracle as the
-- source. This value specifies the default Oracle root used to access the
-- redo logs.
oracleSettings_oraclePathPrefix :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_oraclePathPrefix = Lens.lens (\OracleSettings' {oraclePathPrefix} -> oraclePathPrefix) (\s@OracleSettings' {} a -> s {oraclePathPrefix = a} :: OracleSettings)

-- | For an Oracle source endpoint, the name of a key used for the
-- transparent data encryption (TDE) of the columns and tablespaces in an
-- Oracle source database that is encrypted using TDE. The key value is the
-- value of the @SecurityDbEncryption@ setting. For more information on
-- setting the key name value of @SecurityDbEncryptionName@, see the
-- information and example for setting the @securityDbEncryptionName@ extra
-- connection attribute in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
-- in the /AWS Database Migration Service User Guide/.
oracleSettings_securityDbEncryptionName :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_securityDbEncryptionName = Lens.lens (\OracleSettings' {securityDbEncryptionName} -> securityDbEncryptionName) (\s@OracleSettings' {} a -> s {securityDbEncryptionName = a} :: OracleSettings)

-- | Set this attribute with @archivedLogDestId@ in a primary\/ standby
-- setup. This attribute is useful in the case of a switchover. In this
-- case, AWS DMS needs to know which destination to get archive redo logs
-- from to read changes. This need arises because the previous primary
-- instance is now a standby instance after switchover.
oracleSettings_additionalArchivedLogDestId :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
oracleSettings_additionalArchivedLogDestId = Lens.lens (\OracleSettings' {additionalArchivedLogDestId} -> additionalArchivedLogDestId) (\s@OracleSettings' {} a -> s {additionalArchivedLogDestId = a} :: OracleSettings)

-- | For an Oracle source endpoint, your Oracle Automatic Storage Management
-- (ASM) password. You can set this value from the @ asm_user_password @
-- value. You set this value as part of the comma-separated value that you
-- set to the @Password@ request parameter when you create the endpoint to
-- access transaction logs using Binary Reader. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
oracleSettings_asmPassword :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_asmPassword = Lens.lens (\OracleSettings' {asmPassword} -> asmPassword) (\s@OracleSettings' {} a -> s {asmPassword = a} :: OracleSettings) Core.. Lens.mapping Core._Sensitive

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Oracle endpoint connection
-- details.
oracleSettings_secretsManagerSecretId :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_secretsManagerSecretId = Lens.lens (\OracleSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@OracleSettings' {} a -> s {secretsManagerSecretId = a} :: OracleSettings)

-- | When this field is set to @Y@, AWS DMS only accesses the archived redo
-- logs. If the archived redo logs are stored on Oracle ASM only, the AWS
-- DMS user account needs to be granted ASM privileges.
oracleSettings_archivedLogsOnly :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_archivedLogsOnly = Lens.lens (\OracleSettings' {archivedLogsOnly} -> archivedLogsOnly) (\s@OracleSettings' {} a -> s {archivedLogsOnly = a} :: OracleSettings)

-- | When set to @true@, this attribute specifies a parallel load when
-- @useDirectPathFullLoad@ is set to @Y@. This attribute also only applies
-- when you use the AWS DMS parallel load feature. Note that the target
-- table cannot have any constraints or indexes.
oracleSettings_directPathParallelLoad :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_directPathParallelLoad = Lens.lens (\OracleSettings' {directPathParallelLoad} -> directPathParallelLoad) (\s@OracleSettings' {} a -> s {directPathParallelLoad = a} :: OracleSettings)

-- | When set to @true@, this attribute helps to increase the commit rate on
-- the Oracle target database by writing directly to tables and not writing
-- a trail to database logs.
oracleSettings_directPathNoLog :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_directPathNoLog = Lens.lens (\OracleSettings' {directPathNoLog} -> directPathNoLog) (\s@OracleSettings' {} a -> s {directPathNoLog = a} :: OracleSettings)

-- | Fully qualified domain name of the endpoint.
oracleSettings_serverName :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_serverName = Lens.lens (\OracleSettings' {serverName} -> serverName) (\s@OracleSettings' {} a -> s {serverName = a} :: OracleSettings)

-- | For an Oracle source endpoint, your ASM server address. You can set this
-- value from the @asm_server@ value. You set @asm_server@ as part of the
-- extra connection attribute string to access an Oracle server with Binary
-- Reader that uses ASM. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
oracleSettings_asmServer :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_asmServer = Lens.lens (\OracleSettings' {asmServer} -> asmServer) (\s@OracleSettings' {} a -> s {asmServer = a} :: OracleSettings)

-- | For an Oracle source endpoint, the transparent data encryption (TDE)
-- password required by AWM DMS to access Oracle redo logs encrypted by TDE
-- using Binary Reader. It is also the @ TDE_Password @ part of the
-- comma-separated value you set to the @Password@ request parameter when
-- you create the endpoint. The @SecurityDbEncryptian@ setting is related
-- to this @SecurityDbEncryptionName@ setting. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
-- in the /AWS Database Migration Service User Guide/.
oracleSettings_securityDbEncryption :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_securityDbEncryption = Lens.lens (\OracleSettings' {securityDbEncryption} -> securityDbEncryption) (\s@OracleSettings' {} a -> s {securityDbEncryption = a} :: OracleSettings) Core.. Lens.mapping Core._Sensitive

-- | When set to @true@, this attribute supports tablespace replication.
oracleSettings_readTableSpaceName :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_readTableSpaceName = Lens.lens (\OracleSettings' {readTableSpaceName} -> readTableSpaceName) (\s@OracleSettings' {} a -> s {readTableSpaceName = a} :: OracleSettings)

-- | Endpoint connection password.
oracleSettings_password :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_password = Lens.lens (\OracleSettings' {password} -> password) (\s@OracleSettings' {} a -> s {password = a} :: OracleSettings) Core.. Lens.mapping Core._Sensitive

-- | Set this attribute to @true@ to enable replication of Oracle tables
-- containing columns that are nested tables or defined types.
oracleSettings_allowSelectNestedTables :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_allowSelectNestedTables = Lens.lens (\OracleSettings' {allowSelectNestedTables} -> allowSelectNestedTables) (\s@OracleSettings' {} a -> s {allowSelectNestedTables = a} :: OracleSettings)

-- | Specifies the destination of the archived redo logs. The value should be
-- the same as the DEST_ID number in the v$archived_log table. When working
-- with multiple log destinations (DEST_ID), we recommend that you to
-- specify an archived redo logs location identifier. Doing this improves
-- performance by ensuring that the correct logs are accessed from the
-- outset.
oracleSettings_archivedLogDestId :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
oracleSettings_archivedLogDestId = Lens.lens (\OracleSettings' {archivedLogDestId} -> archivedLogDestId) (\s@OracleSettings' {} a -> s {archivedLogDestId = a} :: OracleSettings)

-- | Set this attribute to true in order to use the Binary Reader to capture
-- change data for an Amazon RDS for Oracle as the source. This setting
-- tells DMS instance to replace the default Oracle root with the specified
-- @usePathPrefix@ setting to access the redo logs.
oracleSettings_replacePathPrefix :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_replacePathPrefix = Lens.lens (\OracleSettings' {replacePathPrefix} -> replacePathPrefix) (\s@OracleSettings' {} a -> s {replacePathPrefix = a} :: OracleSettings)

-- | Endpoint TCP port.
oracleSettings_port :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
oracleSettings_port = Lens.lens (\OracleSettings' {port} -> port) (\s@OracleSettings' {} a -> s {port = a} :: OracleSettings)

-- | Set this attribute to change the number of read-ahead blocks that DMS
-- configures to perform a Change Data Capture (CDC) load using Oracle
-- Automatic Storage Management (ASM). You can specify an integer value
-- between 1000 (the default) and 200,000 (the maximum).
oracleSettings_readAheadBlocks :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
oracleSettings_readAheadBlocks = Lens.lens (\OracleSettings' {readAheadBlocks} -> readAheadBlocks) (\s@OracleSettings' {} a -> s {readAheadBlocks = a} :: OracleSettings)

-- | Set this string attribute to the required value in order to use the
-- Binary Reader to capture change data for an Amazon RDS for Oracle as the
-- source. This value specifies the path prefix used to replace the default
-- Oracle root to access the redo logs.
oracleSettings_usePathPrefix :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_usePathPrefix = Lens.lens (\OracleSettings' {usePathPrefix} -> usePathPrefix) (\s@OracleSettings' {} a -> s {usePathPrefix = a} :: OracleSettings)

-- | For an Oracle source endpoint, your ASM user name. You can set this
-- value from the @asm_user@ value. You set @asm_user@ as part of the extra
-- connection attribute string to access an Oracle server with Binary
-- Reader that uses ASM. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
oracleSettings_asmUser :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_asmUser = Lens.lens (\OracleSettings' {asmUser} -> asmUser) (\s@OracleSettings' {} a -> s {asmUser = a} :: OracleSettings)

-- | Endpoint connection user name.
oracleSettings_username :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_username = Lens.lens (\OracleSettings' {username} -> username) (\s@OracleSettings' {} a -> s {username = a} :: OracleSettings)

-- | Set this attribute to enable homogenous tablespace replication and
-- create existing tables or indexes under the same tablespace on the
-- target.
oracleSettings_enableHomogenousTablespace :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_enableHomogenousTablespace = Lens.lens (\OracleSettings' {enableHomogenousTablespace} -> enableHomogenousTablespace) (\s@OracleSettings' {} a -> s {enableHomogenousTablespace = a} :: OracleSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies AWS
-- DMS as the trusted entity and grants the required permissions to access
-- the value in @SecretsManagerSecret@. @SecretsManagerSecret@ has the
-- value of the AWS Secrets Manager secret that allows access to the Oracle
-- endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
oracleSettings_secretsManagerAccessRoleArn :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_secretsManagerAccessRoleArn = Lens.lens (\OracleSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@OracleSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: OracleSettings)

-- | Set this attribute to change the number of threads that DMS configures
-- to perform a Change Data Capture (CDC) load using Oracle Automatic
-- Storage Management (ASM). You can specify an integer value between 2
-- (the default) and 8 (the maximum). Use this attribute together with the
-- @readAheadBlocks@ attribute.
oracleSettings_parallelAsmReadThreads :: Lens.Lens' OracleSettings (Core.Maybe Core.Int)
oracleSettings_parallelAsmReadThreads = Lens.lens (\OracleSettings' {parallelAsmReadThreads} -> parallelAsmReadThreads) (\s@OracleSettings' {} a -> s {parallelAsmReadThreads = a} :: OracleSettings)

-- | Specifies whether the length of a character column is in bytes or in
-- characters. To indicate that the character column length is in
-- characters, set this attribute to @CHAR@. Otherwise, the character
-- column length is in bytes.
--
-- Example: @charLengthSemantics=CHAR;@
oracleSettings_charLengthSemantics :: Lens.Lens' OracleSettings (Core.Maybe CharLengthSemantics)
oracleSettings_charLengthSemantics = Lens.lens (\OracleSettings' {charLengthSemantics} -> charLengthSemantics) (\s@OracleSettings' {} a -> s {charLengthSemantics = a} :: OracleSettings)

-- | Set this attribute to set up table-level supplemental logging for the
-- Oracle database. This attribute enables PRIMARY KEY supplemental logging
-- on all tables selected for a migration task.
--
-- If you use this option, you still need to enable database-level
-- supplemental logging.
oracleSettings_addSupplementalLogging :: Lens.Lens' OracleSettings (Core.Maybe Core.Bool)
oracleSettings_addSupplementalLogging = Lens.lens (\OracleSettings' {addSupplementalLogging} -> addSupplementalLogging) (\s@OracleSettings' {} a -> s {addSupplementalLogging = a} :: OracleSettings)

-- | Required only if your Oracle endpoint uses Advanced Storage Manager
-- (ASM). The full ARN of the IAM role that specifies AWS DMS as the
-- trusted entity and grants the required permissions to access the
-- @SecretsManagerOracleAsmSecret@. This @SecretsManagerOracleAsmSecret@
-- has the secret value that allows access to the Oracle ASM of the
-- endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and
-- @SecretsManagerOracleAsmSecretId@. Or you can specify clear-text values
-- for @AsmUserName@, @AsmPassword@, and @AsmServerName@. You can\'t
-- specify both. For more information on creating this
-- @SecretsManagerOracleAsmSecret@ and the
-- @SecretsManagerOracleAsmAccessRoleArn@ and
-- @SecretsManagerOracleAsmSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access AWS Database Migration Service resources>
-- in the /AWS Database Migration Service User Guide/.
oracleSettings_secretsManagerOracleAsmAccessRoleArn :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_secretsManagerOracleAsmAccessRoleArn = Lens.lens (\OracleSettings' {secretsManagerOracleAsmAccessRoleArn} -> secretsManagerOracleAsmAccessRoleArn) (\s@OracleSettings' {} a -> s {secretsManagerOracleAsmAccessRoleArn = a} :: OracleSettings)

-- | Database name for the endpoint.
oracleSettings_databaseName :: Lens.Lens' OracleSettings (Core.Maybe Core.Text)
oracleSettings_databaseName = Lens.lens (\OracleSettings' {databaseName} -> databaseName) (\s@OracleSettings' {} a -> s {databaseName = a} :: OracleSettings)

instance Core.FromJSON OracleSettings where
  parseJSON =
    Core.withObject
      "OracleSettings"
      ( \x ->
          OracleSettings'
            Core.<$> (x Core..:? "FailTasksOnLobTruncation")
            Core.<*> (x Core..:? "RetryInterval")
            Core.<*> (x Core..:? "SecretsManagerOracleAsmSecretId")
            Core.<*> (x Core..:? "AccessAlternateDirectly")
            Core.<*> (x Core..:? "UseAlternateFolderForOnline")
            Core.<*> (x Core..:? "NumberDatatypeScale")
            Core.<*> (x Core..:? "OraclePathPrefix")
            Core.<*> (x Core..:? "SecurityDbEncryptionName")
            Core.<*> (x Core..:? "AdditionalArchivedLogDestId")
            Core.<*> (x Core..:? "AsmPassword")
            Core.<*> (x Core..:? "SecretsManagerSecretId")
            Core.<*> (x Core..:? "ArchivedLogsOnly")
            Core.<*> (x Core..:? "DirectPathParallelLoad")
            Core.<*> (x Core..:? "DirectPathNoLog")
            Core.<*> (x Core..:? "ServerName")
            Core.<*> (x Core..:? "AsmServer")
            Core.<*> (x Core..:? "SecurityDbEncryption")
            Core.<*> (x Core..:? "ReadTableSpaceName")
            Core.<*> (x Core..:? "Password")
            Core.<*> (x Core..:? "AllowSelectNestedTables")
            Core.<*> (x Core..:? "ArchivedLogDestId")
            Core.<*> (x Core..:? "ReplacePathPrefix")
            Core.<*> (x Core..:? "Port")
            Core.<*> (x Core..:? "ReadAheadBlocks")
            Core.<*> (x Core..:? "UsePathPrefix")
            Core.<*> (x Core..:? "AsmUser")
            Core.<*> (x Core..:? "Username")
            Core.<*> (x Core..:? "EnableHomogenousTablespace")
            Core.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Core.<*> (x Core..:? "ParallelAsmReadThreads")
            Core.<*> (x Core..:? "CharLengthSemantics")
            Core.<*> (x Core..:? "AddSupplementalLogging")
            Core.<*> (x Core..:? "SecretsManagerOracleAsmAccessRoleArn")
            Core.<*> (x Core..:? "DatabaseName")
      )

instance Core.Hashable OracleSettings

instance Core.NFData OracleSettings

instance Core.ToJSON OracleSettings where
  toJSON OracleSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FailTasksOnLobTruncation" Core..=)
              Core.<$> failTasksOnLobTruncation,
            ("RetryInterval" Core..=) Core.<$> retryInterval,
            ("SecretsManagerOracleAsmSecretId" Core..=)
              Core.<$> secretsManagerOracleAsmSecretId,
            ("AccessAlternateDirectly" Core..=)
              Core.<$> accessAlternateDirectly,
            ("UseAlternateFolderForOnline" Core..=)
              Core.<$> useAlternateFolderForOnline,
            ("NumberDatatypeScale" Core..=)
              Core.<$> numberDatatypeScale,
            ("OraclePathPrefix" Core..=)
              Core.<$> oraclePathPrefix,
            ("SecurityDbEncryptionName" Core..=)
              Core.<$> securityDbEncryptionName,
            ("AdditionalArchivedLogDestId" Core..=)
              Core.<$> additionalArchivedLogDestId,
            ("AsmPassword" Core..=) Core.<$> asmPassword,
            ("SecretsManagerSecretId" Core..=)
              Core.<$> secretsManagerSecretId,
            ("ArchivedLogsOnly" Core..=)
              Core.<$> archivedLogsOnly,
            ("DirectPathParallelLoad" Core..=)
              Core.<$> directPathParallelLoad,
            ("DirectPathNoLog" Core..=) Core.<$> directPathNoLog,
            ("ServerName" Core..=) Core.<$> serverName,
            ("AsmServer" Core..=) Core.<$> asmServer,
            ("SecurityDbEncryption" Core..=)
              Core.<$> securityDbEncryption,
            ("ReadTableSpaceName" Core..=)
              Core.<$> readTableSpaceName,
            ("Password" Core..=) Core.<$> password,
            ("AllowSelectNestedTables" Core..=)
              Core.<$> allowSelectNestedTables,
            ("ArchivedLogDestId" Core..=)
              Core.<$> archivedLogDestId,
            ("ReplacePathPrefix" Core..=)
              Core.<$> replacePathPrefix,
            ("Port" Core..=) Core.<$> port,
            ("ReadAheadBlocks" Core..=) Core.<$> readAheadBlocks,
            ("UsePathPrefix" Core..=) Core.<$> usePathPrefix,
            ("AsmUser" Core..=) Core.<$> asmUser,
            ("Username" Core..=) Core.<$> username,
            ("EnableHomogenousTablespace" Core..=)
              Core.<$> enableHomogenousTablespace,
            ("SecretsManagerAccessRoleArn" Core..=)
              Core.<$> secretsManagerAccessRoleArn,
            ("ParallelAsmReadThreads" Core..=)
              Core.<$> parallelAsmReadThreads,
            ("CharLengthSemantics" Core..=)
              Core.<$> charLengthSemantics,
            ("AddSupplementalLogging" Core..=)
              Core.<$> addSupplementalLogging,
            ("SecretsManagerOracleAsmAccessRoleArn" Core..=)
              Core.<$> secretsManagerOracleAsmAccessRoleArn,
            ("DatabaseName" Core..=) Core.<$> databaseName
          ]
      )

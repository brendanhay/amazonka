{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DMS.Types.CharLengthSemantics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines an Oracle endpoint.
--
-- /See:/ 'newOracleSettings' smart constructor.
data OracleSettings = OracleSettings'
  { -- | When set to @true@, this attribute causes a task to fail if the actual
    -- size of an LOB column is greater than the specified @LobMaxSize@.
    --
    -- If a task is set to limited LOB mode and this option is set to @true@,
    -- the task fails instead of truncating the LOB data.
    failTasksOnLobTruncation :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the number of seconds that the system waits before resending a
    -- query.
    --
    -- Example: @retryInterval=6;@
    retryInterval :: Prelude.Maybe Prelude.Int,
    -- | Required only if your Oracle endpoint uses Advanced Storage Manager
    -- (ASM). The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerOracleAsmSecret@ that contains the Oracle ASM connection
    -- details for the Oracle endpoint.
    secretsManagerOracleAsmSecretId :: Prelude.Maybe Prelude.Text,
    -- | Set this attribute to @false@ in order to use the Binary Reader to
    -- capture change data for an Amazon RDS for Oracle as the source. This
    -- tells the DMS instance to not access redo logs through any specified
    -- path prefix replacement using direct file access.
    accessAlternateDirectly :: Prelude.Maybe Prelude.Bool,
    -- | Set this attribute to @true@ in order to use the Binary Reader to
    -- capture change data for an Amazon RDS for Oracle as the source. This
    -- tells the DMS instance to use any specified prefix replacement to access
    -- all online redo logs.
    useAlternateFolderForOnline :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the number scale. You can select a scale up to 38, or you can
    -- select FLOAT. By default, the NUMBER data type is converted to precision
    -- 38, scale 10.
    --
    -- Example: @numberDataTypeScale=12@
    numberDatatypeScale :: Prelude.Maybe Prelude.Int,
    -- | Set this string attribute to the required value in order to use the
    -- Binary Reader to capture change data for an Amazon RDS for Oracle as the
    -- source. This value specifies the default Oracle root used to access the
    -- redo logs.
    oraclePathPrefix :: Prelude.Maybe Prelude.Text,
    -- | For an Oracle source endpoint, the name of a key used for the
    -- transparent data encryption (TDE) of the columns and tablespaces in an
    -- Oracle source database that is encrypted using TDE. The key value is the
    -- value of the @SecurityDbEncryption@ setting. For more information on
    -- setting the key name value of @SecurityDbEncryptionName@, see the
    -- information and example for setting the @securityDbEncryptionName@ extra
    -- connection attribute in
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
    -- in the /AWS Database Migration Service User Guide/.
    securityDbEncryptionName :: Prelude.Maybe Prelude.Text,
    -- | Set this attribute with @archivedLogDestId@ in a primary\/ standby
    -- setup. This attribute is useful in the case of a switchover. In this
    -- case, AWS DMS needs to know which destination to get archive redo logs
    -- from to read changes. This need arises because the previous primary
    -- instance is now a standby instance after switchover.
    additionalArchivedLogDestId :: Prelude.Maybe Prelude.Int,
    -- | For an Oracle source endpoint, your Oracle Automatic Storage Management
    -- (ASM) password. You can set this value from the @ asm_user_password @
    -- value. You set this value as part of the comma-separated value that you
    -- set to the @Password@ request parameter when you create the endpoint to
    -- access transaction logs using Binary Reader. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
    asmPassword :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the Oracle endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | When this field is set to @Y@, AWS DMS only accesses the archived redo
    -- logs. If the archived redo logs are stored on Oracle ASM only, the AWS
    -- DMS user account needs to be granted ASM privileges.
    archivedLogsOnly :: Prelude.Maybe Prelude.Bool,
    -- | When set to @true@, this attribute specifies a parallel load when
    -- @useDirectPathFullLoad@ is set to @Y@. This attribute also only applies
    -- when you use the AWS DMS parallel load feature. Note that the target
    -- table cannot have any constraints or indexes.
    directPathParallelLoad :: Prelude.Maybe Prelude.Bool,
    -- | When set to @true@, this attribute helps to increase the commit rate on
    -- the Oracle target database by writing directly to tables and not writing
    -- a trail to database logs.
    directPathNoLog :: Prelude.Maybe Prelude.Bool,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | For an Oracle source endpoint, your ASM server address. You can set this
    -- value from the @asm_server@ value. You set @asm_server@ as part of the
    -- extra connection attribute string to access an Oracle server with Binary
    -- Reader that uses ASM. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
    asmServer :: Prelude.Maybe Prelude.Text,
    -- | For an Oracle source endpoint, the transparent data encryption (TDE)
    -- password required by AWM DMS to access Oracle redo logs encrypted by TDE
    -- using Binary Reader. It is also the @ TDE_Password @ part of the
    -- comma-separated value you set to the @Password@ request parameter when
    -- you create the endpoint. The @SecurityDbEncryptian@ setting is related
    -- to this @SecurityDbEncryptionName@ setting. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
    -- in the /AWS Database Migration Service User Guide/.
    securityDbEncryption :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | When set to @true@, this attribute supports tablespace replication.
    readTableSpaceName :: Prelude.Maybe Prelude.Bool,
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Set this attribute to @true@ to enable replication of Oracle tables
    -- containing columns that are nested tables or defined types.
    allowSelectNestedTables :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the destination of the archived redo logs. The value should be
    -- the same as the DEST_ID number in the v$archived_log table. When working
    -- with multiple log destinations (DEST_ID), we recommend that you to
    -- specify an archived redo logs location identifier. Doing this improves
    -- performance by ensuring that the correct logs are accessed from the
    -- outset.
    archivedLogDestId :: Prelude.Maybe Prelude.Int,
    -- | Set this attribute to true in order to use the Binary Reader to capture
    -- change data for an Amazon RDS for Oracle as the source. This setting
    -- tells DMS instance to replace the default Oracle root with the specified
    -- @usePathPrefix@ setting to access the redo logs.
    replacePathPrefix :: Prelude.Maybe Prelude.Bool,
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | Set this attribute to change the number of read-ahead blocks that DMS
    -- configures to perform a Change Data Capture (CDC) load using Oracle
    -- Automatic Storage Management (ASM). You can specify an integer value
    -- between 1000 (the default) and 200,000 (the maximum).
    readAheadBlocks :: Prelude.Maybe Prelude.Int,
    -- | Set this string attribute to the required value in order to use the
    -- Binary Reader to capture change data for an Amazon RDS for Oracle as the
    -- source. This value specifies the path prefix used to replace the default
    -- Oracle root to access the redo logs.
    usePathPrefix :: Prelude.Maybe Prelude.Text,
    -- | For an Oracle source endpoint, your ASM user name. You can set this
    -- value from the @asm_user@ value. You set @asm_user@ as part of the extra
    -- connection attribute string to access an Oracle server with Binary
    -- Reader that uses ASM. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
    asmUser :: Prelude.Maybe Prelude.Text,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | Set this attribute to enable homogenous tablespace replication and
    -- create existing tables or indexes under the same tablespace on the
    -- target.
    enableHomogenousTablespace :: Prelude.Maybe Prelude.Bool,
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
    secretsManagerAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Set this attribute to change the number of threads that DMS configures
    -- to perform a Change Data Capture (CDC) load using Oracle Automatic
    -- Storage Management (ASM). You can specify an integer value between 2
    -- (the default) and 8 (the maximum). Use this attribute together with the
    -- @readAheadBlocks@ attribute.
    parallelAsmReadThreads :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the length of a character column is in bytes or in
    -- characters. To indicate that the character column length is in
    -- characters, set this attribute to @CHAR@. Otherwise, the character
    -- column length is in bytes.
    --
    -- Example: @charLengthSemantics=CHAR;@
    charLengthSemantics :: Prelude.Maybe CharLengthSemantics,
    -- | Set this attribute to set up table-level supplemental logging for the
    -- Oracle database. This attribute enables PRIMARY KEY supplemental logging
    -- on all tables selected for a migration task.
    --
    -- If you use this option, you still need to enable database-level
    -- supplemental logging.
    addSupplementalLogging :: Prelude.Maybe Prelude.Bool,
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
    secretsManagerOracleAsmAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      retryInterval = Prelude.Nothing,
      secretsManagerOracleAsmSecretId = Prelude.Nothing,
      accessAlternateDirectly = Prelude.Nothing,
      useAlternateFolderForOnline = Prelude.Nothing,
      numberDatatypeScale = Prelude.Nothing,
      oraclePathPrefix = Prelude.Nothing,
      securityDbEncryptionName = Prelude.Nothing,
      additionalArchivedLogDestId = Prelude.Nothing,
      asmPassword = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      archivedLogsOnly = Prelude.Nothing,
      directPathParallelLoad = Prelude.Nothing,
      directPathNoLog = Prelude.Nothing,
      serverName = Prelude.Nothing,
      asmServer = Prelude.Nothing,
      securityDbEncryption = Prelude.Nothing,
      readTableSpaceName = Prelude.Nothing,
      password = Prelude.Nothing,
      allowSelectNestedTables = Prelude.Nothing,
      archivedLogDestId = Prelude.Nothing,
      replacePathPrefix = Prelude.Nothing,
      port = Prelude.Nothing,
      readAheadBlocks = Prelude.Nothing,
      usePathPrefix = Prelude.Nothing,
      asmUser = Prelude.Nothing,
      username = Prelude.Nothing,
      enableHomogenousTablespace = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      parallelAsmReadThreads = Prelude.Nothing,
      charLengthSemantics = Prelude.Nothing,
      addSupplementalLogging = Prelude.Nothing,
      secretsManagerOracleAsmAccessRoleArn =
        Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | When set to @true@, this attribute causes a task to fail if the actual
-- size of an LOB column is greater than the specified @LobMaxSize@.
--
-- If a task is set to limited LOB mode and this option is set to @true@,
-- the task fails instead of truncating the LOB data.
oracleSettings_failTasksOnLobTruncation :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_failTasksOnLobTruncation = Lens.lens (\OracleSettings' {failTasksOnLobTruncation} -> failTasksOnLobTruncation) (\s@OracleSettings' {} a -> s {failTasksOnLobTruncation = a} :: OracleSettings)

-- | Specifies the number of seconds that the system waits before resending a
-- query.
--
-- Example: @retryInterval=6;@
oracleSettings_retryInterval :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Int)
oracleSettings_retryInterval = Lens.lens (\OracleSettings' {retryInterval} -> retryInterval) (\s@OracleSettings' {} a -> s {retryInterval = a} :: OracleSettings)

-- | Required only if your Oracle endpoint uses Advanced Storage Manager
-- (ASM). The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerOracleAsmSecret@ that contains the Oracle ASM connection
-- details for the Oracle endpoint.
oracleSettings_secretsManagerOracleAsmSecretId :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_secretsManagerOracleAsmSecretId = Lens.lens (\OracleSettings' {secretsManagerOracleAsmSecretId} -> secretsManagerOracleAsmSecretId) (\s@OracleSettings' {} a -> s {secretsManagerOracleAsmSecretId = a} :: OracleSettings)

-- | Set this attribute to @false@ in order to use the Binary Reader to
-- capture change data for an Amazon RDS for Oracle as the source. This
-- tells the DMS instance to not access redo logs through any specified
-- path prefix replacement using direct file access.
oracleSettings_accessAlternateDirectly :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_accessAlternateDirectly = Lens.lens (\OracleSettings' {accessAlternateDirectly} -> accessAlternateDirectly) (\s@OracleSettings' {} a -> s {accessAlternateDirectly = a} :: OracleSettings)

-- | Set this attribute to @true@ in order to use the Binary Reader to
-- capture change data for an Amazon RDS for Oracle as the source. This
-- tells the DMS instance to use any specified prefix replacement to access
-- all online redo logs.
oracleSettings_useAlternateFolderForOnline :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_useAlternateFolderForOnline = Lens.lens (\OracleSettings' {useAlternateFolderForOnline} -> useAlternateFolderForOnline) (\s@OracleSettings' {} a -> s {useAlternateFolderForOnline = a} :: OracleSettings)

-- | Specifies the number scale. You can select a scale up to 38, or you can
-- select FLOAT. By default, the NUMBER data type is converted to precision
-- 38, scale 10.
--
-- Example: @numberDataTypeScale=12@
oracleSettings_numberDatatypeScale :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Int)
oracleSettings_numberDatatypeScale = Lens.lens (\OracleSettings' {numberDatatypeScale} -> numberDatatypeScale) (\s@OracleSettings' {} a -> s {numberDatatypeScale = a} :: OracleSettings)

-- | Set this string attribute to the required value in order to use the
-- Binary Reader to capture change data for an Amazon RDS for Oracle as the
-- source. This value specifies the default Oracle root used to access the
-- redo logs.
oracleSettings_oraclePathPrefix :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
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
oracleSettings_securityDbEncryptionName :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_securityDbEncryptionName = Lens.lens (\OracleSettings' {securityDbEncryptionName} -> securityDbEncryptionName) (\s@OracleSettings' {} a -> s {securityDbEncryptionName = a} :: OracleSettings)

-- | Set this attribute with @archivedLogDestId@ in a primary\/ standby
-- setup. This attribute is useful in the case of a switchover. In this
-- case, AWS DMS needs to know which destination to get archive redo logs
-- from to read changes. This need arises because the previous primary
-- instance is now a standby instance after switchover.
oracleSettings_additionalArchivedLogDestId :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Int)
oracleSettings_additionalArchivedLogDestId = Lens.lens (\OracleSettings' {additionalArchivedLogDestId} -> additionalArchivedLogDestId) (\s@OracleSettings' {} a -> s {additionalArchivedLogDestId = a} :: OracleSettings)

-- | For an Oracle source endpoint, your Oracle Automatic Storage Management
-- (ASM) password. You can set this value from the @ asm_user_password @
-- value. You set this value as part of the comma-separated value that you
-- set to the @Password@ request parameter when you create the endpoint to
-- access transaction logs using Binary Reader. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
oracleSettings_asmPassword :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_asmPassword = Lens.lens (\OracleSettings' {asmPassword} -> asmPassword) (\s@OracleSettings' {} a -> s {asmPassword = a} :: OracleSettings) Prelude.. Lens.mapping Prelude._Sensitive

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Oracle endpoint connection
-- details.
oracleSettings_secretsManagerSecretId :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_secretsManagerSecretId = Lens.lens (\OracleSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@OracleSettings' {} a -> s {secretsManagerSecretId = a} :: OracleSettings)

-- | When this field is set to @Y@, AWS DMS only accesses the archived redo
-- logs. If the archived redo logs are stored on Oracle ASM only, the AWS
-- DMS user account needs to be granted ASM privileges.
oracleSettings_archivedLogsOnly :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_archivedLogsOnly = Lens.lens (\OracleSettings' {archivedLogsOnly} -> archivedLogsOnly) (\s@OracleSettings' {} a -> s {archivedLogsOnly = a} :: OracleSettings)

-- | When set to @true@, this attribute specifies a parallel load when
-- @useDirectPathFullLoad@ is set to @Y@. This attribute also only applies
-- when you use the AWS DMS parallel load feature. Note that the target
-- table cannot have any constraints or indexes.
oracleSettings_directPathParallelLoad :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_directPathParallelLoad = Lens.lens (\OracleSettings' {directPathParallelLoad} -> directPathParallelLoad) (\s@OracleSettings' {} a -> s {directPathParallelLoad = a} :: OracleSettings)

-- | When set to @true@, this attribute helps to increase the commit rate on
-- the Oracle target database by writing directly to tables and not writing
-- a trail to database logs.
oracleSettings_directPathNoLog :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_directPathNoLog = Lens.lens (\OracleSettings' {directPathNoLog} -> directPathNoLog) (\s@OracleSettings' {} a -> s {directPathNoLog = a} :: OracleSettings)

-- | Fully qualified domain name of the endpoint.
oracleSettings_serverName :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_serverName = Lens.lens (\OracleSettings' {serverName} -> serverName) (\s@OracleSettings' {} a -> s {serverName = a} :: OracleSettings)

-- | For an Oracle source endpoint, your ASM server address. You can set this
-- value from the @asm_server@ value. You set @asm_server@ as part of the
-- extra connection attribute string to access an Oracle server with Binary
-- Reader that uses ASM. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
oracleSettings_asmServer :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_asmServer = Lens.lens (\OracleSettings' {asmServer} -> asmServer) (\s@OracleSettings' {} a -> s {asmServer = a} :: OracleSettings)

-- | For an Oracle source endpoint, the transparent data encryption (TDE)
-- password required by AWM DMS to access Oracle redo logs encrypted by TDE
-- using Binary Reader. It is also the @ TDE_Password @ part of the
-- comma-separated value you set to the @Password@ request parameter when
-- you create the endpoint. The @SecurityDbEncryptian@ setting is related
-- to this @SecurityDbEncryptionName@ setting. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.Encryption Supported encryption methods for using Oracle as a source for AWS DMS>
-- in the /AWS Database Migration Service User Guide/.
oracleSettings_securityDbEncryption :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_securityDbEncryption = Lens.lens (\OracleSettings' {securityDbEncryption} -> securityDbEncryption) (\s@OracleSettings' {} a -> s {securityDbEncryption = a} :: OracleSettings) Prelude.. Lens.mapping Prelude._Sensitive

-- | When set to @true@, this attribute supports tablespace replication.
oracleSettings_readTableSpaceName :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_readTableSpaceName = Lens.lens (\OracleSettings' {readTableSpaceName} -> readTableSpaceName) (\s@OracleSettings' {} a -> s {readTableSpaceName = a} :: OracleSettings)

-- | Endpoint connection password.
oracleSettings_password :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_password = Lens.lens (\OracleSettings' {password} -> password) (\s@OracleSettings' {} a -> s {password = a} :: OracleSettings) Prelude.. Lens.mapping Prelude._Sensitive

-- | Set this attribute to @true@ to enable replication of Oracle tables
-- containing columns that are nested tables or defined types.
oracleSettings_allowSelectNestedTables :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_allowSelectNestedTables = Lens.lens (\OracleSettings' {allowSelectNestedTables} -> allowSelectNestedTables) (\s@OracleSettings' {} a -> s {allowSelectNestedTables = a} :: OracleSettings)

-- | Specifies the destination of the archived redo logs. The value should be
-- the same as the DEST_ID number in the v$archived_log table. When working
-- with multiple log destinations (DEST_ID), we recommend that you to
-- specify an archived redo logs location identifier. Doing this improves
-- performance by ensuring that the correct logs are accessed from the
-- outset.
oracleSettings_archivedLogDestId :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Int)
oracleSettings_archivedLogDestId = Lens.lens (\OracleSettings' {archivedLogDestId} -> archivedLogDestId) (\s@OracleSettings' {} a -> s {archivedLogDestId = a} :: OracleSettings)

-- | Set this attribute to true in order to use the Binary Reader to capture
-- change data for an Amazon RDS for Oracle as the source. This setting
-- tells DMS instance to replace the default Oracle root with the specified
-- @usePathPrefix@ setting to access the redo logs.
oracleSettings_replacePathPrefix :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
oracleSettings_replacePathPrefix = Lens.lens (\OracleSettings' {replacePathPrefix} -> replacePathPrefix) (\s@OracleSettings' {} a -> s {replacePathPrefix = a} :: OracleSettings)

-- | Endpoint TCP port.
oracleSettings_port :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Int)
oracleSettings_port = Lens.lens (\OracleSettings' {port} -> port) (\s@OracleSettings' {} a -> s {port = a} :: OracleSettings)

-- | Set this attribute to change the number of read-ahead blocks that DMS
-- configures to perform a Change Data Capture (CDC) load using Oracle
-- Automatic Storage Management (ASM). You can specify an integer value
-- between 1000 (the default) and 200,000 (the maximum).
oracleSettings_readAheadBlocks :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Int)
oracleSettings_readAheadBlocks = Lens.lens (\OracleSettings' {readAheadBlocks} -> readAheadBlocks) (\s@OracleSettings' {} a -> s {readAheadBlocks = a} :: OracleSettings)

-- | Set this string attribute to the required value in order to use the
-- Binary Reader to capture change data for an Amazon RDS for Oracle as the
-- source. This value specifies the path prefix used to replace the default
-- Oracle root to access the redo logs.
oracleSettings_usePathPrefix :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_usePathPrefix = Lens.lens (\OracleSettings' {usePathPrefix} -> usePathPrefix) (\s@OracleSettings' {} a -> s {usePathPrefix = a} :: OracleSettings)

-- | For an Oracle source endpoint, your ASM user name. You can set this
-- value from the @asm_user@ value. You set @asm_user@ as part of the extra
-- connection attribute string to access an Oracle server with Binary
-- Reader that uses ASM. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.Oracle.html#dms/latest/userguide/CHAP_Source.Oracle.html#CHAP_Source.Oracle.CDC.Configuration Configuration for change data capture (CDC) on an Oracle source database>.
oracleSettings_asmUser :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_asmUser = Lens.lens (\OracleSettings' {asmUser} -> asmUser) (\s@OracleSettings' {} a -> s {asmUser = a} :: OracleSettings)

-- | Endpoint connection user name.
oracleSettings_username :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_username = Lens.lens (\OracleSettings' {username} -> username) (\s@OracleSettings' {} a -> s {username = a} :: OracleSettings)

-- | Set this attribute to enable homogenous tablespace replication and
-- create existing tables or indexes under the same tablespace on the
-- target.
oracleSettings_enableHomogenousTablespace :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
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
oracleSettings_secretsManagerAccessRoleArn :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_secretsManagerAccessRoleArn = Lens.lens (\OracleSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@OracleSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: OracleSettings)

-- | Set this attribute to change the number of threads that DMS configures
-- to perform a Change Data Capture (CDC) load using Oracle Automatic
-- Storage Management (ASM). You can specify an integer value between 2
-- (the default) and 8 (the maximum). Use this attribute together with the
-- @readAheadBlocks@ attribute.
oracleSettings_parallelAsmReadThreads :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Int)
oracleSettings_parallelAsmReadThreads = Lens.lens (\OracleSettings' {parallelAsmReadThreads} -> parallelAsmReadThreads) (\s@OracleSettings' {} a -> s {parallelAsmReadThreads = a} :: OracleSettings)

-- | Specifies whether the length of a character column is in bytes or in
-- characters. To indicate that the character column length is in
-- characters, set this attribute to @CHAR@. Otherwise, the character
-- column length is in bytes.
--
-- Example: @charLengthSemantics=CHAR;@
oracleSettings_charLengthSemantics :: Lens.Lens' OracleSettings (Prelude.Maybe CharLengthSemantics)
oracleSettings_charLengthSemantics = Lens.lens (\OracleSettings' {charLengthSemantics} -> charLengthSemantics) (\s@OracleSettings' {} a -> s {charLengthSemantics = a} :: OracleSettings)

-- | Set this attribute to set up table-level supplemental logging for the
-- Oracle database. This attribute enables PRIMARY KEY supplemental logging
-- on all tables selected for a migration task.
--
-- If you use this option, you still need to enable database-level
-- supplemental logging.
oracleSettings_addSupplementalLogging :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Bool)
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
oracleSettings_secretsManagerOracleAsmAccessRoleArn :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_secretsManagerOracleAsmAccessRoleArn = Lens.lens (\OracleSettings' {secretsManagerOracleAsmAccessRoleArn} -> secretsManagerOracleAsmAccessRoleArn) (\s@OracleSettings' {} a -> s {secretsManagerOracleAsmAccessRoleArn = a} :: OracleSettings)

-- | Database name for the endpoint.
oracleSettings_databaseName :: Lens.Lens' OracleSettings (Prelude.Maybe Prelude.Text)
oracleSettings_databaseName = Lens.lens (\OracleSettings' {databaseName} -> databaseName) (\s@OracleSettings' {} a -> s {databaseName = a} :: OracleSettings)

instance Prelude.FromJSON OracleSettings where
  parseJSON =
    Prelude.withObject
      "OracleSettings"
      ( \x ->
          OracleSettings'
            Prelude.<$> (x Prelude..:? "FailTasksOnLobTruncation")
            Prelude.<*> (x Prelude..:? "RetryInterval")
            Prelude.<*> (x Prelude..:? "SecretsManagerOracleAsmSecretId")
            Prelude.<*> (x Prelude..:? "AccessAlternateDirectly")
            Prelude.<*> (x Prelude..:? "UseAlternateFolderForOnline")
            Prelude.<*> (x Prelude..:? "NumberDatatypeScale")
            Prelude.<*> (x Prelude..:? "OraclePathPrefix")
            Prelude.<*> (x Prelude..:? "SecurityDbEncryptionName")
            Prelude.<*> (x Prelude..:? "AdditionalArchivedLogDestId")
            Prelude.<*> (x Prelude..:? "AsmPassword")
            Prelude.<*> (x Prelude..:? "SecretsManagerSecretId")
            Prelude.<*> (x Prelude..:? "ArchivedLogsOnly")
            Prelude.<*> (x Prelude..:? "DirectPathParallelLoad")
            Prelude.<*> (x Prelude..:? "DirectPathNoLog")
            Prelude.<*> (x Prelude..:? "ServerName")
            Prelude.<*> (x Prelude..:? "AsmServer")
            Prelude.<*> (x Prelude..:? "SecurityDbEncryption")
            Prelude.<*> (x Prelude..:? "ReadTableSpaceName")
            Prelude.<*> (x Prelude..:? "Password")
            Prelude.<*> (x Prelude..:? "AllowSelectNestedTables")
            Prelude.<*> (x Prelude..:? "ArchivedLogDestId")
            Prelude.<*> (x Prelude..:? "ReplacePathPrefix")
            Prelude.<*> (x Prelude..:? "Port")
            Prelude.<*> (x Prelude..:? "ReadAheadBlocks")
            Prelude.<*> (x Prelude..:? "UsePathPrefix")
            Prelude.<*> (x Prelude..:? "AsmUser")
            Prelude.<*> (x Prelude..:? "Username")
            Prelude.<*> (x Prelude..:? "EnableHomogenousTablespace")
            Prelude.<*> (x Prelude..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Prelude..:? "ParallelAsmReadThreads")
            Prelude.<*> (x Prelude..:? "CharLengthSemantics")
            Prelude.<*> (x Prelude..:? "AddSupplementalLogging")
            Prelude.<*> ( x
                            Prelude..:? "SecretsManagerOracleAsmAccessRoleArn"
                        )
            Prelude.<*> (x Prelude..:? "DatabaseName")
      )

instance Prelude.Hashable OracleSettings

instance Prelude.NFData OracleSettings

instance Prelude.ToJSON OracleSettings where
  toJSON OracleSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FailTasksOnLobTruncation" Prelude..=)
              Prelude.<$> failTasksOnLobTruncation,
            ("RetryInterval" Prelude..=)
              Prelude.<$> retryInterval,
            ("SecretsManagerOracleAsmSecretId" Prelude..=)
              Prelude.<$> secretsManagerOracleAsmSecretId,
            ("AccessAlternateDirectly" Prelude..=)
              Prelude.<$> accessAlternateDirectly,
            ("UseAlternateFolderForOnline" Prelude..=)
              Prelude.<$> useAlternateFolderForOnline,
            ("NumberDatatypeScale" Prelude..=)
              Prelude.<$> numberDatatypeScale,
            ("OraclePathPrefix" Prelude..=)
              Prelude.<$> oraclePathPrefix,
            ("SecurityDbEncryptionName" Prelude..=)
              Prelude.<$> securityDbEncryptionName,
            ("AdditionalArchivedLogDestId" Prelude..=)
              Prelude.<$> additionalArchivedLogDestId,
            ("AsmPassword" Prelude..=) Prelude.<$> asmPassword,
            ("SecretsManagerSecretId" Prelude..=)
              Prelude.<$> secretsManagerSecretId,
            ("ArchivedLogsOnly" Prelude..=)
              Prelude.<$> archivedLogsOnly,
            ("DirectPathParallelLoad" Prelude..=)
              Prelude.<$> directPathParallelLoad,
            ("DirectPathNoLog" Prelude..=)
              Prelude.<$> directPathNoLog,
            ("ServerName" Prelude..=) Prelude.<$> serverName,
            ("AsmServer" Prelude..=) Prelude.<$> asmServer,
            ("SecurityDbEncryption" Prelude..=)
              Prelude.<$> securityDbEncryption,
            ("ReadTableSpaceName" Prelude..=)
              Prelude.<$> readTableSpaceName,
            ("Password" Prelude..=) Prelude.<$> password,
            ("AllowSelectNestedTables" Prelude..=)
              Prelude.<$> allowSelectNestedTables,
            ("ArchivedLogDestId" Prelude..=)
              Prelude.<$> archivedLogDestId,
            ("ReplacePathPrefix" Prelude..=)
              Prelude.<$> replacePathPrefix,
            ("Port" Prelude..=) Prelude.<$> port,
            ("ReadAheadBlocks" Prelude..=)
              Prelude.<$> readAheadBlocks,
            ("UsePathPrefix" Prelude..=)
              Prelude.<$> usePathPrefix,
            ("AsmUser" Prelude..=) Prelude.<$> asmUser,
            ("Username" Prelude..=) Prelude.<$> username,
            ("EnableHomogenousTablespace" Prelude..=)
              Prelude.<$> enableHomogenousTablespace,
            ("SecretsManagerAccessRoleArn" Prelude..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("ParallelAsmReadThreads" Prelude..=)
              Prelude.<$> parallelAsmReadThreads,
            ("CharLengthSemantics" Prelude..=)
              Prelude.<$> charLengthSemantics,
            ("AddSupplementalLogging" Prelude..=)
              Prelude.<$> addSupplementalLogging,
            ("SecretsManagerOracleAsmAccessRoleArn" Prelude..=)
              Prelude.<$> secretsManagerOracleAsmAccessRoleArn,
            ("DatabaseName" Prelude..=)
              Prelude.<$> databaseName
          ]
      )

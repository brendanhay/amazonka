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
-- Module      : Amazonka.DMS.Types.RedshiftSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RedshiftSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.EncryptionModeValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines an Amazon Redshift endpoint.
--
-- /See:/ 'newRedshiftSettings' smart constructor.
data RedshiftSettings = RedshiftSettings'
  { -- | A value that specifies whether DMS should migrate empty CHAR and VARCHAR
    -- fields as NULL. A value of @true@ sets empty CHAR and VARCHAR fields to
    -- null. The default is @false@.
    emptyAsNull :: Prelude.Maybe Prelude.Bool,
    -- | The port number for Amazon Redshift. The default value is 5439.
    port :: Prelude.Maybe Prelude.Int,
    -- | A value that specifies to truncate data in columns to the appropriate
    -- number of characters, so that the data fits in the column. This
    -- parameter applies only to columns with a VARCHAR or CHAR data type, and
    -- rows with a size of 4 MB or less. Choose @true@ to truncate data. The
    -- default is @false@.
    truncateColumns :: Prelude.Maybe Prelude.Bool,
    -- | The maximum size (in KB) of any .csv file used to load data on an S3
    -- bucket and transfer data to Amazon Redshift. It defaults to 1048576KB (1
    -- GB).
    maxFileSize :: Prelude.Maybe Prelude.Int,
    -- | A value that specifies to replaces the invalid characters specified in
    -- @ReplaceInvalidChars@, substituting the specified characters instead.
    -- The default is @\"?\"@.
    replaceChars :: Prelude.Maybe Prelude.Text,
    -- | The time format that you want to use. Valid values are @auto@
    -- (case-sensitive), @\'timeformat_string\'@, @\'epochsecs\'@, or
    -- @\'epochmillisecs\'@. It defaults to 10. Using @auto@ recognizes most
    -- strings, even some that aren\'t supported when you use a time format
    -- string.
    --
    -- If your date and time values use formats different from each other, set
    -- this parameter to @auto@.
    timeFormat :: Prelude.Maybe Prelude.Text,
    -- | The amount of time to wait (in milliseconds) before timing out of
    -- operations performed by DMS on a Redshift cluster, such as Redshift
    -- COPY, INSERT, DELETE, and UPDATE.
    loadTimeout :: Prelude.Maybe Prelude.Int,
    -- | An S3 folder where the comma-separated-value (.csv) files are stored
    -- before being uploaded to the target Redshift cluster.
    --
    -- For full load mode, DMS converts source records into .csv files and
    -- loads them to the /BucketFolder\/TableID/ path. DMS uses the Redshift
    -- @COPY@ command to upload the .csv files to the target table. The files
    -- are deleted once the @COPY@ operation has finished. For more
    -- information, see
    -- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html COPY> in the
    -- /Amazon Redshift Database Developer Guide/.
    --
    -- For change-data-capture (CDC) mode, DMS creates a /NetChanges/ table,
    -- and loads the .csv files to this /BucketFolder\/NetChangesTableID/ path.
    bucketFolder :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the Amazon Redshift
    -- endpoint.
    --
    -- You can specify one of two sets of values for these permissions. You can
    -- specify the values for this setting and @SecretsManagerSecretId@. Or you
    -- can specify clear-text values for @UserName@, @Password@, @ServerName@,
    -- and @Port@. You can\'t specify both. For more information on creating
    -- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
    -- @SecretsManagerSecretId@ required to access it, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
    -- in the /Database Migration Service User Guide/.
    secretsManagerAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A value that sets the amount of time to wait (in milliseconds) before
    -- timing out, beginning from when you initially establish a connection.
    connectionTimeout :: Prelude.Maybe Prelude.Int,
    -- | The password for the user named in the @username@ property.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the Amazon Redshift cluster you are using.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | If Amazon Redshift is configured to support case sensitive schema names,
    -- set @CaseSensitiveNames@ to @true@. The default is @false@.
    caseSensitiveNames :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Amazon Redshift data warehouse (service) that you are
    -- working with.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Redshift user name for a registered user.
    username :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies to remove surrounding quotation marks from
    -- strings in the incoming data. All characters within the quotation marks,
    -- including delimiters, are retained. Choose @true@ to remove quotation
    -- marks. The default is @false@.
    removeQuotes :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM role that has access to the
    -- Amazon Redshift service. The role must allow the @iam:PassRole@ action.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | If you set @CompUpdate@ to @true@ Amazon Redshift applies automatic
    -- compression if the table is empty. This applies even if the table
    -- columns already have encodings other than @RAW@. If you set @CompUpdate@
    -- to @false@, automatic compression is disabled and existing column
    -- encodings aren\'t changed. The default is @true@.
    compUpdate :: Prelude.Maybe Prelude.Bool,
    -- | A list of characters that you want to replace. Use with @ReplaceChars@.
    replaceInvalidChars :: Prelude.Maybe Prelude.Text,
    -- | Code to run after connecting. This parameter should contain the code
    -- itself, not the name of a file containing the code.
    afterConnectScript :: Prelude.Maybe Prelude.Text,
    -- | The number of threads used to upload a single file. This parameter
    -- accepts a value from 1 through 64. It defaults to 10.
    --
    -- The number of parallel streams used to upload a single .csv file to an
    -- S3 bucket using S3 Multipart Upload. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart upload overview>.
    --
    -- @FileTransferUploadStreams@ accepts a value from 1 through 64. It
    -- defaults to 10.
    fileTransferUploadStreams :: Prelude.Maybe Prelude.Int,
    -- | The name of the intermediate S3 bucket used to store .csv files before
    -- uploading data to Redshift.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the Amazon Redshift endpoint
    -- connection details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | This setting is only valid for a full-load migration task. Set
    -- @ExplicitIds@ to @true@ to have tables with @IDENTITY@ columns override
    -- their auto-generated values with explicit values loaded from the source
    -- data files used to populate the tables. The default is @false@.
    explicitIds :: Prelude.Maybe Prelude.Bool,
    -- | The KMS key ID. If you are using @SSE_KMS@ for the @EncryptionMode@,
    -- provide this key ID. The key that you use needs an attached policy that
    -- enables IAM user permissions and allows use of the key.
    serverSideEncryptionKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The date format that you are using. Valid values are @auto@
    -- (case-sensitive), your date format string enclosed in quotes, or NULL.
    -- If this parameter is left unset (NULL), it defaults to a format of
    -- \'YYYY-MM-DD\'. Using @auto@ recognizes most strings, even some that
    -- aren\'t supported when you use a date format string.
    --
    -- If your date and time values use formats different from each other, set
    -- this to @auto@.
    dateFormat :: Prelude.Maybe Prelude.Text,
    -- | The size (in KB) of the in-memory file write buffer used when generating
    -- .csv files on the local disk at the DMS replication instance. The
    -- default value is 1000 (buffer size is 1000KB).
    writeBufferSize :: Prelude.Maybe Prelude.Int,
    -- | A value that specifies to remove the trailing white space characters
    -- from a VARCHAR string. This parameter applies only to columns with a
    -- VARCHAR data type. Choose @true@ to remove unneeded white space. The
    -- default is @false@.
    trimBlanks :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates to allow any date format, including invalid
    -- formats such as 00\/00\/00 00:00:00, to be loaded without generating an
    -- error. You can choose @true@ or @false@ (the default).
    --
    -- This parameter applies only to TIMESTAMP and DATE columns. Always use
    -- ACCEPTANYDATE with the DATEFORMAT parameter. If the date format for the
    -- data doesn\'t match the DATEFORMAT specification, Amazon Redshift
    -- inserts a NULL value into that field.
    acceptAnyDate :: Prelude.Maybe Prelude.Bool,
    -- | The type of server-side encryption that you want to use for your data.
    -- This encryption type is part of the endpoint settings or the extra
    -- connections attributes for Amazon S3. You can choose either @SSE_S3@
    -- (the default) or @SSE_KMS@.
    --
    -- For the @ModifyEndpoint@ operation, you can change the existing value of
    -- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
    -- change the existing value from @SSE_S3@ to @SSE_KMS@.
    --
    -- To use @SSE_S3@, create an Identity and Access Management (IAM) role
    -- with a policy that allows @\"arn:aws:s3:::*\"@ to use the following
    -- actions: @\"s3:PutObject\", \"s3:ListBucket\"@
    encryptionMode :: Prelude.Maybe EncryptionModeValue
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emptyAsNull', 'redshiftSettings_emptyAsNull' - A value that specifies whether DMS should migrate empty CHAR and VARCHAR
-- fields as NULL. A value of @true@ sets empty CHAR and VARCHAR fields to
-- null. The default is @false@.
--
-- 'port', 'redshiftSettings_port' - The port number for Amazon Redshift. The default value is 5439.
--
-- 'truncateColumns', 'redshiftSettings_truncateColumns' - A value that specifies to truncate data in columns to the appropriate
-- number of characters, so that the data fits in the column. This
-- parameter applies only to columns with a VARCHAR or CHAR data type, and
-- rows with a size of 4 MB or less. Choose @true@ to truncate data. The
-- default is @false@.
--
-- 'maxFileSize', 'redshiftSettings_maxFileSize' - The maximum size (in KB) of any .csv file used to load data on an S3
-- bucket and transfer data to Amazon Redshift. It defaults to 1048576KB (1
-- GB).
--
-- 'replaceChars', 'redshiftSettings_replaceChars' - A value that specifies to replaces the invalid characters specified in
-- @ReplaceInvalidChars@, substituting the specified characters instead.
-- The default is @\"?\"@.
--
-- 'timeFormat', 'redshiftSettings_timeFormat' - The time format that you want to use. Valid values are @auto@
-- (case-sensitive), @\'timeformat_string\'@, @\'epochsecs\'@, or
-- @\'epochmillisecs\'@. It defaults to 10. Using @auto@ recognizes most
-- strings, even some that aren\'t supported when you use a time format
-- string.
--
-- If your date and time values use formats different from each other, set
-- this parameter to @auto@.
--
-- 'loadTimeout', 'redshiftSettings_loadTimeout' - The amount of time to wait (in milliseconds) before timing out of
-- operations performed by DMS on a Redshift cluster, such as Redshift
-- COPY, INSERT, DELETE, and UPDATE.
--
-- 'bucketFolder', 'redshiftSettings_bucketFolder' - An S3 folder where the comma-separated-value (.csv) files are stored
-- before being uploaded to the target Redshift cluster.
--
-- For full load mode, DMS converts source records into .csv files and
-- loads them to the /BucketFolder\/TableID/ path. DMS uses the Redshift
-- @COPY@ command to upload the .csv files to the target table. The files
-- are deleted once the @COPY@ operation has finished. For more
-- information, see
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html COPY> in the
-- /Amazon Redshift Database Developer Guide/.
--
-- For change-data-capture (CDC) mode, DMS creates a /NetChanges/ table,
-- and loads the .csv files to this /BucketFolder\/NetChangesTableID/ path.
--
-- 'secretsManagerAccessRoleArn', 'redshiftSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the Amazon Redshift
-- endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
--
-- 'connectionTimeout', 'redshiftSettings_connectionTimeout' - A value that sets the amount of time to wait (in milliseconds) before
-- timing out, beginning from when you initially establish a connection.
--
-- 'password', 'redshiftSettings_password' - The password for the user named in the @username@ property.
--
-- 'serverName', 'redshiftSettings_serverName' - The name of the Amazon Redshift cluster you are using.
--
-- 'caseSensitiveNames', 'redshiftSettings_caseSensitiveNames' - If Amazon Redshift is configured to support case sensitive schema names,
-- set @CaseSensitiveNames@ to @true@. The default is @false@.
--
-- 'databaseName', 'redshiftSettings_databaseName' - The name of the Amazon Redshift data warehouse (service) that you are
-- working with.
--
-- 'username', 'redshiftSettings_username' - An Amazon Redshift user name for a registered user.
--
-- 'removeQuotes', 'redshiftSettings_removeQuotes' - A value that specifies to remove surrounding quotation marks from
-- strings in the incoming data. All characters within the quotation marks,
-- including delimiters, are retained. Choose @true@ to remove quotation
-- marks. The default is @false@.
--
-- 'serviceAccessRoleArn', 'redshiftSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) of the IAM role that has access to the
-- Amazon Redshift service. The role must allow the @iam:PassRole@ action.
--
-- 'compUpdate', 'redshiftSettings_compUpdate' - If you set @CompUpdate@ to @true@ Amazon Redshift applies automatic
-- compression if the table is empty. This applies even if the table
-- columns already have encodings other than @RAW@. If you set @CompUpdate@
-- to @false@, automatic compression is disabled and existing column
-- encodings aren\'t changed. The default is @true@.
--
-- 'replaceInvalidChars', 'redshiftSettings_replaceInvalidChars' - A list of characters that you want to replace. Use with @ReplaceChars@.
--
-- 'afterConnectScript', 'redshiftSettings_afterConnectScript' - Code to run after connecting. This parameter should contain the code
-- itself, not the name of a file containing the code.
--
-- 'fileTransferUploadStreams', 'redshiftSettings_fileTransferUploadStreams' - The number of threads used to upload a single file. This parameter
-- accepts a value from 1 through 64. It defaults to 10.
--
-- The number of parallel streams used to upload a single .csv file to an
-- S3 bucket using S3 Multipart Upload. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart upload overview>.
--
-- @FileTransferUploadStreams@ accepts a value from 1 through 64. It
-- defaults to 10.
--
-- 'bucketName', 'redshiftSettings_bucketName' - The name of the intermediate S3 bucket used to store .csv files before
-- uploading data to Redshift.
--
-- 'secretsManagerSecretId', 'redshiftSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Amazon Redshift endpoint
-- connection details.
--
-- 'explicitIds', 'redshiftSettings_explicitIds' - This setting is only valid for a full-load migration task. Set
-- @ExplicitIds@ to @true@ to have tables with @IDENTITY@ columns override
-- their auto-generated values with explicit values loaded from the source
-- data files used to populate the tables. The default is @false@.
--
-- 'serverSideEncryptionKmsKeyId', 'redshiftSettings_serverSideEncryptionKmsKeyId' - The KMS key ID. If you are using @SSE_KMS@ for the @EncryptionMode@,
-- provide this key ID. The key that you use needs an attached policy that
-- enables IAM user permissions and allows use of the key.
--
-- 'dateFormat', 'redshiftSettings_dateFormat' - The date format that you are using. Valid values are @auto@
-- (case-sensitive), your date format string enclosed in quotes, or NULL.
-- If this parameter is left unset (NULL), it defaults to a format of
-- \'YYYY-MM-DD\'. Using @auto@ recognizes most strings, even some that
-- aren\'t supported when you use a date format string.
--
-- If your date and time values use formats different from each other, set
-- this to @auto@.
--
-- 'writeBufferSize', 'redshiftSettings_writeBufferSize' - The size (in KB) of the in-memory file write buffer used when generating
-- .csv files on the local disk at the DMS replication instance. The
-- default value is 1000 (buffer size is 1000KB).
--
-- 'trimBlanks', 'redshiftSettings_trimBlanks' - A value that specifies to remove the trailing white space characters
-- from a VARCHAR string. This parameter applies only to columns with a
-- VARCHAR data type. Choose @true@ to remove unneeded white space. The
-- default is @false@.
--
-- 'acceptAnyDate', 'redshiftSettings_acceptAnyDate' - A value that indicates to allow any date format, including invalid
-- formats such as 00\/00\/00 00:00:00, to be loaded without generating an
-- error. You can choose @true@ or @false@ (the default).
--
-- This parameter applies only to TIMESTAMP and DATE columns. Always use
-- ACCEPTANYDATE with the DATEFORMAT parameter. If the date format for the
-- data doesn\'t match the DATEFORMAT specification, Amazon Redshift
-- inserts a NULL value into that field.
--
-- 'encryptionMode', 'redshiftSettings_encryptionMode' - The type of server-side encryption that you want to use for your data.
-- This encryption type is part of the endpoint settings or the extra
-- connections attributes for Amazon S3. You can choose either @SSE_S3@
-- (the default) or @SSE_KMS@.
--
-- For the @ModifyEndpoint@ operation, you can change the existing value of
-- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
-- change the existing value from @SSE_S3@ to @SSE_KMS@.
--
-- To use @SSE_S3@, create an Identity and Access Management (IAM) role
-- with a policy that allows @\"arn:aws:s3:::*\"@ to use the following
-- actions: @\"s3:PutObject\", \"s3:ListBucket\"@
newRedshiftSettings ::
  RedshiftSettings
newRedshiftSettings =
  RedshiftSettings'
    { emptyAsNull = Prelude.Nothing,
      port = Prelude.Nothing,
      truncateColumns = Prelude.Nothing,
      maxFileSize = Prelude.Nothing,
      replaceChars = Prelude.Nothing,
      timeFormat = Prelude.Nothing,
      loadTimeout = Prelude.Nothing,
      bucketFolder = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      connectionTimeout = Prelude.Nothing,
      password = Prelude.Nothing,
      serverName = Prelude.Nothing,
      caseSensitiveNames = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      username = Prelude.Nothing,
      removeQuotes = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      compUpdate = Prelude.Nothing,
      replaceInvalidChars = Prelude.Nothing,
      afterConnectScript = Prelude.Nothing,
      fileTransferUploadStreams = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      explicitIds = Prelude.Nothing,
      serverSideEncryptionKmsKeyId = Prelude.Nothing,
      dateFormat = Prelude.Nothing,
      writeBufferSize = Prelude.Nothing,
      trimBlanks = Prelude.Nothing,
      acceptAnyDate = Prelude.Nothing,
      encryptionMode = Prelude.Nothing
    }

-- | A value that specifies whether DMS should migrate empty CHAR and VARCHAR
-- fields as NULL. A value of @true@ sets empty CHAR and VARCHAR fields to
-- null. The default is @false@.
redshiftSettings_emptyAsNull :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Bool)
redshiftSettings_emptyAsNull = Lens.lens (\RedshiftSettings' {emptyAsNull} -> emptyAsNull) (\s@RedshiftSettings' {} a -> s {emptyAsNull = a} :: RedshiftSettings)

-- | The port number for Amazon Redshift. The default value is 5439.
redshiftSettings_port :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Int)
redshiftSettings_port = Lens.lens (\RedshiftSettings' {port} -> port) (\s@RedshiftSettings' {} a -> s {port = a} :: RedshiftSettings)

-- | A value that specifies to truncate data in columns to the appropriate
-- number of characters, so that the data fits in the column. This
-- parameter applies only to columns with a VARCHAR or CHAR data type, and
-- rows with a size of 4 MB or less. Choose @true@ to truncate data. The
-- default is @false@.
redshiftSettings_truncateColumns :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Bool)
redshiftSettings_truncateColumns = Lens.lens (\RedshiftSettings' {truncateColumns} -> truncateColumns) (\s@RedshiftSettings' {} a -> s {truncateColumns = a} :: RedshiftSettings)

-- | The maximum size (in KB) of any .csv file used to load data on an S3
-- bucket and transfer data to Amazon Redshift. It defaults to 1048576KB (1
-- GB).
redshiftSettings_maxFileSize :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Int)
redshiftSettings_maxFileSize = Lens.lens (\RedshiftSettings' {maxFileSize} -> maxFileSize) (\s@RedshiftSettings' {} a -> s {maxFileSize = a} :: RedshiftSettings)

-- | A value that specifies to replaces the invalid characters specified in
-- @ReplaceInvalidChars@, substituting the specified characters instead.
-- The default is @\"?\"@.
redshiftSettings_replaceChars :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_replaceChars = Lens.lens (\RedshiftSettings' {replaceChars} -> replaceChars) (\s@RedshiftSettings' {} a -> s {replaceChars = a} :: RedshiftSettings)

-- | The time format that you want to use. Valid values are @auto@
-- (case-sensitive), @\'timeformat_string\'@, @\'epochsecs\'@, or
-- @\'epochmillisecs\'@. It defaults to 10. Using @auto@ recognizes most
-- strings, even some that aren\'t supported when you use a time format
-- string.
--
-- If your date and time values use formats different from each other, set
-- this parameter to @auto@.
redshiftSettings_timeFormat :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_timeFormat = Lens.lens (\RedshiftSettings' {timeFormat} -> timeFormat) (\s@RedshiftSettings' {} a -> s {timeFormat = a} :: RedshiftSettings)

-- | The amount of time to wait (in milliseconds) before timing out of
-- operations performed by DMS on a Redshift cluster, such as Redshift
-- COPY, INSERT, DELETE, and UPDATE.
redshiftSettings_loadTimeout :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Int)
redshiftSettings_loadTimeout = Lens.lens (\RedshiftSettings' {loadTimeout} -> loadTimeout) (\s@RedshiftSettings' {} a -> s {loadTimeout = a} :: RedshiftSettings)

-- | An S3 folder where the comma-separated-value (.csv) files are stored
-- before being uploaded to the target Redshift cluster.
--
-- For full load mode, DMS converts source records into .csv files and
-- loads them to the /BucketFolder\/TableID/ path. DMS uses the Redshift
-- @COPY@ command to upload the .csv files to the target table. The files
-- are deleted once the @COPY@ operation has finished. For more
-- information, see
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html COPY> in the
-- /Amazon Redshift Database Developer Guide/.
--
-- For change-data-capture (CDC) mode, DMS creates a /NetChanges/ table,
-- and loads the .csv files to this /BucketFolder\/NetChangesTableID/ path.
redshiftSettings_bucketFolder :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_bucketFolder = Lens.lens (\RedshiftSettings' {bucketFolder} -> bucketFolder) (\s@RedshiftSettings' {} a -> s {bucketFolder = a} :: RedshiftSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the Amazon Redshift
-- endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
redshiftSettings_secretsManagerAccessRoleArn :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_secretsManagerAccessRoleArn = Lens.lens (\RedshiftSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@RedshiftSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: RedshiftSettings)

-- | A value that sets the amount of time to wait (in milliseconds) before
-- timing out, beginning from when you initially establish a connection.
redshiftSettings_connectionTimeout :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Int)
redshiftSettings_connectionTimeout = Lens.lens (\RedshiftSettings' {connectionTimeout} -> connectionTimeout) (\s@RedshiftSettings' {} a -> s {connectionTimeout = a} :: RedshiftSettings)

-- | The password for the user named in the @username@ property.
redshiftSettings_password :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_password = Lens.lens (\RedshiftSettings' {password} -> password) (\s@RedshiftSettings' {} a -> s {password = a} :: RedshiftSettings) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the Amazon Redshift cluster you are using.
redshiftSettings_serverName :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_serverName = Lens.lens (\RedshiftSettings' {serverName} -> serverName) (\s@RedshiftSettings' {} a -> s {serverName = a} :: RedshiftSettings)

-- | If Amazon Redshift is configured to support case sensitive schema names,
-- set @CaseSensitiveNames@ to @true@. The default is @false@.
redshiftSettings_caseSensitiveNames :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Bool)
redshiftSettings_caseSensitiveNames = Lens.lens (\RedshiftSettings' {caseSensitiveNames} -> caseSensitiveNames) (\s@RedshiftSettings' {} a -> s {caseSensitiveNames = a} :: RedshiftSettings)

-- | The name of the Amazon Redshift data warehouse (service) that you are
-- working with.
redshiftSettings_databaseName :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_databaseName = Lens.lens (\RedshiftSettings' {databaseName} -> databaseName) (\s@RedshiftSettings' {} a -> s {databaseName = a} :: RedshiftSettings)

-- | An Amazon Redshift user name for a registered user.
redshiftSettings_username :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_username = Lens.lens (\RedshiftSettings' {username} -> username) (\s@RedshiftSettings' {} a -> s {username = a} :: RedshiftSettings)

-- | A value that specifies to remove surrounding quotation marks from
-- strings in the incoming data. All characters within the quotation marks,
-- including delimiters, are retained. Choose @true@ to remove quotation
-- marks. The default is @false@.
redshiftSettings_removeQuotes :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Bool)
redshiftSettings_removeQuotes = Lens.lens (\RedshiftSettings' {removeQuotes} -> removeQuotes) (\s@RedshiftSettings' {} a -> s {removeQuotes = a} :: RedshiftSettings)

-- | The Amazon Resource Name (ARN) of the IAM role that has access to the
-- Amazon Redshift service. The role must allow the @iam:PassRole@ action.
redshiftSettings_serviceAccessRoleArn :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_serviceAccessRoleArn = Lens.lens (\RedshiftSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@RedshiftSettings' {} a -> s {serviceAccessRoleArn = a} :: RedshiftSettings)

-- | If you set @CompUpdate@ to @true@ Amazon Redshift applies automatic
-- compression if the table is empty. This applies even if the table
-- columns already have encodings other than @RAW@. If you set @CompUpdate@
-- to @false@, automatic compression is disabled and existing column
-- encodings aren\'t changed. The default is @true@.
redshiftSettings_compUpdate :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Bool)
redshiftSettings_compUpdate = Lens.lens (\RedshiftSettings' {compUpdate} -> compUpdate) (\s@RedshiftSettings' {} a -> s {compUpdate = a} :: RedshiftSettings)

-- | A list of characters that you want to replace. Use with @ReplaceChars@.
redshiftSettings_replaceInvalidChars :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_replaceInvalidChars = Lens.lens (\RedshiftSettings' {replaceInvalidChars} -> replaceInvalidChars) (\s@RedshiftSettings' {} a -> s {replaceInvalidChars = a} :: RedshiftSettings)

-- | Code to run after connecting. This parameter should contain the code
-- itself, not the name of a file containing the code.
redshiftSettings_afterConnectScript :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_afterConnectScript = Lens.lens (\RedshiftSettings' {afterConnectScript} -> afterConnectScript) (\s@RedshiftSettings' {} a -> s {afterConnectScript = a} :: RedshiftSettings)

-- | The number of threads used to upload a single file. This parameter
-- accepts a value from 1 through 64. It defaults to 10.
--
-- The number of parallel streams used to upload a single .csv file to an
-- S3 bucket using S3 Multipart Upload. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart upload overview>.
--
-- @FileTransferUploadStreams@ accepts a value from 1 through 64. It
-- defaults to 10.
redshiftSettings_fileTransferUploadStreams :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Int)
redshiftSettings_fileTransferUploadStreams = Lens.lens (\RedshiftSettings' {fileTransferUploadStreams} -> fileTransferUploadStreams) (\s@RedshiftSettings' {} a -> s {fileTransferUploadStreams = a} :: RedshiftSettings)

-- | The name of the intermediate S3 bucket used to store .csv files before
-- uploading data to Redshift.
redshiftSettings_bucketName :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_bucketName = Lens.lens (\RedshiftSettings' {bucketName} -> bucketName) (\s@RedshiftSettings' {} a -> s {bucketName = a} :: RedshiftSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the Amazon Redshift endpoint
-- connection details.
redshiftSettings_secretsManagerSecretId :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_secretsManagerSecretId = Lens.lens (\RedshiftSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@RedshiftSettings' {} a -> s {secretsManagerSecretId = a} :: RedshiftSettings)

-- | This setting is only valid for a full-load migration task. Set
-- @ExplicitIds@ to @true@ to have tables with @IDENTITY@ columns override
-- their auto-generated values with explicit values loaded from the source
-- data files used to populate the tables. The default is @false@.
redshiftSettings_explicitIds :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Bool)
redshiftSettings_explicitIds = Lens.lens (\RedshiftSettings' {explicitIds} -> explicitIds) (\s@RedshiftSettings' {} a -> s {explicitIds = a} :: RedshiftSettings)

-- | The KMS key ID. If you are using @SSE_KMS@ for the @EncryptionMode@,
-- provide this key ID. The key that you use needs an attached policy that
-- enables IAM user permissions and allows use of the key.
redshiftSettings_serverSideEncryptionKmsKeyId :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_serverSideEncryptionKmsKeyId = Lens.lens (\RedshiftSettings' {serverSideEncryptionKmsKeyId} -> serverSideEncryptionKmsKeyId) (\s@RedshiftSettings' {} a -> s {serverSideEncryptionKmsKeyId = a} :: RedshiftSettings)

-- | The date format that you are using. Valid values are @auto@
-- (case-sensitive), your date format string enclosed in quotes, or NULL.
-- If this parameter is left unset (NULL), it defaults to a format of
-- \'YYYY-MM-DD\'. Using @auto@ recognizes most strings, even some that
-- aren\'t supported when you use a date format string.
--
-- If your date and time values use formats different from each other, set
-- this to @auto@.
redshiftSettings_dateFormat :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Text)
redshiftSettings_dateFormat = Lens.lens (\RedshiftSettings' {dateFormat} -> dateFormat) (\s@RedshiftSettings' {} a -> s {dateFormat = a} :: RedshiftSettings)

-- | The size (in KB) of the in-memory file write buffer used when generating
-- .csv files on the local disk at the DMS replication instance. The
-- default value is 1000 (buffer size is 1000KB).
redshiftSettings_writeBufferSize :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Int)
redshiftSettings_writeBufferSize = Lens.lens (\RedshiftSettings' {writeBufferSize} -> writeBufferSize) (\s@RedshiftSettings' {} a -> s {writeBufferSize = a} :: RedshiftSettings)

-- | A value that specifies to remove the trailing white space characters
-- from a VARCHAR string. This parameter applies only to columns with a
-- VARCHAR data type. Choose @true@ to remove unneeded white space. The
-- default is @false@.
redshiftSettings_trimBlanks :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Bool)
redshiftSettings_trimBlanks = Lens.lens (\RedshiftSettings' {trimBlanks} -> trimBlanks) (\s@RedshiftSettings' {} a -> s {trimBlanks = a} :: RedshiftSettings)

-- | A value that indicates to allow any date format, including invalid
-- formats such as 00\/00\/00 00:00:00, to be loaded without generating an
-- error. You can choose @true@ or @false@ (the default).
--
-- This parameter applies only to TIMESTAMP and DATE columns. Always use
-- ACCEPTANYDATE with the DATEFORMAT parameter. If the date format for the
-- data doesn\'t match the DATEFORMAT specification, Amazon Redshift
-- inserts a NULL value into that field.
redshiftSettings_acceptAnyDate :: Lens.Lens' RedshiftSettings (Prelude.Maybe Prelude.Bool)
redshiftSettings_acceptAnyDate = Lens.lens (\RedshiftSettings' {acceptAnyDate} -> acceptAnyDate) (\s@RedshiftSettings' {} a -> s {acceptAnyDate = a} :: RedshiftSettings)

-- | The type of server-side encryption that you want to use for your data.
-- This encryption type is part of the endpoint settings or the extra
-- connections attributes for Amazon S3. You can choose either @SSE_S3@
-- (the default) or @SSE_KMS@.
--
-- For the @ModifyEndpoint@ operation, you can change the existing value of
-- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
-- change the existing value from @SSE_S3@ to @SSE_KMS@.
--
-- To use @SSE_S3@, create an Identity and Access Management (IAM) role
-- with a policy that allows @\"arn:aws:s3:::*\"@ to use the following
-- actions: @\"s3:PutObject\", \"s3:ListBucket\"@
redshiftSettings_encryptionMode :: Lens.Lens' RedshiftSettings (Prelude.Maybe EncryptionModeValue)
redshiftSettings_encryptionMode = Lens.lens (\RedshiftSettings' {encryptionMode} -> encryptionMode) (\s@RedshiftSettings' {} a -> s {encryptionMode = a} :: RedshiftSettings)

instance Data.FromJSON RedshiftSettings where
  parseJSON =
    Data.withObject
      "RedshiftSettings"
      ( \x ->
          RedshiftSettings'
            Prelude.<$> (x Data..:? "EmptyAsNull")
            Prelude.<*> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "TruncateColumns")
            Prelude.<*> (x Data..:? "MaxFileSize")
            Prelude.<*> (x Data..:? "ReplaceChars")
            Prelude.<*> (x Data..:? "TimeFormat")
            Prelude.<*> (x Data..:? "LoadTimeout")
            Prelude.<*> (x Data..:? "BucketFolder")
            Prelude.<*> (x Data..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Data..:? "ConnectionTimeout")
            Prelude.<*> (x Data..:? "Password")
            Prelude.<*> (x Data..:? "ServerName")
            Prelude.<*> (x Data..:? "CaseSensitiveNames")
            Prelude.<*> (x Data..:? "DatabaseName")
            Prelude.<*> (x Data..:? "Username")
            Prelude.<*> (x Data..:? "RemoveQuotes")
            Prelude.<*> (x Data..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Data..:? "CompUpdate")
            Prelude.<*> (x Data..:? "ReplaceInvalidChars")
            Prelude.<*> (x Data..:? "AfterConnectScript")
            Prelude.<*> (x Data..:? "FileTransferUploadStreams")
            Prelude.<*> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "SecretsManagerSecretId")
            Prelude.<*> (x Data..:? "ExplicitIds")
            Prelude.<*> (x Data..:? "ServerSideEncryptionKmsKeyId")
            Prelude.<*> (x Data..:? "DateFormat")
            Prelude.<*> (x Data..:? "WriteBufferSize")
            Prelude.<*> (x Data..:? "TrimBlanks")
            Prelude.<*> (x Data..:? "AcceptAnyDate")
            Prelude.<*> (x Data..:? "EncryptionMode")
      )

instance Prelude.Hashable RedshiftSettings where
  hashWithSalt _salt RedshiftSettings' {..} =
    _salt `Prelude.hashWithSalt` emptyAsNull
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` truncateColumns
      `Prelude.hashWithSalt` maxFileSize
      `Prelude.hashWithSalt` replaceChars
      `Prelude.hashWithSalt` timeFormat
      `Prelude.hashWithSalt` loadTimeout
      `Prelude.hashWithSalt` bucketFolder
      `Prelude.hashWithSalt` secretsManagerAccessRoleArn
      `Prelude.hashWithSalt` connectionTimeout
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` caseSensitiveNames
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` removeQuotes
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` compUpdate
      `Prelude.hashWithSalt` replaceInvalidChars
      `Prelude.hashWithSalt` afterConnectScript
      `Prelude.hashWithSalt` fileTransferUploadStreams
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` secretsManagerSecretId
      `Prelude.hashWithSalt` explicitIds
      `Prelude.hashWithSalt` serverSideEncryptionKmsKeyId
      `Prelude.hashWithSalt` dateFormat
      `Prelude.hashWithSalt` writeBufferSize
      `Prelude.hashWithSalt` trimBlanks
      `Prelude.hashWithSalt` acceptAnyDate
      `Prelude.hashWithSalt` encryptionMode

instance Prelude.NFData RedshiftSettings where
  rnf RedshiftSettings' {..} =
    Prelude.rnf emptyAsNull
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf truncateColumns
      `Prelude.seq` Prelude.rnf maxFileSize
      `Prelude.seq` Prelude.rnf replaceChars
      `Prelude.seq` Prelude.rnf timeFormat
      `Prelude.seq` Prelude.rnf loadTimeout
      `Prelude.seq` Prelude.rnf bucketFolder
      `Prelude.seq` Prelude.rnf secretsManagerAccessRoleArn
      `Prelude.seq` Prelude.rnf connectionTimeout
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf caseSensitiveNames
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf removeQuotes
      `Prelude.seq` Prelude.rnf serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf compUpdate
      `Prelude.seq` Prelude.rnf replaceInvalidChars
      `Prelude.seq` Prelude.rnf afterConnectScript
      `Prelude.seq` Prelude.rnf
        fileTransferUploadStreams
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf
        secretsManagerSecretId
      `Prelude.seq` Prelude.rnf
        explicitIds
      `Prelude.seq` Prelude.rnf
        serverSideEncryptionKmsKeyId
      `Prelude.seq` Prelude.rnf
        dateFormat
      `Prelude.seq` Prelude.rnf
        writeBufferSize
      `Prelude.seq` Prelude.rnf
        trimBlanks
      `Prelude.seq` Prelude.rnf
        acceptAnyDate
      `Prelude.seq` Prelude.rnf
        encryptionMode

instance Data.ToJSON RedshiftSettings where
  toJSON RedshiftSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EmptyAsNull" Data..=) Prelude.<$> emptyAsNull,
            ("Port" Data..=) Prelude.<$> port,
            ("TruncateColumns" Data..=)
              Prelude.<$> truncateColumns,
            ("MaxFileSize" Data..=) Prelude.<$> maxFileSize,
            ("ReplaceChars" Data..=) Prelude.<$> replaceChars,
            ("TimeFormat" Data..=) Prelude.<$> timeFormat,
            ("LoadTimeout" Data..=) Prelude.<$> loadTimeout,
            ("BucketFolder" Data..=) Prelude.<$> bucketFolder,
            ("SecretsManagerAccessRoleArn" Data..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("ConnectionTimeout" Data..=)
              Prelude.<$> connectionTimeout,
            ("Password" Data..=) Prelude.<$> password,
            ("ServerName" Data..=) Prelude.<$> serverName,
            ("CaseSensitiveNames" Data..=)
              Prelude.<$> caseSensitiveNames,
            ("DatabaseName" Data..=) Prelude.<$> databaseName,
            ("Username" Data..=) Prelude.<$> username,
            ("RemoveQuotes" Data..=) Prelude.<$> removeQuotes,
            ("ServiceAccessRoleArn" Data..=)
              Prelude.<$> serviceAccessRoleArn,
            ("CompUpdate" Data..=) Prelude.<$> compUpdate,
            ("ReplaceInvalidChars" Data..=)
              Prelude.<$> replaceInvalidChars,
            ("AfterConnectScript" Data..=)
              Prelude.<$> afterConnectScript,
            ("FileTransferUploadStreams" Data..=)
              Prelude.<$> fileTransferUploadStreams,
            ("BucketName" Data..=) Prelude.<$> bucketName,
            ("SecretsManagerSecretId" Data..=)
              Prelude.<$> secretsManagerSecretId,
            ("ExplicitIds" Data..=) Prelude.<$> explicitIds,
            ("ServerSideEncryptionKmsKeyId" Data..=)
              Prelude.<$> serverSideEncryptionKmsKeyId,
            ("DateFormat" Data..=) Prelude.<$> dateFormat,
            ("WriteBufferSize" Data..=)
              Prelude.<$> writeBufferSize,
            ("TrimBlanks" Data..=) Prelude.<$> trimBlanks,
            ("AcceptAnyDate" Data..=) Prelude.<$> acceptAnyDate,
            ("EncryptionMode" Data..=)
              Prelude.<$> encryptionMode
          ]
      )

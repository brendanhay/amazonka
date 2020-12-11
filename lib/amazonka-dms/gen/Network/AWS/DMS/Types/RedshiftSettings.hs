-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.RedshiftSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RedshiftSettings
  ( RedshiftSettings (..),

    -- * Smart constructor
    mkRedshiftSettings,

    -- * Lenses
    rsEmptyAsNull,
    rsCaseSensitiveNames,
    rsMaxFileSize,
    rsReplaceChars,
    rsServerName,
    rsConnectionTimeout,
    rsLoadTimeout,
    rsServiceAccessRoleARN,
    rsExplicitIds,
    rsBucketFolder,
    rsTruncateColumns,
    rsReplaceInvalidChars,
    rsUsername,
    rsBucketName,
    rsEncryptionMode,
    rsDateFormat,
    rsRemoveQuotes,
    rsPassword,
    rsDatabaseName,
    rsAcceptAnyDate,
    rsAfterConnectScript,
    rsWriteBufferSize,
    rsCompUpdate,
    rsTrimBlanks,
    rsTimeFormat,
    rsServerSideEncryptionKMSKeyId,
    rsPort,
    rsFileTransferUploadStreams,
  )
where

import Network.AWS.DMS.Types.EncryptionModeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information that defines an Amazon Redshift endpoint.
--
-- /See:/ 'mkRedshiftSettings' smart constructor.
data RedshiftSettings = RedshiftSettings'
  { emptyAsNull ::
      Lude.Maybe Lude.Bool,
    caseSensitiveNames :: Lude.Maybe Lude.Bool,
    maxFileSize :: Lude.Maybe Lude.Int,
    replaceChars :: Lude.Maybe Lude.Text,
    serverName :: Lude.Maybe Lude.Text,
    connectionTimeout :: Lude.Maybe Lude.Int,
    loadTimeout :: Lude.Maybe Lude.Int,
    serviceAccessRoleARN :: Lude.Maybe Lude.Text,
    explicitIds :: Lude.Maybe Lude.Bool,
    bucketFolder :: Lude.Maybe Lude.Text,
    truncateColumns :: Lude.Maybe Lude.Bool,
    replaceInvalidChars :: Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    bucketName :: Lude.Maybe Lude.Text,
    encryptionMode :: Lude.Maybe EncryptionModeValue,
    dateFormat :: Lude.Maybe Lude.Text,
    removeQuotes :: Lude.Maybe Lude.Bool,
    password :: Lude.Maybe (Lude.Sensitive Lude.Text),
    databaseName :: Lude.Maybe Lude.Text,
    acceptAnyDate :: Lude.Maybe Lude.Bool,
    afterConnectScript :: Lude.Maybe Lude.Text,
    writeBufferSize :: Lude.Maybe Lude.Int,
    compUpdate :: Lude.Maybe Lude.Bool,
    trimBlanks :: Lude.Maybe Lude.Bool,
    timeFormat :: Lude.Maybe Lude.Text,
    serverSideEncryptionKMSKeyId :: Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int,
    fileTransferUploadStreams :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftSettings' with the minimum fields required to make a request.
--
-- * 'acceptAnyDate' - A value that indicates to allow any date format, including invalid formats such as 00/00/00 00:00:00, to be loaded without generating an error. You can choose @true@ or @false@ (the default).
--
-- This parameter applies only to TIMESTAMP and DATE columns. Always use ACCEPTANYDATE with the DATEFORMAT parameter. If the date format for the data doesn't match the DATEFORMAT specification, Amazon Redshift inserts a NULL value into that field.
-- * 'afterConnectScript' - Code to run after connecting. This parameter should contain the code itself, not the name of a file containing the code.
-- * 'bucketFolder' - An S3 folder where the comma-separated-value (.csv) files are stored before being uploaded to the target Redshift cluster.
--
-- For full load mode, AWS DMS converts source records into .csv files and loads them to the /BucketFolder\/TableID/ path. AWS DMS uses the Redshift @COPY@ command to upload the .csv files to the target table. The files are deleted once the @COPY@ operation has finished. For more information, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift Database Developer Guide>
-- For change-data-capture (CDC) mode, AWS DMS creates a /NetChanges/ table, and loads the .csv files to this /BucketFolder\/NetChangesTableID/ path.
-- * 'bucketName' - The name of the intermediate S3 bucket used to store .csv files before uploading data to Redshift.
-- * 'caseSensitiveNames' - If Amazon Redshift is configured to support case sensitive schema names, set @CaseSensitiveNames@ to @true@ . The default is @false@ .
-- * 'compUpdate' - If you set @CompUpdate@ to @true@ Amazon Redshift applies automatic compression if the table is empty. This applies even if the table columns already have encodings other than @RAW@ . If you set @CompUpdate@ to @false@ , automatic compression is disabled and existing column encodings aren't changed. The default is @true@ .
-- * 'connectionTimeout' - A value that sets the amount of time to wait (in milliseconds) before timing out, beginning from when you initially establish a connection.
-- * 'databaseName' - The name of the Amazon Redshift data warehouse (service) that you are working with.
-- * 'dateFormat' - The date format that you are using. Valid values are @auto@ (case-sensitive), your date format string enclosed in quotes, or NULL. If this parameter is left unset (NULL), it defaults to a format of 'YYYY-MM-DD'. Using @auto@ recognizes most strings, even some that aren't supported when you use a date format string.
--
-- If your date and time values use formats different from each other, set this to @auto@ .
-- * 'emptyAsNull' - A value that specifies whether AWS DMS should migrate empty CHAR and VARCHAR fields as NULL. A value of @true@ sets empty CHAR and VARCHAR fields to null. The default is @false@ .
-- * 'encryptionMode' - The type of server-side encryption that you want to use for your data. This encryption type is part of the endpoint settings or the extra connections attributes for Amazon S3. You can choose either @SSE_S3@ (the default) or @SSE_KMS@ .
--
-- To use @SSE_S3@ , create an AWS Identity and Access Management (IAM) role with a policy that allows @"arn:aws:s3:::*"@ to use the following actions: @"s3:PutObject", "s3:ListBucket"@
-- * 'explicitIds' - This setting is only valid for a full-load migration task. Set @ExplicitIds@ to @true@ to have tables with @IDENTITY@ columns override their auto-generated values with explicit values loaded from the source data files used to populate the tables. The default is @false@ .
-- * 'fileTransferUploadStreams' - The number of threads used to upload a single file. This parameter accepts a value from 1 through 64. It defaults to 10.
--
-- The number of parallel streams used to upload a single .csv file to an S3 bucket using S3 Multipart Upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart upload overview> .
-- @FileTransferUploadStreams@ accepts a value from 1 through 64. It defaults to 10.
-- * 'loadTimeout' - The amount of time to wait (in milliseconds) before timing out of operations performed by AWS DMS on a Redshift cluster, such as Redshift COPY, INSERT, DELETE, and UPDATE.
-- * 'maxFileSize' - The maximum size (in KB) of any .csv file used to load data on an S3 bucket and transfer data to Amazon Redshift. It defaults to 1048576KB (1 GB).
-- * 'password' - The password for the user named in the @username@ property.
-- * 'port' - The port number for Amazon Redshift. The default value is 5439.
-- * 'removeQuotes' - A value that specifies to remove surrounding quotation marks from strings in the incoming data. All characters within the quotation marks, including delimiters, are retained. Choose @true@ to remove quotation marks. The default is @false@ .
-- * 'replaceChars' - A value that specifies to replaces the invalid characters specified in @ReplaceInvalidChars@ , substituting the specified characters instead. The default is @"?"@ .
-- * 'replaceInvalidChars' - A list of characters that you want to replace. Use with @ReplaceChars@ .
-- * 'serverName' - The name of the Amazon Redshift cluster you are using.
-- * 'serverSideEncryptionKMSKeyId' - The AWS KMS key ID. If you are using @SSE_KMS@ for the @EncryptionMode@ , provide this key ID. The key that you use needs an attached policy that enables IAM user permissions and allows use of the key.
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) of the IAM role that has access to the Amazon Redshift service.
-- * 'timeFormat' - The time format that you want to use. Valid values are @auto@ (case-sensitive), @'timeformat_string'@ , @'epochsecs'@ , or @'epochmillisecs'@ . It defaults to 10. Using @auto@ recognizes most strings, even some that aren't supported when you use a time format string.
--
-- If your date and time values use formats different from each other, set this parameter to @auto@ .
-- * 'trimBlanks' - A value that specifies to remove the trailing white space characters from a VARCHAR string. This parameter applies only to columns with a VARCHAR data type. Choose @true@ to remove unneeded white space. The default is @false@ .
-- * 'truncateColumns' - A value that specifies to truncate data in columns to the appropriate number of characters, so that the data fits in the column. This parameter applies only to columns with a VARCHAR or CHAR data type, and rows with a size of 4 MB or less. Choose @true@ to truncate data. The default is @false@ .
-- * 'username' - An Amazon Redshift user name for a registered user.
-- * 'writeBufferSize' - The size (in KB) of the in-memory file write buffer used when generating .csv files on the local disk at the DMS replication instance. The default value is 1000 (buffer size is 1000KB).
mkRedshiftSettings ::
  RedshiftSettings
mkRedshiftSettings =
  RedshiftSettings'
    { emptyAsNull = Lude.Nothing,
      caseSensitiveNames = Lude.Nothing,
      maxFileSize = Lude.Nothing,
      replaceChars = Lude.Nothing,
      serverName = Lude.Nothing,
      connectionTimeout = Lude.Nothing,
      loadTimeout = Lude.Nothing,
      serviceAccessRoleARN = Lude.Nothing,
      explicitIds = Lude.Nothing,
      bucketFolder = Lude.Nothing,
      truncateColumns = Lude.Nothing,
      replaceInvalidChars = Lude.Nothing,
      username = Lude.Nothing,
      bucketName = Lude.Nothing,
      encryptionMode = Lude.Nothing,
      dateFormat = Lude.Nothing,
      removeQuotes = Lude.Nothing,
      password = Lude.Nothing,
      databaseName = Lude.Nothing,
      acceptAnyDate = Lude.Nothing,
      afterConnectScript = Lude.Nothing,
      writeBufferSize = Lude.Nothing,
      compUpdate = Lude.Nothing,
      trimBlanks = Lude.Nothing,
      timeFormat = Lude.Nothing,
      serverSideEncryptionKMSKeyId = Lude.Nothing,
      port = Lude.Nothing,
      fileTransferUploadStreams = Lude.Nothing
    }

-- | A value that specifies whether AWS DMS should migrate empty CHAR and VARCHAR fields as NULL. A value of @true@ sets empty CHAR and VARCHAR fields to null. The default is @false@ .
--
-- /Note:/ Consider using 'emptyAsNull' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsEmptyAsNull :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Bool)
rsEmptyAsNull = Lens.lens (emptyAsNull :: RedshiftSettings -> Lude.Maybe Lude.Bool) (\s a -> s {emptyAsNull = a} :: RedshiftSettings)
{-# DEPRECATED rsEmptyAsNull "Use generic-lens or generic-optics with 'emptyAsNull' instead." #-}

-- | If Amazon Redshift is configured to support case sensitive schema names, set @CaseSensitiveNames@ to @true@ . The default is @false@ .
--
-- /Note:/ Consider using 'caseSensitiveNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsCaseSensitiveNames :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Bool)
rsCaseSensitiveNames = Lens.lens (caseSensitiveNames :: RedshiftSettings -> Lude.Maybe Lude.Bool) (\s a -> s {caseSensitiveNames = a} :: RedshiftSettings)
{-# DEPRECATED rsCaseSensitiveNames "Use generic-lens or generic-optics with 'caseSensitiveNames' instead." #-}

-- | The maximum size (in KB) of any .csv file used to load data on an S3 bucket and transfer data to Amazon Redshift. It defaults to 1048576KB (1 GB).
--
-- /Note:/ Consider using 'maxFileSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsMaxFileSize :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Int)
rsMaxFileSize = Lens.lens (maxFileSize :: RedshiftSettings -> Lude.Maybe Lude.Int) (\s a -> s {maxFileSize = a} :: RedshiftSettings)
{-# DEPRECATED rsMaxFileSize "Use generic-lens or generic-optics with 'maxFileSize' instead." #-}

-- | A value that specifies to replaces the invalid characters specified in @ReplaceInvalidChars@ , substituting the specified characters instead. The default is @"?"@ .
--
-- /Note:/ Consider using 'replaceChars' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsReplaceChars :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsReplaceChars = Lens.lens (replaceChars :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {replaceChars = a} :: RedshiftSettings)
{-# DEPRECATED rsReplaceChars "Use generic-lens or generic-optics with 'replaceChars' instead." #-}

-- | The name of the Amazon Redshift cluster you are using.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsServerName :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsServerName = Lens.lens (serverName :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: RedshiftSettings)
{-# DEPRECATED rsServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | A value that sets the amount of time to wait (in milliseconds) before timing out, beginning from when you initially establish a connection.
--
-- /Note:/ Consider using 'connectionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsConnectionTimeout :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Int)
rsConnectionTimeout = Lens.lens (connectionTimeout :: RedshiftSettings -> Lude.Maybe Lude.Int) (\s a -> s {connectionTimeout = a} :: RedshiftSettings)
{-# DEPRECATED rsConnectionTimeout "Use generic-lens or generic-optics with 'connectionTimeout' instead." #-}

-- | The amount of time to wait (in milliseconds) before timing out of operations performed by AWS DMS on a Redshift cluster, such as Redshift COPY, INSERT, DELETE, and UPDATE.
--
-- /Note:/ Consider using 'loadTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsLoadTimeout :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Int)
rsLoadTimeout = Lens.lens (loadTimeout :: RedshiftSettings -> Lude.Maybe Lude.Int) (\s a -> s {loadTimeout = a} :: RedshiftSettings)
{-# DEPRECATED rsLoadTimeout "Use generic-lens or generic-optics with 'loadTimeout' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that has access to the Amazon Redshift service.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsServiceAccessRoleARN :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: RedshiftSettings)
{-# DEPRECATED rsServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | This setting is only valid for a full-load migration task. Set @ExplicitIds@ to @true@ to have tables with @IDENTITY@ columns override their auto-generated values with explicit values loaded from the source data files used to populate the tables. The default is @false@ .
--
-- /Note:/ Consider using 'explicitIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsExplicitIds :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Bool)
rsExplicitIds = Lens.lens (explicitIds :: RedshiftSettings -> Lude.Maybe Lude.Bool) (\s a -> s {explicitIds = a} :: RedshiftSettings)
{-# DEPRECATED rsExplicitIds "Use generic-lens or generic-optics with 'explicitIds' instead." #-}

-- | An S3 folder where the comma-separated-value (.csv) files are stored before being uploaded to the target Redshift cluster.
--
-- For full load mode, AWS DMS converts source records into .csv files and loads them to the /BucketFolder\/TableID/ path. AWS DMS uses the Redshift @COPY@ command to upload the .csv files to the target table. The files are deleted once the @COPY@ operation has finished. For more information, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift Database Developer Guide>
-- For change-data-capture (CDC) mode, AWS DMS creates a /NetChanges/ table, and loads the .csv files to this /BucketFolder\/NetChangesTableID/ path.
--
-- /Note:/ Consider using 'bucketFolder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsBucketFolder :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsBucketFolder = Lens.lens (bucketFolder :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {bucketFolder = a} :: RedshiftSettings)
{-# DEPRECATED rsBucketFolder "Use generic-lens or generic-optics with 'bucketFolder' instead." #-}

-- | A value that specifies to truncate data in columns to the appropriate number of characters, so that the data fits in the column. This parameter applies only to columns with a VARCHAR or CHAR data type, and rows with a size of 4 MB or less. Choose @true@ to truncate data. The default is @false@ .
--
-- /Note:/ Consider using 'truncateColumns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsTruncateColumns :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Bool)
rsTruncateColumns = Lens.lens (truncateColumns :: RedshiftSettings -> Lude.Maybe Lude.Bool) (\s a -> s {truncateColumns = a} :: RedshiftSettings)
{-# DEPRECATED rsTruncateColumns "Use generic-lens or generic-optics with 'truncateColumns' instead." #-}

-- | A list of characters that you want to replace. Use with @ReplaceChars@ .
--
-- /Note:/ Consider using 'replaceInvalidChars' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsReplaceInvalidChars :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsReplaceInvalidChars = Lens.lens (replaceInvalidChars :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {replaceInvalidChars = a} :: RedshiftSettings)
{-# DEPRECATED rsReplaceInvalidChars "Use generic-lens or generic-optics with 'replaceInvalidChars' instead." #-}

-- | An Amazon Redshift user name for a registered user.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsUsername :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsUsername = Lens.lens (username :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: RedshiftSettings)
{-# DEPRECATED rsUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The name of the intermediate S3 bucket used to store .csv files before uploading data to Redshift.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsBucketName :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsBucketName = Lens.lens (bucketName :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {bucketName = a} :: RedshiftSettings)
{-# DEPRECATED rsBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The type of server-side encryption that you want to use for your data. This encryption type is part of the endpoint settings or the extra connections attributes for Amazon S3. You can choose either @SSE_S3@ (the default) or @SSE_KMS@ .
--
-- To use @SSE_S3@ , create an AWS Identity and Access Management (IAM) role with a policy that allows @"arn:aws:s3:::*"@ to use the following actions: @"s3:PutObject", "s3:ListBucket"@
--
-- /Note:/ Consider using 'encryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsEncryptionMode :: Lens.Lens' RedshiftSettings (Lude.Maybe EncryptionModeValue)
rsEncryptionMode = Lens.lens (encryptionMode :: RedshiftSettings -> Lude.Maybe EncryptionModeValue) (\s a -> s {encryptionMode = a} :: RedshiftSettings)
{-# DEPRECATED rsEncryptionMode "Use generic-lens or generic-optics with 'encryptionMode' instead." #-}

-- | The date format that you are using. Valid values are @auto@ (case-sensitive), your date format string enclosed in quotes, or NULL. If this parameter is left unset (NULL), it defaults to a format of 'YYYY-MM-DD'. Using @auto@ recognizes most strings, even some that aren't supported when you use a date format string.
--
-- If your date and time values use formats different from each other, set this to @auto@ .
--
-- /Note:/ Consider using 'dateFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsDateFormat :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsDateFormat = Lens.lens (dateFormat :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {dateFormat = a} :: RedshiftSettings)
{-# DEPRECATED rsDateFormat "Use generic-lens or generic-optics with 'dateFormat' instead." #-}

-- | A value that specifies to remove surrounding quotation marks from strings in the incoming data. All characters within the quotation marks, including delimiters, are retained. Choose @true@ to remove quotation marks. The default is @false@ .
--
-- /Note:/ Consider using 'removeQuotes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsRemoveQuotes :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Bool)
rsRemoveQuotes = Lens.lens (removeQuotes :: RedshiftSettings -> Lude.Maybe Lude.Bool) (\s a -> s {removeQuotes = a} :: RedshiftSettings)
{-# DEPRECATED rsRemoveQuotes "Use generic-lens or generic-optics with 'removeQuotes' instead." #-}

-- | The password for the user named in the @username@ property.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsPassword :: Lens.Lens' RedshiftSettings (Lude.Maybe (Lude.Sensitive Lude.Text))
rsPassword = Lens.lens (password :: RedshiftSettings -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {password = a} :: RedshiftSettings)
{-# DEPRECATED rsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The name of the Amazon Redshift data warehouse (service) that you are working with.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsDatabaseName :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsDatabaseName = Lens.lens (databaseName :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: RedshiftSettings)
{-# DEPRECATED rsDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | A value that indicates to allow any date format, including invalid formats such as 00/00/00 00:00:00, to be loaded without generating an error. You can choose @true@ or @false@ (the default).
--
-- This parameter applies only to TIMESTAMP and DATE columns. Always use ACCEPTANYDATE with the DATEFORMAT parameter. If the date format for the data doesn't match the DATEFORMAT specification, Amazon Redshift inserts a NULL value into that field.
--
-- /Note:/ Consider using 'acceptAnyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsAcceptAnyDate :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Bool)
rsAcceptAnyDate = Lens.lens (acceptAnyDate :: RedshiftSettings -> Lude.Maybe Lude.Bool) (\s a -> s {acceptAnyDate = a} :: RedshiftSettings)
{-# DEPRECATED rsAcceptAnyDate "Use generic-lens or generic-optics with 'acceptAnyDate' instead." #-}

-- | Code to run after connecting. This parameter should contain the code itself, not the name of a file containing the code.
--
-- /Note:/ Consider using 'afterConnectScript' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsAfterConnectScript :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsAfterConnectScript = Lens.lens (afterConnectScript :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {afterConnectScript = a} :: RedshiftSettings)
{-# DEPRECATED rsAfterConnectScript "Use generic-lens or generic-optics with 'afterConnectScript' instead." #-}

-- | The size (in KB) of the in-memory file write buffer used when generating .csv files on the local disk at the DMS replication instance. The default value is 1000 (buffer size is 1000KB).
--
-- /Note:/ Consider using 'writeBufferSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsWriteBufferSize :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Int)
rsWriteBufferSize = Lens.lens (writeBufferSize :: RedshiftSettings -> Lude.Maybe Lude.Int) (\s a -> s {writeBufferSize = a} :: RedshiftSettings)
{-# DEPRECATED rsWriteBufferSize "Use generic-lens or generic-optics with 'writeBufferSize' instead." #-}

-- | If you set @CompUpdate@ to @true@ Amazon Redshift applies automatic compression if the table is empty. This applies even if the table columns already have encodings other than @RAW@ . If you set @CompUpdate@ to @false@ , automatic compression is disabled and existing column encodings aren't changed. The default is @true@ .
--
-- /Note:/ Consider using 'compUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsCompUpdate :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Bool)
rsCompUpdate = Lens.lens (compUpdate :: RedshiftSettings -> Lude.Maybe Lude.Bool) (\s a -> s {compUpdate = a} :: RedshiftSettings)
{-# DEPRECATED rsCompUpdate "Use generic-lens or generic-optics with 'compUpdate' instead." #-}

-- | A value that specifies to remove the trailing white space characters from a VARCHAR string. This parameter applies only to columns with a VARCHAR data type. Choose @true@ to remove unneeded white space. The default is @false@ .
--
-- /Note:/ Consider using 'trimBlanks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsTrimBlanks :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Bool)
rsTrimBlanks = Lens.lens (trimBlanks :: RedshiftSettings -> Lude.Maybe Lude.Bool) (\s a -> s {trimBlanks = a} :: RedshiftSettings)
{-# DEPRECATED rsTrimBlanks "Use generic-lens or generic-optics with 'trimBlanks' instead." #-}

-- | The time format that you want to use. Valid values are @auto@ (case-sensitive), @'timeformat_string'@ , @'epochsecs'@ , or @'epochmillisecs'@ . It defaults to 10. Using @auto@ recognizes most strings, even some that aren't supported when you use a time format string.
--
-- If your date and time values use formats different from each other, set this parameter to @auto@ .
--
-- /Note:/ Consider using 'timeFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsTimeFormat :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsTimeFormat = Lens.lens (timeFormat :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {timeFormat = a} :: RedshiftSettings)
{-# DEPRECATED rsTimeFormat "Use generic-lens or generic-optics with 'timeFormat' instead." #-}

-- | The AWS KMS key ID. If you are using @SSE_KMS@ for the @EncryptionMode@ , provide this key ID. The key that you use needs an attached policy that enables IAM user permissions and allows use of the key.
--
-- /Note:/ Consider using 'serverSideEncryptionKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsServerSideEncryptionKMSKeyId :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Text)
rsServerSideEncryptionKMSKeyId = Lens.lens (serverSideEncryptionKMSKeyId :: RedshiftSettings -> Lude.Maybe Lude.Text) (\s a -> s {serverSideEncryptionKMSKeyId = a} :: RedshiftSettings)
{-# DEPRECATED rsServerSideEncryptionKMSKeyId "Use generic-lens or generic-optics with 'serverSideEncryptionKMSKeyId' instead." #-}

-- | The port number for Amazon Redshift. The default value is 5439.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsPort :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Int)
rsPort = Lens.lens (port :: RedshiftSettings -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RedshiftSettings)
{-# DEPRECATED rsPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The number of threads used to upload a single file. This parameter accepts a value from 1 through 64. It defaults to 10.
--
-- The number of parallel streams used to upload a single .csv file to an S3 bucket using S3 Multipart Upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart upload overview> .
-- @FileTransferUploadStreams@ accepts a value from 1 through 64. It defaults to 10.
--
-- /Note:/ Consider using 'fileTransferUploadStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsFileTransferUploadStreams :: Lens.Lens' RedshiftSettings (Lude.Maybe Lude.Int)
rsFileTransferUploadStreams = Lens.lens (fileTransferUploadStreams :: RedshiftSettings -> Lude.Maybe Lude.Int) (\s a -> s {fileTransferUploadStreams = a} :: RedshiftSettings)
{-# DEPRECATED rsFileTransferUploadStreams "Use generic-lens or generic-optics with 'fileTransferUploadStreams' instead." #-}

instance Lude.FromJSON RedshiftSettings where
  parseJSON =
    Lude.withObject
      "RedshiftSettings"
      ( \x ->
          RedshiftSettings'
            Lude.<$> (x Lude..:? "EmptyAsNull")
            Lude.<*> (x Lude..:? "CaseSensitiveNames")
            Lude.<*> (x Lude..:? "MaxFileSize")
            Lude.<*> (x Lude..:? "ReplaceChars")
            Lude.<*> (x Lude..:? "ServerName")
            Lude.<*> (x Lude..:? "ConnectionTimeout")
            Lude.<*> (x Lude..:? "LoadTimeout")
            Lude.<*> (x Lude..:? "ServiceAccessRoleArn")
            Lude.<*> (x Lude..:? "ExplicitIds")
            Lude.<*> (x Lude..:? "BucketFolder")
            Lude.<*> (x Lude..:? "TruncateColumns")
            Lude.<*> (x Lude..:? "ReplaceInvalidChars")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "BucketName")
            Lude.<*> (x Lude..:? "EncryptionMode")
            Lude.<*> (x Lude..:? "DateFormat")
            Lude.<*> (x Lude..:? "RemoveQuotes")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "DatabaseName")
            Lude.<*> (x Lude..:? "AcceptAnyDate")
            Lude.<*> (x Lude..:? "AfterConnectScript")
            Lude.<*> (x Lude..:? "WriteBufferSize")
            Lude.<*> (x Lude..:? "CompUpdate")
            Lude.<*> (x Lude..:? "TrimBlanks")
            Lude.<*> (x Lude..:? "TimeFormat")
            Lude.<*> (x Lude..:? "ServerSideEncryptionKmsKeyId")
            Lude.<*> (x Lude..:? "Port")
            Lude.<*> (x Lude..:? "FileTransferUploadStreams")
      )

instance Lude.ToJSON RedshiftSettings where
  toJSON RedshiftSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EmptyAsNull" Lude..=) Lude.<$> emptyAsNull,
            ("CaseSensitiveNames" Lude..=) Lude.<$> caseSensitiveNames,
            ("MaxFileSize" Lude..=) Lude.<$> maxFileSize,
            ("ReplaceChars" Lude..=) Lude.<$> replaceChars,
            ("ServerName" Lude..=) Lude.<$> serverName,
            ("ConnectionTimeout" Lude..=) Lude.<$> connectionTimeout,
            ("LoadTimeout" Lude..=) Lude.<$> loadTimeout,
            ("ServiceAccessRoleArn" Lude..=) Lude.<$> serviceAccessRoleARN,
            ("ExplicitIds" Lude..=) Lude.<$> explicitIds,
            ("BucketFolder" Lude..=) Lude.<$> bucketFolder,
            ("TruncateColumns" Lude..=) Lude.<$> truncateColumns,
            ("ReplaceInvalidChars" Lude..=) Lude.<$> replaceInvalidChars,
            ("Username" Lude..=) Lude.<$> username,
            ("BucketName" Lude..=) Lude.<$> bucketName,
            ("EncryptionMode" Lude..=) Lude.<$> encryptionMode,
            ("DateFormat" Lude..=) Lude.<$> dateFormat,
            ("RemoveQuotes" Lude..=) Lude.<$> removeQuotes,
            ("Password" Lude..=) Lude.<$> password,
            ("DatabaseName" Lude..=) Lude.<$> databaseName,
            ("AcceptAnyDate" Lude..=) Lude.<$> acceptAnyDate,
            ("AfterConnectScript" Lude..=) Lude.<$> afterConnectScript,
            ("WriteBufferSize" Lude..=) Lude.<$> writeBufferSize,
            ("CompUpdate" Lude..=) Lude.<$> compUpdate,
            ("TrimBlanks" Lude..=) Lude.<$> trimBlanks,
            ("TimeFormat" Lude..=) Lude.<$> timeFormat,
            ("ServerSideEncryptionKmsKeyId" Lude..=)
              Lude.<$> serverSideEncryptionKMSKeyId,
            ("Port" Lude..=) Lude.<$> port,
            ("FileTransferUploadStreams" Lude..=)
              Lude.<$> fileTransferUploadStreams
          ]
      )

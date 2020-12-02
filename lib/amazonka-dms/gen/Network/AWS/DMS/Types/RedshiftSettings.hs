{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.RedshiftSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RedshiftSettings where

import Network.AWS.DMS.Types.EncryptionModeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information that defines an Amazon Redshift endpoint.
--
--
--
-- /See:/ 'redshiftSettings' smart constructor.
data RedshiftSettings = RedshiftSettings'
  { _rsEmptyAsNull ::
      !(Maybe Bool),
    _rsCaseSensitiveNames :: !(Maybe Bool),
    _rsMaxFileSize :: !(Maybe Int),
    _rsReplaceChars :: !(Maybe Text),
    _rsServerName :: !(Maybe Text),
    _rsConnectionTimeout :: !(Maybe Int),
    _rsLoadTimeout :: !(Maybe Int),
    _rsServiceAccessRoleARN :: !(Maybe Text),
    _rsExplicitIds :: !(Maybe Bool),
    _rsBucketFolder :: !(Maybe Text),
    _rsTruncateColumns :: !(Maybe Bool),
    _rsReplaceInvalidChars :: !(Maybe Text),
    _rsUsername :: !(Maybe Text),
    _rsBucketName :: !(Maybe Text),
    _rsEncryptionMode :: !(Maybe EncryptionModeValue),
    _rsDateFormat :: !(Maybe Text),
    _rsRemoveQuotes :: !(Maybe Bool),
    _rsPassword :: !(Maybe (Sensitive Text)),
    _rsDatabaseName :: !(Maybe Text),
    _rsAcceptAnyDate :: !(Maybe Bool),
    _rsAfterConnectScript :: !(Maybe Text),
    _rsWriteBufferSize :: !(Maybe Int),
    _rsCompUpdate :: !(Maybe Bool),
    _rsTrimBlanks :: !(Maybe Bool),
    _rsTimeFormat :: !(Maybe Text),
    _rsServerSideEncryptionKMSKeyId :: !(Maybe Text),
    _rsPort :: !(Maybe Int),
    _rsFileTransferUploadStreams :: !(Maybe Int)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'RedshiftSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsEmptyAsNull' - A value that specifies whether AWS DMS should migrate empty CHAR and VARCHAR fields as NULL. A value of @true@ sets empty CHAR and VARCHAR fields to null. The default is @false@ .
--
-- * 'rsCaseSensitiveNames' - If Amazon Redshift is configured to support case sensitive schema names, set @CaseSensitiveNames@ to @true@ . The default is @false@ .
--
-- * 'rsMaxFileSize' - The maximum size (in KB) of any .csv file used to load data on an S3 bucket and transfer data to Amazon Redshift. It defaults to 1048576KB (1 GB).
--
-- * 'rsReplaceChars' - A value that specifies to replaces the invalid characters specified in @ReplaceInvalidChars@ , substituting the specified characters instead. The default is @"?"@ .
--
-- * 'rsServerName' - The name of the Amazon Redshift cluster you are using.
--
-- * 'rsConnectionTimeout' - A value that sets the amount of time to wait (in milliseconds) before timing out, beginning from when you initially establish a connection.
--
-- * 'rsLoadTimeout' - The amount of time to wait (in milliseconds) before timing out of operations performed by AWS DMS on a Redshift cluster, such as Redshift COPY, INSERT, DELETE, and UPDATE.
--
-- * 'rsServiceAccessRoleARN' - The Amazon Resource Name (ARN) of the IAM role that has access to the Amazon Redshift service.
--
-- * 'rsExplicitIds' - This setting is only valid for a full-load migration task. Set @ExplicitIds@ to @true@ to have tables with @IDENTITY@ columns override their auto-generated values with explicit values loaded from the source data files used to populate the tables. The default is @false@ .
--
-- * 'rsBucketFolder' - An S3 folder where the comma-separated-value (.csv) files are stored before being uploaded to the target Redshift cluster.  For full load mode, AWS DMS converts source records into .csv files and loads them to the /BucketFolder\/TableID/ path. AWS DMS uses the Redshift @COPY@ command to upload the .csv files to the target table. The files are deleted once the @COPY@ operation has finished. For more information, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift Database Developer Guide>  For change-data-capture (CDC) mode, AWS DMS creates a /NetChanges/ table, and loads the .csv files to this /BucketFolder\/NetChangesTableID/ path.
--
-- * 'rsTruncateColumns' - A value that specifies to truncate data in columns to the appropriate number of characters, so that the data fits in the column. This parameter applies only to columns with a VARCHAR or CHAR data type, and rows with a size of 4 MB or less. Choose @true@ to truncate data. The default is @false@ .
--
-- * 'rsReplaceInvalidChars' - A list of characters that you want to replace. Use with @ReplaceChars@ .
--
-- * 'rsUsername' - An Amazon Redshift user name for a registered user.
--
-- * 'rsBucketName' - The name of the intermediate S3 bucket used to store .csv files before uploading data to Redshift.
--
-- * 'rsEncryptionMode' - The type of server-side encryption that you want to use for your data. This encryption type is part of the endpoint settings or the extra connections attributes for Amazon S3. You can choose either @SSE_S3@ (the default) or @SSE_KMS@ .  To use @SSE_S3@ , create an AWS Identity and Access Management (IAM) role with a policy that allows @"arn:aws:s3:::*"@ to use the following actions: @"s3:PutObject", "s3:ListBucket"@
--
-- * 'rsDateFormat' - The date format that you are using. Valid values are @auto@ (case-sensitive), your date format string enclosed in quotes, or NULL. If this parameter is left unset (NULL), it defaults to a format of 'YYYY-MM-DD'. Using @auto@ recognizes most strings, even some that aren't supported when you use a date format string.  If your date and time values use formats different from each other, set this to @auto@ .
--
-- * 'rsRemoveQuotes' - A value that specifies to remove surrounding quotation marks from strings in the incoming data. All characters within the quotation marks, including delimiters, are retained. Choose @true@ to remove quotation marks. The default is @false@ .
--
-- * 'rsPassword' - The password for the user named in the @username@ property.
--
-- * 'rsDatabaseName' - The name of the Amazon Redshift data warehouse (service) that you are working with.
--
-- * 'rsAcceptAnyDate' - A value that indicates to allow any date format, including invalid formats such as 00/00/00 00:00:00, to be loaded without generating an error. You can choose @true@ or @false@ (the default). This parameter applies only to TIMESTAMP and DATE columns. Always use ACCEPTANYDATE with the DATEFORMAT parameter. If the date format for the data doesn't match the DATEFORMAT specification, Amazon Redshift inserts a NULL value into that field.
--
-- * 'rsAfterConnectScript' - Code to run after connecting. This parameter should contain the code itself, not the name of a file containing the code.
--
-- * 'rsWriteBufferSize' - The size (in KB) of the in-memory file write buffer used when generating .csv files on the local disk at the DMS replication instance. The default value is 1000 (buffer size is 1000KB).
--
-- * 'rsCompUpdate' - If you set @CompUpdate@ to @true@ Amazon Redshift applies automatic compression if the table is empty. This applies even if the table columns already have encodings other than @RAW@ . If you set @CompUpdate@ to @false@ , automatic compression is disabled and existing column encodings aren't changed. The default is @true@ .
--
-- * 'rsTrimBlanks' - A value that specifies to remove the trailing white space characters from a VARCHAR string. This parameter applies only to columns with a VARCHAR data type. Choose @true@ to remove unneeded white space. The default is @false@ .
--
-- * 'rsTimeFormat' - The time format that you want to use. Valid values are @auto@ (case-sensitive), @'timeformat_string'@ , @'epochsecs'@ , or @'epochmillisecs'@ . It defaults to 10. Using @auto@ recognizes most strings, even some that aren't supported when you use a time format string.  If your date and time values use formats different from each other, set this parameter to @auto@ .
--
-- * 'rsServerSideEncryptionKMSKeyId' - The AWS KMS key ID. If you are using @SSE_KMS@ for the @EncryptionMode@ , provide this key ID. The key that you use needs an attached policy that enables IAM user permissions and allows use of the key.
--
-- * 'rsPort' - The port number for Amazon Redshift. The default value is 5439.
--
-- * 'rsFileTransferUploadStreams' - The number of threads used to upload a single file. This parameter accepts a value from 1 through 64. It defaults to 10. The number of parallel streams used to upload a single .csv file to an S3 bucket using S3 Multipart Upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart upload overview> .  @FileTransferUploadStreams@ accepts a value from 1 through 64. It defaults to 10.
redshiftSettings ::
  RedshiftSettings
redshiftSettings =
  RedshiftSettings'
    { _rsEmptyAsNull = Nothing,
      _rsCaseSensitiveNames = Nothing,
      _rsMaxFileSize = Nothing,
      _rsReplaceChars = Nothing,
      _rsServerName = Nothing,
      _rsConnectionTimeout = Nothing,
      _rsLoadTimeout = Nothing,
      _rsServiceAccessRoleARN = Nothing,
      _rsExplicitIds = Nothing,
      _rsBucketFolder = Nothing,
      _rsTruncateColumns = Nothing,
      _rsReplaceInvalidChars = Nothing,
      _rsUsername = Nothing,
      _rsBucketName = Nothing,
      _rsEncryptionMode = Nothing,
      _rsDateFormat = Nothing,
      _rsRemoveQuotes = Nothing,
      _rsPassword = Nothing,
      _rsDatabaseName = Nothing,
      _rsAcceptAnyDate = Nothing,
      _rsAfterConnectScript = Nothing,
      _rsWriteBufferSize = Nothing,
      _rsCompUpdate = Nothing,
      _rsTrimBlanks = Nothing,
      _rsTimeFormat = Nothing,
      _rsServerSideEncryptionKMSKeyId = Nothing,
      _rsPort = Nothing,
      _rsFileTransferUploadStreams = Nothing
    }

-- | A value that specifies whether AWS DMS should migrate empty CHAR and VARCHAR fields as NULL. A value of @true@ sets empty CHAR and VARCHAR fields to null. The default is @false@ .
rsEmptyAsNull :: Lens' RedshiftSettings (Maybe Bool)
rsEmptyAsNull = lens _rsEmptyAsNull (\s a -> s {_rsEmptyAsNull = a})

-- | If Amazon Redshift is configured to support case sensitive schema names, set @CaseSensitiveNames@ to @true@ . The default is @false@ .
rsCaseSensitiveNames :: Lens' RedshiftSettings (Maybe Bool)
rsCaseSensitiveNames = lens _rsCaseSensitiveNames (\s a -> s {_rsCaseSensitiveNames = a})

-- | The maximum size (in KB) of any .csv file used to load data on an S3 bucket and transfer data to Amazon Redshift. It defaults to 1048576KB (1 GB).
rsMaxFileSize :: Lens' RedshiftSettings (Maybe Int)
rsMaxFileSize = lens _rsMaxFileSize (\s a -> s {_rsMaxFileSize = a})

-- | A value that specifies to replaces the invalid characters specified in @ReplaceInvalidChars@ , substituting the specified characters instead. The default is @"?"@ .
rsReplaceChars :: Lens' RedshiftSettings (Maybe Text)
rsReplaceChars = lens _rsReplaceChars (\s a -> s {_rsReplaceChars = a})

-- | The name of the Amazon Redshift cluster you are using.
rsServerName :: Lens' RedshiftSettings (Maybe Text)
rsServerName = lens _rsServerName (\s a -> s {_rsServerName = a})

-- | A value that sets the amount of time to wait (in milliseconds) before timing out, beginning from when you initially establish a connection.
rsConnectionTimeout :: Lens' RedshiftSettings (Maybe Int)
rsConnectionTimeout = lens _rsConnectionTimeout (\s a -> s {_rsConnectionTimeout = a})

-- | The amount of time to wait (in milliseconds) before timing out of operations performed by AWS DMS on a Redshift cluster, such as Redshift COPY, INSERT, DELETE, and UPDATE.
rsLoadTimeout :: Lens' RedshiftSettings (Maybe Int)
rsLoadTimeout = lens _rsLoadTimeout (\s a -> s {_rsLoadTimeout = a})

-- | The Amazon Resource Name (ARN) of the IAM role that has access to the Amazon Redshift service.
rsServiceAccessRoleARN :: Lens' RedshiftSettings (Maybe Text)
rsServiceAccessRoleARN = lens _rsServiceAccessRoleARN (\s a -> s {_rsServiceAccessRoleARN = a})

-- | This setting is only valid for a full-load migration task. Set @ExplicitIds@ to @true@ to have tables with @IDENTITY@ columns override their auto-generated values with explicit values loaded from the source data files used to populate the tables. The default is @false@ .
rsExplicitIds :: Lens' RedshiftSettings (Maybe Bool)
rsExplicitIds = lens _rsExplicitIds (\s a -> s {_rsExplicitIds = a})

-- | An S3 folder where the comma-separated-value (.csv) files are stored before being uploaded to the target Redshift cluster.  For full load mode, AWS DMS converts source records into .csv files and loads them to the /BucketFolder\/TableID/ path. AWS DMS uses the Redshift @COPY@ command to upload the .csv files to the target table. The files are deleted once the @COPY@ operation has finished. For more information, see <https://docs.aws.amazon.com/redshift/latest/dg/r_COPY.html Amazon Redshift Database Developer Guide>  For change-data-capture (CDC) mode, AWS DMS creates a /NetChanges/ table, and loads the .csv files to this /BucketFolder\/NetChangesTableID/ path.
rsBucketFolder :: Lens' RedshiftSettings (Maybe Text)
rsBucketFolder = lens _rsBucketFolder (\s a -> s {_rsBucketFolder = a})

-- | A value that specifies to truncate data in columns to the appropriate number of characters, so that the data fits in the column. This parameter applies only to columns with a VARCHAR or CHAR data type, and rows with a size of 4 MB or less. Choose @true@ to truncate data. The default is @false@ .
rsTruncateColumns :: Lens' RedshiftSettings (Maybe Bool)
rsTruncateColumns = lens _rsTruncateColumns (\s a -> s {_rsTruncateColumns = a})

-- | A list of characters that you want to replace. Use with @ReplaceChars@ .
rsReplaceInvalidChars :: Lens' RedshiftSettings (Maybe Text)
rsReplaceInvalidChars = lens _rsReplaceInvalidChars (\s a -> s {_rsReplaceInvalidChars = a})

-- | An Amazon Redshift user name for a registered user.
rsUsername :: Lens' RedshiftSettings (Maybe Text)
rsUsername = lens _rsUsername (\s a -> s {_rsUsername = a})

-- | The name of the intermediate S3 bucket used to store .csv files before uploading data to Redshift.
rsBucketName :: Lens' RedshiftSettings (Maybe Text)
rsBucketName = lens _rsBucketName (\s a -> s {_rsBucketName = a})

-- | The type of server-side encryption that you want to use for your data. This encryption type is part of the endpoint settings or the extra connections attributes for Amazon S3. You can choose either @SSE_S3@ (the default) or @SSE_KMS@ .  To use @SSE_S3@ , create an AWS Identity and Access Management (IAM) role with a policy that allows @"arn:aws:s3:::*"@ to use the following actions: @"s3:PutObject", "s3:ListBucket"@
rsEncryptionMode :: Lens' RedshiftSettings (Maybe EncryptionModeValue)
rsEncryptionMode = lens _rsEncryptionMode (\s a -> s {_rsEncryptionMode = a})

-- | The date format that you are using. Valid values are @auto@ (case-sensitive), your date format string enclosed in quotes, or NULL. If this parameter is left unset (NULL), it defaults to a format of 'YYYY-MM-DD'. Using @auto@ recognizes most strings, even some that aren't supported when you use a date format string.  If your date and time values use formats different from each other, set this to @auto@ .
rsDateFormat :: Lens' RedshiftSettings (Maybe Text)
rsDateFormat = lens _rsDateFormat (\s a -> s {_rsDateFormat = a})

-- | A value that specifies to remove surrounding quotation marks from strings in the incoming data. All characters within the quotation marks, including delimiters, are retained. Choose @true@ to remove quotation marks. The default is @false@ .
rsRemoveQuotes :: Lens' RedshiftSettings (Maybe Bool)
rsRemoveQuotes = lens _rsRemoveQuotes (\s a -> s {_rsRemoveQuotes = a})

-- | The password for the user named in the @username@ property.
rsPassword :: Lens' RedshiftSettings (Maybe Text)
rsPassword = lens _rsPassword (\s a -> s {_rsPassword = a}) . mapping _Sensitive

-- | The name of the Amazon Redshift data warehouse (service) that you are working with.
rsDatabaseName :: Lens' RedshiftSettings (Maybe Text)
rsDatabaseName = lens _rsDatabaseName (\s a -> s {_rsDatabaseName = a})

-- | A value that indicates to allow any date format, including invalid formats such as 00/00/00 00:00:00, to be loaded without generating an error. You can choose @true@ or @false@ (the default). This parameter applies only to TIMESTAMP and DATE columns. Always use ACCEPTANYDATE with the DATEFORMAT parameter. If the date format for the data doesn't match the DATEFORMAT specification, Amazon Redshift inserts a NULL value into that field.
rsAcceptAnyDate :: Lens' RedshiftSettings (Maybe Bool)
rsAcceptAnyDate = lens _rsAcceptAnyDate (\s a -> s {_rsAcceptAnyDate = a})

-- | Code to run after connecting. This parameter should contain the code itself, not the name of a file containing the code.
rsAfterConnectScript :: Lens' RedshiftSettings (Maybe Text)
rsAfterConnectScript = lens _rsAfterConnectScript (\s a -> s {_rsAfterConnectScript = a})

-- | The size (in KB) of the in-memory file write buffer used when generating .csv files on the local disk at the DMS replication instance. The default value is 1000 (buffer size is 1000KB).
rsWriteBufferSize :: Lens' RedshiftSettings (Maybe Int)
rsWriteBufferSize = lens _rsWriteBufferSize (\s a -> s {_rsWriteBufferSize = a})

-- | If you set @CompUpdate@ to @true@ Amazon Redshift applies automatic compression if the table is empty. This applies even if the table columns already have encodings other than @RAW@ . If you set @CompUpdate@ to @false@ , automatic compression is disabled and existing column encodings aren't changed. The default is @true@ .
rsCompUpdate :: Lens' RedshiftSettings (Maybe Bool)
rsCompUpdate = lens _rsCompUpdate (\s a -> s {_rsCompUpdate = a})

-- | A value that specifies to remove the trailing white space characters from a VARCHAR string. This parameter applies only to columns with a VARCHAR data type. Choose @true@ to remove unneeded white space. The default is @false@ .
rsTrimBlanks :: Lens' RedshiftSettings (Maybe Bool)
rsTrimBlanks = lens _rsTrimBlanks (\s a -> s {_rsTrimBlanks = a})

-- | The time format that you want to use. Valid values are @auto@ (case-sensitive), @'timeformat_string'@ , @'epochsecs'@ , or @'epochmillisecs'@ . It defaults to 10. Using @auto@ recognizes most strings, even some that aren't supported when you use a time format string.  If your date and time values use formats different from each other, set this parameter to @auto@ .
rsTimeFormat :: Lens' RedshiftSettings (Maybe Text)
rsTimeFormat = lens _rsTimeFormat (\s a -> s {_rsTimeFormat = a})

-- | The AWS KMS key ID. If you are using @SSE_KMS@ for the @EncryptionMode@ , provide this key ID. The key that you use needs an attached policy that enables IAM user permissions and allows use of the key.
rsServerSideEncryptionKMSKeyId :: Lens' RedshiftSettings (Maybe Text)
rsServerSideEncryptionKMSKeyId = lens _rsServerSideEncryptionKMSKeyId (\s a -> s {_rsServerSideEncryptionKMSKeyId = a})

-- | The port number for Amazon Redshift. The default value is 5439.
rsPort :: Lens' RedshiftSettings (Maybe Int)
rsPort = lens _rsPort (\s a -> s {_rsPort = a})

-- | The number of threads used to upload a single file. This parameter accepts a value from 1 through 64. It defaults to 10. The number of parallel streams used to upload a single .csv file to an S3 bucket using S3 Multipart Upload. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html Multipart upload overview> .  @FileTransferUploadStreams@ accepts a value from 1 through 64. It defaults to 10.
rsFileTransferUploadStreams :: Lens' RedshiftSettings (Maybe Int)
rsFileTransferUploadStreams = lens _rsFileTransferUploadStreams (\s a -> s {_rsFileTransferUploadStreams = a})

instance FromJSON RedshiftSettings where
  parseJSON =
    withObject
      "RedshiftSettings"
      ( \x ->
          RedshiftSettings'
            <$> (x .:? "EmptyAsNull")
            <*> (x .:? "CaseSensitiveNames")
            <*> (x .:? "MaxFileSize")
            <*> (x .:? "ReplaceChars")
            <*> (x .:? "ServerName")
            <*> (x .:? "ConnectionTimeout")
            <*> (x .:? "LoadTimeout")
            <*> (x .:? "ServiceAccessRoleArn")
            <*> (x .:? "ExplicitIds")
            <*> (x .:? "BucketFolder")
            <*> (x .:? "TruncateColumns")
            <*> (x .:? "ReplaceInvalidChars")
            <*> (x .:? "Username")
            <*> (x .:? "BucketName")
            <*> (x .:? "EncryptionMode")
            <*> (x .:? "DateFormat")
            <*> (x .:? "RemoveQuotes")
            <*> (x .:? "Password")
            <*> (x .:? "DatabaseName")
            <*> (x .:? "AcceptAnyDate")
            <*> (x .:? "AfterConnectScript")
            <*> (x .:? "WriteBufferSize")
            <*> (x .:? "CompUpdate")
            <*> (x .:? "TrimBlanks")
            <*> (x .:? "TimeFormat")
            <*> (x .:? "ServerSideEncryptionKmsKeyId")
            <*> (x .:? "Port")
            <*> (x .:? "FileTransferUploadStreams")
      )

instance Hashable RedshiftSettings

instance NFData RedshiftSettings

instance ToJSON RedshiftSettings where
  toJSON RedshiftSettings' {..} =
    object
      ( catMaybes
          [ ("EmptyAsNull" .=) <$> _rsEmptyAsNull,
            ("CaseSensitiveNames" .=) <$> _rsCaseSensitiveNames,
            ("MaxFileSize" .=) <$> _rsMaxFileSize,
            ("ReplaceChars" .=) <$> _rsReplaceChars,
            ("ServerName" .=) <$> _rsServerName,
            ("ConnectionTimeout" .=) <$> _rsConnectionTimeout,
            ("LoadTimeout" .=) <$> _rsLoadTimeout,
            ("ServiceAccessRoleArn" .=) <$> _rsServiceAccessRoleARN,
            ("ExplicitIds" .=) <$> _rsExplicitIds,
            ("BucketFolder" .=) <$> _rsBucketFolder,
            ("TruncateColumns" .=) <$> _rsTruncateColumns,
            ("ReplaceInvalidChars" .=) <$> _rsReplaceInvalidChars,
            ("Username" .=) <$> _rsUsername,
            ("BucketName" .=) <$> _rsBucketName,
            ("EncryptionMode" .=) <$> _rsEncryptionMode,
            ("DateFormat" .=) <$> _rsDateFormat,
            ("RemoveQuotes" .=) <$> _rsRemoveQuotes,
            ("Password" .=) <$> _rsPassword,
            ("DatabaseName" .=) <$> _rsDatabaseName,
            ("AcceptAnyDate" .=) <$> _rsAcceptAnyDate,
            ("AfterConnectScript" .=) <$> _rsAfterConnectScript,
            ("WriteBufferSize" .=) <$> _rsWriteBufferSize,
            ("CompUpdate" .=) <$> _rsCompUpdate,
            ("TrimBlanks" .=) <$> _rsTrimBlanks,
            ("TimeFormat" .=) <$> _rsTimeFormat,
            ("ServerSideEncryptionKmsKeyId" .=)
              <$> _rsServerSideEncryptionKMSKeyId,
            ("Port" .=) <$> _rsPort,
            ("FileTransferUploadStreams" .=) <$> _rsFileTransferUploadStreams
          ]
      )

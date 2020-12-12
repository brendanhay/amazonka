{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.S3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.S3Settings
  ( S3Settings (..),

    -- * Smart constructor
    mkS3Settings,

    -- * Lenses
    ssParquetVersion,
    ssParquetTimestampInMillisecond,
    ssIncludeOpForFullLoad,
    ssCSVDelimiter,
    ssServiceAccessRoleARN,
    ssBucketFolder,
    ssDataFormat,
    ssDatePartitionEnabled,
    ssEncodingType,
    ssExternalTableDefinition,
    ssDictPageSizeLimit,
    ssBucketName,
    ssEncryptionMode,
    ssEnableStatistics,
    ssCdcInsertsOnly,
    ssTimestampColumnName,
    ssCSVRowDelimiter,
    ssDatePartitionDelimiter,
    ssCompressionType,
    ssServerSideEncryptionKMSKeyId,
    ssDataPageSize,
    ssCdcInsertsAndUpdates,
    ssDatePartitionSequence,
    ssRowGroupLength,
  )
where

import Network.AWS.DMS.Types.CompressionTypeValue
import Network.AWS.DMS.Types.DataFormatValue
import Network.AWS.DMS.Types.DatePartitionDelimiterValue
import Network.AWS.DMS.Types.DatePartitionSequenceValue
import Network.AWS.DMS.Types.EncodingTypeValue
import Network.AWS.DMS.Types.EncryptionModeValue
import Network.AWS.DMS.Types.ParquetVersionValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for exporting data to Amazon S3.
--
-- /See:/ 'mkS3Settings' smart constructor.
data S3Settings = S3Settings'
  { parquetVersion ::
      Lude.Maybe ParquetVersionValue,
    parquetTimestampInMillisecond :: Lude.Maybe Lude.Bool,
    includeOpForFullLoad :: Lude.Maybe Lude.Bool,
    csvDelimiter :: Lude.Maybe Lude.Text,
    serviceAccessRoleARN :: Lude.Maybe Lude.Text,
    bucketFolder :: Lude.Maybe Lude.Text,
    dataFormat :: Lude.Maybe DataFormatValue,
    datePartitionEnabled :: Lude.Maybe Lude.Bool,
    encodingType :: Lude.Maybe EncodingTypeValue,
    externalTableDefinition :: Lude.Maybe Lude.Text,
    dictPageSizeLimit :: Lude.Maybe Lude.Int,
    bucketName :: Lude.Maybe Lude.Text,
    encryptionMode :: Lude.Maybe EncryptionModeValue,
    enableStatistics :: Lude.Maybe Lude.Bool,
    cdcInsertsOnly :: Lude.Maybe Lude.Bool,
    timestampColumnName :: Lude.Maybe Lude.Text,
    csvRowDelimiter :: Lude.Maybe Lude.Text,
    datePartitionDelimiter :: Lude.Maybe DatePartitionDelimiterValue,
    compressionType :: Lude.Maybe CompressionTypeValue,
    serverSideEncryptionKMSKeyId :: Lude.Maybe Lude.Text,
    dataPageSize :: Lude.Maybe Lude.Int,
    cdcInsertsAndUpdates :: Lude.Maybe Lude.Bool,
    datePartitionSequence :: Lude.Maybe DatePartitionSequenceValue,
    rowGroupLength :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Settings' with the minimum fields required to make a request.
--
-- * 'bucketFolder' - An optional parameter to set a folder name in the S3 bucket. If provided, tables are created in the path @/bucketFolder/ //schema_name/ //table_name/ /@ . If this parameter isn't specified, then the path used is @/schema_name/ //table_name/ /@ .
-- * 'bucketName' - The name of the S3 bucket.
-- * 'cdcInsertsAndUpdates' - A value that enables a change data capture (CDC) load to write INSERT and UPDATE operations to .csv or .parquet (columnar storage) output files. The default setting is @false@ , but when @CdcInsertsAndUpdates@ is set to @true@ or @y@ , only INSERTs and UPDATEs from the source database are migrated to the .csv or .parquet file.
--
-- For .csv file format only, how these INSERTs and UPDATEs are recorded depends on the value of the @IncludeOpForFullLoad@ parameter. If @IncludeOpForFullLoad@ is set to @true@ , the first field of every CDC record is set to either @I@ or @U@ to indicate INSERT and UPDATE operations at the source. But if @IncludeOpForFullLoad@ is set to @false@ , CDC records are written without an indication of INSERT or UPDATE operations at the source. For more information about how these settings work together, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data> in the /AWS Database Migration Service User Guide./ .
-- * 'cdcInsertsOnly' - A value that enables a change data capture (CDC) load to write only INSERT operations to .csv or columnar storage (.parquet) output files. By default (the @false@ setting), the first field in a .csv or .parquet record contains the letter I (INSERT), U (UPDATE), or D (DELETE). These values indicate whether the row was inserted, updated, or deleted at the source database for a CDC load to the target.
--
-- If @CdcInsertsOnly@ is set to @true@ or @y@ , only INSERTs from the source database are migrated to the .csv or .parquet file. For .csv format only, how these INSERTs are recorded depends on the value of @IncludeOpForFullLoad@ . If @IncludeOpForFullLoad@ is set to @true@ , the first field of every CDC record is set to I to indicate the INSERT operation at the source. If @IncludeOpForFullLoad@ is set to @false@ , every CDC record is written without a first field to indicate the INSERT operation at the source. For more information about how these settings work together, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data> in the /AWS Database Migration Service User Guide./ .
-- * 'compressionType' - An optional parameter to use GZIP to compress the target files. Set to GZIP to compress the target files. Either set this parameter to NONE (the default) or don't use it to leave the files uncompressed. This parameter applies to both .csv and .parquet file formats.
-- * 'csvDelimiter' - The delimiter used to separate columns in the .csv file for both source and target. The default is a comma.
-- * 'csvRowDelimiter' - The delimiter used to separate rows in the .csv file for both source and target. The default is a carriage return (@\n@ ).
-- * 'dataFormat' - The format of the data that you want to use for output. You can choose one of the following:
--
--
--     * @csv@ : This is a row-based file format with comma-separated values (.csv).
--
--
--     * @parquet@ : Apache Parquet (.parquet) is a columnar storage file format that features efficient compression and provides faster query response.
--
--
-- * 'dataPageSize' - The size of one data page in bytes. This parameter defaults to 1024 * 1024 bytes (1 MiB). This number is used for .parquet file format only.
-- * 'datePartitionDelimiter' - Specifies a date separating delimiter to use during folder partitioning. The default value is @SLASH@ . Use this parameter when @DatePartitionedEnabled@ is set to @true@ .
-- * 'datePartitionEnabled' - When set to @true@ , this parameter partitions S3 bucket folders based on transaction commit dates. The default value is @false@ . For more information about date-based folder partitoning, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning> .
-- * 'datePartitionSequence' - Identifies the sequence of the date format to use during folder partitioning. The default value is @YYYYMMDD@ . Use this parameter when @DatePartitionedEnabled@ is set to @true@ .
-- * 'dictPageSizeLimit' - The maximum size of an encoded dictionary page of a column. If the dictionary page exceeds this, this column is stored using an encoding type of @PLAIN@ . This parameter defaults to 1024 * 1024 bytes (1 MiB), the maximum size of a dictionary page before it reverts to @PLAIN@ encoding. This size is used for .parquet file format only.
-- * 'enableStatistics' - A value that enables statistics for Parquet pages and row groups. Choose @true@ to enable statistics, @false@ to disable. Statistics include @NULL@ , @DISTINCT@ , @MAX@ , and @MIN@ values. This parameter defaults to @true@ . This value is used for .parquet file format only.
-- * 'encodingType' - The type of encoding you are using:
--
--
--     * @RLE_DICTIONARY@ uses a combination of bit-packing and run-length encoding to store repeated values more efficiently. This is the default.
--
--
--     * @PLAIN@ doesn't use encoding at all. Values are stored as they are.
--
--
--     * @PLAIN_DICTIONARY@ builds a dictionary of the values encountered in a given column. The dictionary is stored in a dictionary page for each column chunk.
--
--
-- * 'encryptionMode' - The type of server-side encryption that you want to use for your data. This encryption type is part of the endpoint settings or the extra connections attributes for Amazon S3. You can choose either @SSE_S3@ (the default) or @SSE_KMS@ .
--
-- To use @SSE_S3@ , you need an AWS Identity and Access Management (IAM) role with permission to allow @"arn:aws:s3:::dms-*"@ to use the following actions:
--
--     * @s3:CreateBucket@
--
--
--     * @s3:ListBucket@
--
--
--     * @s3:DeleteBucket@
--
--
--     * @s3:GetBucketLocation@
--
--
--     * @s3:GetObject@
--
--
--     * @s3:PutObject@
--
--
--     * @s3:DeleteObject@
--
--
--     * @s3:GetObjectVersion@
--
--
--     * @s3:GetBucketPolicy@
--
--
--     * @s3:PutBucketPolicy@
--
--
--     * @s3:DeleteBucketPolicy@
--
--
-- * 'externalTableDefinition' - Specifies how tables are defined in the S3 source files only.
-- * 'includeOpForFullLoad' - A value that enables a full load to write INSERT operations to the comma-separated value (.csv) output files only to indicate how the rows were added to the source database.
--
-- For full load, records can only be inserted. By default (the @false@ setting), no information is recorded in these output files for a full load to indicate that the rows were inserted at the source database. If @IncludeOpForFullLoad@ is set to @true@ or @y@ , the INSERT is recorded as an I annotation in the first field of the .csv file. This allows the format of your target records from a full load to be consistent with the target records from a CDC load.
-- * 'parquetTimestampInMillisecond' - A value that specifies the precision of any @TIMESTAMP@ column values that are written to an Amazon S3 object file in .parquet format.
--
-- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@ , AWS DMS writes all @TIMESTAMP@ columns in a .parquet formatted file with millisecond precision. Otherwise, DMS writes them with microsecond precision.
-- Currently, Amazon Athena and AWS Glue can handle only millisecond precision for @TIMESTAMP@ values. Set this parameter to @true@ for S3 endpoint object files that are .parquet formatted only if you plan to query or process the data with Athena or AWS Glue.
-- * 'parquetVersion' - The version of the Apache Parquet format that you want to use: @parquet_1_0@ (the default) or @parquet_2_0@ .
-- * 'rowGroupLength' - The number of rows in a row group. A smaller row group size provides faster reads. But as the number of row groups grows, the slower writes become. This parameter defaults to 10,000 rows. This number is used for .parquet file format only.
--
-- If you choose a value larger than the maximum, @RowGroupLength@ is set to the max row group length in bytes (64 * 1024 * 1024).
-- * 'serverSideEncryptionKMSKeyId' - If you are using @SSE_KMS@ for the @EncryptionMode@ , provide the AWS KMS key ID. The key that you use needs an attached policy that enables AWS Identity and Access Management (IAM) user permissions and allows use of the key.
--
-- Here is a CLI example: @aws dms create-endpoint --endpoint-identifier /value/ --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=/value/ ,BucketFolder=/value/ ,BucketName=/value/ ,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=/value/ @
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) used by the service access IAM role. It is a required parameter that enables DMS to write and read objects from an 3S bucket.
-- * 'timestampColumnName' - A value that when nonblank causes AWS DMS to add a column with timestamp information to the endpoint data for an Amazon S3 target.
--
-- DMS includes an additional @STRING@ column in the .csv or .parquet object files of your migrated data when you set @TimestampColumnName@ to a nonblank value.
-- For a full load, each row of this timestamp column contains a timestamp for when the data was transferred from the source to the target by DMS.
-- For a change data capture (CDC) load, each row of the timestamp column contains the timestamp for the commit of that row in the source database.
-- The string format for this timestamp column value is @yyyy-MM-dd HH:mm:ss.SSSSSS@ . By default, the precision of this value is in microseconds. For a CDC load, the rounding of the precision depends on the commit timestamp supported by DMS for the source database.
-- When the @AddColumnName@ parameter is set to @true@ , DMS also includes a name for the timestamp column that you set with @TimestampColumnName@ .
mkS3Settings ::
  S3Settings
mkS3Settings =
  S3Settings'
    { parquetVersion = Lude.Nothing,
      parquetTimestampInMillisecond = Lude.Nothing,
      includeOpForFullLoad = Lude.Nothing,
      csvDelimiter = Lude.Nothing,
      serviceAccessRoleARN = Lude.Nothing,
      bucketFolder = Lude.Nothing,
      dataFormat = Lude.Nothing,
      datePartitionEnabled = Lude.Nothing,
      encodingType = Lude.Nothing,
      externalTableDefinition = Lude.Nothing,
      dictPageSizeLimit = Lude.Nothing,
      bucketName = Lude.Nothing,
      encryptionMode = Lude.Nothing,
      enableStatistics = Lude.Nothing,
      cdcInsertsOnly = Lude.Nothing,
      timestampColumnName = Lude.Nothing,
      csvRowDelimiter = Lude.Nothing,
      datePartitionDelimiter = Lude.Nothing,
      compressionType = Lude.Nothing,
      serverSideEncryptionKMSKeyId = Lude.Nothing,
      dataPageSize = Lude.Nothing,
      cdcInsertsAndUpdates = Lude.Nothing,
      datePartitionSequence = Lude.Nothing,
      rowGroupLength = Lude.Nothing
    }

-- | The version of the Apache Parquet format that you want to use: @parquet_1_0@ (the default) or @parquet_2_0@ .
--
-- /Note:/ Consider using 'parquetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParquetVersion :: Lens.Lens' S3Settings (Lude.Maybe ParquetVersionValue)
ssParquetVersion = Lens.lens (parquetVersion :: S3Settings -> Lude.Maybe ParquetVersionValue) (\s a -> s {parquetVersion = a} :: S3Settings)
{-# DEPRECATED ssParquetVersion "Use generic-lens or generic-optics with 'parquetVersion' instead." #-}

-- | A value that specifies the precision of any @TIMESTAMP@ column values that are written to an Amazon S3 object file in .parquet format.
--
-- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@ , AWS DMS writes all @TIMESTAMP@ columns in a .parquet formatted file with millisecond precision. Otherwise, DMS writes them with microsecond precision.
-- Currently, Amazon Athena and AWS Glue can handle only millisecond precision for @TIMESTAMP@ values. Set this parameter to @true@ for S3 endpoint object files that are .parquet formatted only if you plan to query or process the data with Athena or AWS Glue.
--
-- /Note:/ Consider using 'parquetTimestampInMillisecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParquetTimestampInMillisecond :: Lens.Lens' S3Settings (Lude.Maybe Lude.Bool)
ssParquetTimestampInMillisecond = Lens.lens (parquetTimestampInMillisecond :: S3Settings -> Lude.Maybe Lude.Bool) (\s a -> s {parquetTimestampInMillisecond = a} :: S3Settings)
{-# DEPRECATED ssParquetTimestampInMillisecond "Use generic-lens or generic-optics with 'parquetTimestampInMillisecond' instead." #-}

-- | A value that enables a full load to write INSERT operations to the comma-separated value (.csv) output files only to indicate how the rows were added to the source database.
--
-- For full load, records can only be inserted. By default (the @false@ setting), no information is recorded in these output files for a full load to indicate that the rows were inserted at the source database. If @IncludeOpForFullLoad@ is set to @true@ or @y@ , the INSERT is recorded as an I annotation in the first field of the .csv file. This allows the format of your target records from a full load to be consistent with the target records from a CDC load.
--
-- /Note:/ Consider using 'includeOpForFullLoad' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssIncludeOpForFullLoad :: Lens.Lens' S3Settings (Lude.Maybe Lude.Bool)
ssIncludeOpForFullLoad = Lens.lens (includeOpForFullLoad :: S3Settings -> Lude.Maybe Lude.Bool) (\s a -> s {includeOpForFullLoad = a} :: S3Settings)
{-# DEPRECATED ssIncludeOpForFullLoad "Use generic-lens or generic-optics with 'includeOpForFullLoad' instead." #-}

-- | The delimiter used to separate columns in the .csv file for both source and target. The default is a comma.
--
-- /Note:/ Consider using 'csvDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCSVDelimiter :: Lens.Lens' S3Settings (Lude.Maybe Lude.Text)
ssCSVDelimiter = Lens.lens (csvDelimiter :: S3Settings -> Lude.Maybe Lude.Text) (\s a -> s {csvDelimiter = a} :: S3Settings)
{-# DEPRECATED ssCSVDelimiter "Use generic-lens or generic-optics with 'csvDelimiter' instead." #-}

-- | The Amazon Resource Name (ARN) used by the service access IAM role. It is a required parameter that enables DMS to write and read objects from an 3S bucket.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssServiceAccessRoleARN :: Lens.Lens' S3Settings (Lude.Maybe Lude.Text)
ssServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: S3Settings -> Lude.Maybe Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: S3Settings)
{-# DEPRECATED ssServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

-- | An optional parameter to set a folder name in the S3 bucket. If provided, tables are created in the path @/bucketFolder/ //schema_name/ //table_name/ /@ . If this parameter isn't specified, then the path used is @/schema_name/ //table_name/ /@ .
--
-- /Note:/ Consider using 'bucketFolder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssBucketFolder :: Lens.Lens' S3Settings (Lude.Maybe Lude.Text)
ssBucketFolder = Lens.lens (bucketFolder :: S3Settings -> Lude.Maybe Lude.Text) (\s a -> s {bucketFolder = a} :: S3Settings)
{-# DEPRECATED ssBucketFolder "Use generic-lens or generic-optics with 'bucketFolder' instead." #-}

-- | The format of the data that you want to use for output. You can choose one of the following:
--
--
--     * @csv@ : This is a row-based file format with comma-separated values (.csv).
--
--
--     * @parquet@ : Apache Parquet (.parquet) is a columnar storage file format that features efficient compression and provides faster query response.
--
--
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDataFormat :: Lens.Lens' S3Settings (Lude.Maybe DataFormatValue)
ssDataFormat = Lens.lens (dataFormat :: S3Settings -> Lude.Maybe DataFormatValue) (\s a -> s {dataFormat = a} :: S3Settings)
{-# DEPRECATED ssDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | When set to @true@ , this parameter partitions S3 bucket folders based on transaction commit dates. The default value is @false@ . For more information about date-based folder partitoning, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning> .
--
-- /Note:/ Consider using 'datePartitionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDatePartitionEnabled :: Lens.Lens' S3Settings (Lude.Maybe Lude.Bool)
ssDatePartitionEnabled = Lens.lens (datePartitionEnabled :: S3Settings -> Lude.Maybe Lude.Bool) (\s a -> s {datePartitionEnabled = a} :: S3Settings)
{-# DEPRECATED ssDatePartitionEnabled "Use generic-lens or generic-optics with 'datePartitionEnabled' instead." #-}

-- | The type of encoding you are using:
--
--
--     * @RLE_DICTIONARY@ uses a combination of bit-packing and run-length encoding to store repeated values more efficiently. This is the default.
--
--
--     * @PLAIN@ doesn't use encoding at all. Values are stored as they are.
--
--
--     * @PLAIN_DICTIONARY@ builds a dictionary of the values encountered in a given column. The dictionary is stored in a dictionary page for each column chunk.
--
--
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEncodingType :: Lens.Lens' S3Settings (Lude.Maybe EncodingTypeValue)
ssEncodingType = Lens.lens (encodingType :: S3Settings -> Lude.Maybe EncodingTypeValue) (\s a -> s {encodingType = a} :: S3Settings)
{-# DEPRECATED ssEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | Specifies how tables are defined in the S3 source files only.
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssExternalTableDefinition :: Lens.Lens' S3Settings (Lude.Maybe Lude.Text)
ssExternalTableDefinition = Lens.lens (externalTableDefinition :: S3Settings -> Lude.Maybe Lude.Text) (\s a -> s {externalTableDefinition = a} :: S3Settings)
{-# DEPRECATED ssExternalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead." #-}

-- | The maximum size of an encoded dictionary page of a column. If the dictionary page exceeds this, this column is stored using an encoding type of @PLAIN@ . This parameter defaults to 1024 * 1024 bytes (1 MiB), the maximum size of a dictionary page before it reverts to @PLAIN@ encoding. This size is used for .parquet file format only.
--
-- /Note:/ Consider using 'dictPageSizeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDictPageSizeLimit :: Lens.Lens' S3Settings (Lude.Maybe Lude.Int)
ssDictPageSizeLimit = Lens.lens (dictPageSizeLimit :: S3Settings -> Lude.Maybe Lude.Int) (\s a -> s {dictPageSizeLimit = a} :: S3Settings)
{-# DEPRECATED ssDictPageSizeLimit "Use generic-lens or generic-optics with 'dictPageSizeLimit' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssBucketName :: Lens.Lens' S3Settings (Lude.Maybe Lude.Text)
ssBucketName = Lens.lens (bucketName :: S3Settings -> Lude.Maybe Lude.Text) (\s a -> s {bucketName = a} :: S3Settings)
{-# DEPRECATED ssBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The type of server-side encryption that you want to use for your data. This encryption type is part of the endpoint settings or the extra connections attributes for Amazon S3. You can choose either @SSE_S3@ (the default) or @SSE_KMS@ .
--
-- To use @SSE_S3@ , you need an AWS Identity and Access Management (IAM) role with permission to allow @"arn:aws:s3:::dms-*"@ to use the following actions:
--
--     * @s3:CreateBucket@
--
--
--     * @s3:ListBucket@
--
--
--     * @s3:DeleteBucket@
--
--
--     * @s3:GetBucketLocation@
--
--
--     * @s3:GetObject@
--
--
--     * @s3:PutObject@
--
--
--     * @s3:DeleteObject@
--
--
--     * @s3:GetObjectVersion@
--
--
--     * @s3:GetBucketPolicy@
--
--
--     * @s3:PutBucketPolicy@
--
--
--     * @s3:DeleteBucketPolicy@
--
--
--
-- /Note:/ Consider using 'encryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEncryptionMode :: Lens.Lens' S3Settings (Lude.Maybe EncryptionModeValue)
ssEncryptionMode = Lens.lens (encryptionMode :: S3Settings -> Lude.Maybe EncryptionModeValue) (\s a -> s {encryptionMode = a} :: S3Settings)
{-# DEPRECATED ssEncryptionMode "Use generic-lens or generic-optics with 'encryptionMode' instead." #-}

-- | A value that enables statistics for Parquet pages and row groups. Choose @true@ to enable statistics, @false@ to disable. Statistics include @NULL@ , @DISTINCT@ , @MAX@ , and @MIN@ values. This parameter defaults to @true@ . This value is used for .parquet file format only.
--
-- /Note:/ Consider using 'enableStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEnableStatistics :: Lens.Lens' S3Settings (Lude.Maybe Lude.Bool)
ssEnableStatistics = Lens.lens (enableStatistics :: S3Settings -> Lude.Maybe Lude.Bool) (\s a -> s {enableStatistics = a} :: S3Settings)
{-# DEPRECATED ssEnableStatistics "Use generic-lens or generic-optics with 'enableStatistics' instead." #-}

-- | A value that enables a change data capture (CDC) load to write only INSERT operations to .csv or columnar storage (.parquet) output files. By default (the @false@ setting), the first field in a .csv or .parquet record contains the letter I (INSERT), U (UPDATE), or D (DELETE). These values indicate whether the row was inserted, updated, or deleted at the source database for a CDC load to the target.
--
-- If @CdcInsertsOnly@ is set to @true@ or @y@ , only INSERTs from the source database are migrated to the .csv or .parquet file. For .csv format only, how these INSERTs are recorded depends on the value of @IncludeOpForFullLoad@ . If @IncludeOpForFullLoad@ is set to @true@ , the first field of every CDC record is set to I to indicate the INSERT operation at the source. If @IncludeOpForFullLoad@ is set to @false@ , every CDC record is written without a first field to indicate the INSERT operation at the source. For more information about how these settings work together, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data> in the /AWS Database Migration Service User Guide./ .
--
-- /Note:/ Consider using 'cdcInsertsOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCdcInsertsOnly :: Lens.Lens' S3Settings (Lude.Maybe Lude.Bool)
ssCdcInsertsOnly = Lens.lens (cdcInsertsOnly :: S3Settings -> Lude.Maybe Lude.Bool) (\s a -> s {cdcInsertsOnly = a} :: S3Settings)
{-# DEPRECATED ssCdcInsertsOnly "Use generic-lens or generic-optics with 'cdcInsertsOnly' instead." #-}

-- | A value that when nonblank causes AWS DMS to add a column with timestamp information to the endpoint data for an Amazon S3 target.
--
-- DMS includes an additional @STRING@ column in the .csv or .parquet object files of your migrated data when you set @TimestampColumnName@ to a nonblank value.
-- For a full load, each row of this timestamp column contains a timestamp for when the data was transferred from the source to the target by DMS.
-- For a change data capture (CDC) load, each row of the timestamp column contains the timestamp for the commit of that row in the source database.
-- The string format for this timestamp column value is @yyyy-MM-dd HH:mm:ss.SSSSSS@ . By default, the precision of this value is in microseconds. For a CDC load, the rounding of the precision depends on the commit timestamp supported by DMS for the source database.
-- When the @AddColumnName@ parameter is set to @true@ , DMS also includes a name for the timestamp column that you set with @TimestampColumnName@ .
--
-- /Note:/ Consider using 'timestampColumnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTimestampColumnName :: Lens.Lens' S3Settings (Lude.Maybe Lude.Text)
ssTimestampColumnName = Lens.lens (timestampColumnName :: S3Settings -> Lude.Maybe Lude.Text) (\s a -> s {timestampColumnName = a} :: S3Settings)
{-# DEPRECATED ssTimestampColumnName "Use generic-lens or generic-optics with 'timestampColumnName' instead." #-}

-- | The delimiter used to separate rows in the .csv file for both source and target. The default is a carriage return (@\n@ ).
--
-- /Note:/ Consider using 'csvRowDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCSVRowDelimiter :: Lens.Lens' S3Settings (Lude.Maybe Lude.Text)
ssCSVRowDelimiter = Lens.lens (csvRowDelimiter :: S3Settings -> Lude.Maybe Lude.Text) (\s a -> s {csvRowDelimiter = a} :: S3Settings)
{-# DEPRECATED ssCSVRowDelimiter "Use generic-lens or generic-optics with 'csvRowDelimiter' instead." #-}

-- | Specifies a date separating delimiter to use during folder partitioning. The default value is @SLASH@ . Use this parameter when @DatePartitionedEnabled@ is set to @true@ .
--
-- /Note:/ Consider using 'datePartitionDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDatePartitionDelimiter :: Lens.Lens' S3Settings (Lude.Maybe DatePartitionDelimiterValue)
ssDatePartitionDelimiter = Lens.lens (datePartitionDelimiter :: S3Settings -> Lude.Maybe DatePartitionDelimiterValue) (\s a -> s {datePartitionDelimiter = a} :: S3Settings)
{-# DEPRECATED ssDatePartitionDelimiter "Use generic-lens or generic-optics with 'datePartitionDelimiter' instead." #-}

-- | An optional parameter to use GZIP to compress the target files. Set to GZIP to compress the target files. Either set this parameter to NONE (the default) or don't use it to leave the files uncompressed. This parameter applies to both .csv and .parquet file formats.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCompressionType :: Lens.Lens' S3Settings (Lude.Maybe CompressionTypeValue)
ssCompressionType = Lens.lens (compressionType :: S3Settings -> Lude.Maybe CompressionTypeValue) (\s a -> s {compressionType = a} :: S3Settings)
{-# DEPRECATED ssCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

-- | If you are using @SSE_KMS@ for the @EncryptionMode@ , provide the AWS KMS key ID. The key that you use needs an attached policy that enables AWS Identity and Access Management (IAM) user permissions and allows use of the key.
--
-- Here is a CLI example: @aws dms create-endpoint --endpoint-identifier /value/ --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=/value/ ,BucketFolder=/value/ ,BucketName=/value/ ,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=/value/ @
--
-- /Note:/ Consider using 'serverSideEncryptionKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssServerSideEncryptionKMSKeyId :: Lens.Lens' S3Settings (Lude.Maybe Lude.Text)
ssServerSideEncryptionKMSKeyId = Lens.lens (serverSideEncryptionKMSKeyId :: S3Settings -> Lude.Maybe Lude.Text) (\s a -> s {serverSideEncryptionKMSKeyId = a} :: S3Settings)
{-# DEPRECATED ssServerSideEncryptionKMSKeyId "Use generic-lens or generic-optics with 'serverSideEncryptionKMSKeyId' instead." #-}

-- | The size of one data page in bytes. This parameter defaults to 1024 * 1024 bytes (1 MiB). This number is used for .parquet file format only.
--
-- /Note:/ Consider using 'dataPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDataPageSize :: Lens.Lens' S3Settings (Lude.Maybe Lude.Int)
ssDataPageSize = Lens.lens (dataPageSize :: S3Settings -> Lude.Maybe Lude.Int) (\s a -> s {dataPageSize = a} :: S3Settings)
{-# DEPRECATED ssDataPageSize "Use generic-lens or generic-optics with 'dataPageSize' instead." #-}

-- | A value that enables a change data capture (CDC) load to write INSERT and UPDATE operations to .csv or .parquet (columnar storage) output files. The default setting is @false@ , but when @CdcInsertsAndUpdates@ is set to @true@ or @y@ , only INSERTs and UPDATEs from the source database are migrated to the .csv or .parquet file.
--
-- For .csv file format only, how these INSERTs and UPDATEs are recorded depends on the value of the @IncludeOpForFullLoad@ parameter. If @IncludeOpForFullLoad@ is set to @true@ , the first field of every CDC record is set to either @I@ or @U@ to indicate INSERT and UPDATE operations at the source. But if @IncludeOpForFullLoad@ is set to @false@ , CDC records are written without an indication of INSERT or UPDATE operations at the source. For more information about how these settings work together, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data> in the /AWS Database Migration Service User Guide./ .
--
-- /Note:/ Consider using 'cdcInsertsAndUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCdcInsertsAndUpdates :: Lens.Lens' S3Settings (Lude.Maybe Lude.Bool)
ssCdcInsertsAndUpdates = Lens.lens (cdcInsertsAndUpdates :: S3Settings -> Lude.Maybe Lude.Bool) (\s a -> s {cdcInsertsAndUpdates = a} :: S3Settings)
{-# DEPRECATED ssCdcInsertsAndUpdates "Use generic-lens or generic-optics with 'cdcInsertsAndUpdates' instead." #-}

-- | Identifies the sequence of the date format to use during folder partitioning. The default value is @YYYYMMDD@ . Use this parameter when @DatePartitionedEnabled@ is set to @true@ .
--
-- /Note:/ Consider using 'datePartitionSequence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDatePartitionSequence :: Lens.Lens' S3Settings (Lude.Maybe DatePartitionSequenceValue)
ssDatePartitionSequence = Lens.lens (datePartitionSequence :: S3Settings -> Lude.Maybe DatePartitionSequenceValue) (\s a -> s {datePartitionSequence = a} :: S3Settings)
{-# DEPRECATED ssDatePartitionSequence "Use generic-lens or generic-optics with 'datePartitionSequence' instead." #-}

-- | The number of rows in a row group. A smaller row group size provides faster reads. But as the number of row groups grows, the slower writes become. This parameter defaults to 10,000 rows. This number is used for .parquet file format only.
--
-- If you choose a value larger than the maximum, @RowGroupLength@ is set to the max row group length in bytes (64 * 1024 * 1024).
--
-- /Note:/ Consider using 'rowGroupLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRowGroupLength :: Lens.Lens' S3Settings (Lude.Maybe Lude.Int)
ssRowGroupLength = Lens.lens (rowGroupLength :: S3Settings -> Lude.Maybe Lude.Int) (\s a -> s {rowGroupLength = a} :: S3Settings)
{-# DEPRECATED ssRowGroupLength "Use generic-lens or generic-optics with 'rowGroupLength' instead." #-}

instance Lude.FromJSON S3Settings where
  parseJSON =
    Lude.withObject
      "S3Settings"
      ( \x ->
          S3Settings'
            Lude.<$> (x Lude..:? "ParquetVersion")
            Lude.<*> (x Lude..:? "ParquetTimestampInMillisecond")
            Lude.<*> (x Lude..:? "IncludeOpForFullLoad")
            Lude.<*> (x Lude..:? "CsvDelimiter")
            Lude.<*> (x Lude..:? "ServiceAccessRoleArn")
            Lude.<*> (x Lude..:? "BucketFolder")
            Lude.<*> (x Lude..:? "DataFormat")
            Lude.<*> (x Lude..:? "DatePartitionEnabled")
            Lude.<*> (x Lude..:? "EncodingType")
            Lude.<*> (x Lude..:? "ExternalTableDefinition")
            Lude.<*> (x Lude..:? "DictPageSizeLimit")
            Lude.<*> (x Lude..:? "BucketName")
            Lude.<*> (x Lude..:? "EncryptionMode")
            Lude.<*> (x Lude..:? "EnableStatistics")
            Lude.<*> (x Lude..:? "CdcInsertsOnly")
            Lude.<*> (x Lude..:? "TimestampColumnName")
            Lude.<*> (x Lude..:? "CsvRowDelimiter")
            Lude.<*> (x Lude..:? "DatePartitionDelimiter")
            Lude.<*> (x Lude..:? "CompressionType")
            Lude.<*> (x Lude..:? "ServerSideEncryptionKmsKeyId")
            Lude.<*> (x Lude..:? "DataPageSize")
            Lude.<*> (x Lude..:? "CdcInsertsAndUpdates")
            Lude.<*> (x Lude..:? "DatePartitionSequence")
            Lude.<*> (x Lude..:? "RowGroupLength")
      )

instance Lude.ToJSON S3Settings where
  toJSON S3Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParquetVersion" Lude..=) Lude.<$> parquetVersion,
            ("ParquetTimestampInMillisecond" Lude..=)
              Lude.<$> parquetTimestampInMillisecond,
            ("IncludeOpForFullLoad" Lude..=) Lude.<$> includeOpForFullLoad,
            ("CsvDelimiter" Lude..=) Lude.<$> csvDelimiter,
            ("ServiceAccessRoleArn" Lude..=) Lude.<$> serviceAccessRoleARN,
            ("BucketFolder" Lude..=) Lude.<$> bucketFolder,
            ("DataFormat" Lude..=) Lude.<$> dataFormat,
            ("DatePartitionEnabled" Lude..=) Lude.<$> datePartitionEnabled,
            ("EncodingType" Lude..=) Lude.<$> encodingType,
            ("ExternalTableDefinition" Lude..=)
              Lude.<$> externalTableDefinition,
            ("DictPageSizeLimit" Lude..=) Lude.<$> dictPageSizeLimit,
            ("BucketName" Lude..=) Lude.<$> bucketName,
            ("EncryptionMode" Lude..=) Lude.<$> encryptionMode,
            ("EnableStatistics" Lude..=) Lude.<$> enableStatistics,
            ("CdcInsertsOnly" Lude..=) Lude.<$> cdcInsertsOnly,
            ("TimestampColumnName" Lude..=) Lude.<$> timestampColumnName,
            ("CsvRowDelimiter" Lude..=) Lude.<$> csvRowDelimiter,
            ("DatePartitionDelimiter" Lude..=) Lude.<$> datePartitionDelimiter,
            ("CompressionType" Lude..=) Lude.<$> compressionType,
            ("ServerSideEncryptionKmsKeyId" Lude..=)
              Lude.<$> serverSideEncryptionKMSKeyId,
            ("DataPageSize" Lude..=) Lude.<$> dataPageSize,
            ("CdcInsertsAndUpdates" Lude..=) Lude.<$> cdcInsertsAndUpdates,
            ("DatePartitionSequence" Lude..=) Lude.<$> datePartitionSequence,
            ("RowGroupLength" Lude..=) Lude.<$> rowGroupLength
          ]
      )

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
    ssBucketFolder,
    ssBucketName,
    ssCdcInsertsAndUpdates,
    ssCdcInsertsOnly,
    ssCompressionType,
    ssCsvDelimiter,
    ssCsvRowDelimiter,
    ssDataFormat,
    ssDataPageSize,
    ssDatePartitionDelimiter,
    ssDatePartitionEnabled,
    ssDatePartitionSequence,
    ssDictPageSizeLimit,
    ssEnableStatistics,
    ssEncodingType,
    ssEncryptionMode,
    ssExternalTableDefinition,
    ssIncludeOpForFullLoad,
    ssParquetTimestampInMillisecond,
    ssParquetVersion,
    ssRowGroupLength,
    ssServerSideEncryptionKmsKeyId,
    ssServiceAccessRoleArn,
    ssTimestampColumnName,
  )
where

import qualified Network.AWS.DMS.Types.CompressionTypeValue as Types
import qualified Network.AWS.DMS.Types.DataFormatValue as Types
import qualified Network.AWS.DMS.Types.DatePartitionDelimiterValue as Types
import qualified Network.AWS.DMS.Types.DatePartitionSequenceValue as Types
import qualified Network.AWS.DMS.Types.EncodingTypeValue as Types
import qualified Network.AWS.DMS.Types.EncryptionModeValue as Types
import qualified Network.AWS.DMS.Types.ParquetVersionValue as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for exporting data to Amazon S3.
--
-- /See:/ 'mkS3Settings' smart constructor.
data S3Settings = S3Settings'
  { -- | An optional parameter to set a folder name in the S3 bucket. If provided, tables are created in the path @/bucketFolder/ //schema_name/ //table_name/ /@ . If this parameter isn't specified, then the path used is @/schema_name/ //table_name/ /@ .
    bucketFolder :: Core.Maybe Types.String,
    -- | The name of the S3 bucket.
    bucketName :: Core.Maybe Types.String,
    -- | A value that enables a change data capture (CDC) load to write INSERT and UPDATE operations to .csv or .parquet (columnar storage) output files. The default setting is @false@ , but when @CdcInsertsAndUpdates@ is set to @true@ or @y@ , only INSERTs and UPDATEs from the source database are migrated to the .csv or .parquet file.
    --
    -- For .csv file format only, how these INSERTs and UPDATEs are recorded depends on the value of the @IncludeOpForFullLoad@ parameter. If @IncludeOpForFullLoad@ is set to @true@ , the first field of every CDC record is set to either @I@ or @U@ to indicate INSERT and UPDATE operations at the source. But if @IncludeOpForFullLoad@ is set to @false@ , CDC records are written without an indication of INSERT or UPDATE operations at the source. For more information about how these settings work together, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data> in the /AWS Database Migration Service User Guide./ .
    cdcInsertsAndUpdates :: Core.Maybe Core.Bool,
    -- | A value that enables a change data capture (CDC) load to write only INSERT operations to .csv or columnar storage (.parquet) output files. By default (the @false@ setting), the first field in a .csv or .parquet record contains the letter I (INSERT), U (UPDATE), or D (DELETE). These values indicate whether the row was inserted, updated, or deleted at the source database for a CDC load to the target.
    --
    -- If @CdcInsertsOnly@ is set to @true@ or @y@ , only INSERTs from the source database are migrated to the .csv or .parquet file. For .csv format only, how these INSERTs are recorded depends on the value of @IncludeOpForFullLoad@ . If @IncludeOpForFullLoad@ is set to @true@ , the first field of every CDC record is set to I to indicate the INSERT operation at the source. If @IncludeOpForFullLoad@ is set to @false@ , every CDC record is written without a first field to indicate the INSERT operation at the source. For more information about how these settings work together, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data> in the /AWS Database Migration Service User Guide./ .
    cdcInsertsOnly :: Core.Maybe Core.Bool,
    -- | An optional parameter to use GZIP to compress the target files. Set to GZIP to compress the target files. Either set this parameter to NONE (the default) or don't use it to leave the files uncompressed. This parameter applies to both .csv and .parquet file formats.
    compressionType :: Core.Maybe Types.CompressionTypeValue,
    -- | The delimiter used to separate columns in the .csv file for both source and target. The default is a comma.
    csvDelimiter :: Core.Maybe Types.String,
    -- | The delimiter used to separate rows in the .csv file for both source and target. The default is a carriage return (@\n@ ).
    csvRowDelimiter :: Core.Maybe Types.String,
    -- | The format of the data that you want to use for output. You can choose one of the following:
    --
    --
    --     * @csv@ : This is a row-based file format with comma-separated values (.csv).
    --
    --
    --     * @parquet@ : Apache Parquet (.parquet) is a columnar storage file format that features efficient compression and provides faster query response.
    dataFormat :: Core.Maybe Types.DataFormatValue,
    -- | The size of one data page in bytes. This parameter defaults to 1024 * 1024 bytes (1 MiB). This number is used for .parquet file format only.
    dataPageSize :: Core.Maybe Core.Int,
    -- | Specifies a date separating delimiter to use during folder partitioning. The default value is @SLASH@ . Use this parameter when @DatePartitionedEnabled@ is set to @true@ .
    datePartitionDelimiter :: Core.Maybe Types.DatePartitionDelimiterValue,
    -- | When set to @true@ , this parameter partitions S3 bucket folders based on transaction commit dates. The default value is @false@ . For more information about date-based folder partitoning, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning> .
    datePartitionEnabled :: Core.Maybe Core.Bool,
    -- | Identifies the sequence of the date format to use during folder partitioning. The default value is @YYYYMMDD@ . Use this parameter when @DatePartitionedEnabled@ is set to @true@ .
    datePartitionSequence :: Core.Maybe Types.DatePartitionSequenceValue,
    -- | The maximum size of an encoded dictionary page of a column. If the dictionary page exceeds this, this column is stored using an encoding type of @PLAIN@ . This parameter defaults to 1024 * 1024 bytes (1 MiB), the maximum size of a dictionary page before it reverts to @PLAIN@ encoding. This size is used for .parquet file format only.
    dictPageSizeLimit :: Core.Maybe Core.Int,
    -- | A value that enables statistics for Parquet pages and row groups. Choose @true@ to enable statistics, @false@ to disable. Statistics include @NULL@ , @DISTINCT@ , @MAX@ , and @MIN@ values. This parameter defaults to @true@ . This value is used for .parquet file format only.
    enableStatistics :: Core.Maybe Core.Bool,
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
    encodingType :: Core.Maybe Types.EncodingTypeValue,
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
    encryptionMode :: Core.Maybe Types.EncryptionModeValue,
    -- | Specifies how tables are defined in the S3 source files only.
    externalTableDefinition :: Core.Maybe Types.String,
    -- | A value that enables a full load to write INSERT operations to the comma-separated value (.csv) output files only to indicate how the rows were added to the source database.
    --
    -- For full load, records can only be inserted. By default (the @false@ setting), no information is recorded in these output files for a full load to indicate that the rows were inserted at the source database. If @IncludeOpForFullLoad@ is set to @true@ or @y@ , the INSERT is recorded as an I annotation in the first field of the .csv file. This allows the format of your target records from a full load to be consistent with the target records from a CDC load.
    includeOpForFullLoad :: Core.Maybe Core.Bool,
    -- | A value that specifies the precision of any @TIMESTAMP@ column values that are written to an Amazon S3 object file in .parquet format.
    --
    -- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@ , AWS DMS writes all @TIMESTAMP@ columns in a .parquet formatted file with millisecond precision. Otherwise, DMS writes them with microsecond precision.
    -- Currently, Amazon Athena and AWS Glue can handle only millisecond precision for @TIMESTAMP@ values. Set this parameter to @true@ for S3 endpoint object files that are .parquet formatted only if you plan to query or process the data with Athena or AWS Glue.
    parquetTimestampInMillisecond :: Core.Maybe Core.Bool,
    -- | The version of the Apache Parquet format that you want to use: @parquet_1_0@ (the default) or @parquet_2_0@ .
    parquetVersion :: Core.Maybe Types.ParquetVersionValue,
    -- | The number of rows in a row group. A smaller row group size provides faster reads. But as the number of row groups grows, the slower writes become. This parameter defaults to 10,000 rows. This number is used for .parquet file format only.
    --
    -- If you choose a value larger than the maximum, @RowGroupLength@ is set to the max row group length in bytes (64 * 1024 * 1024).
    rowGroupLength :: Core.Maybe Core.Int,
    -- | If you are using @SSE_KMS@ for the @EncryptionMode@ , provide the AWS KMS key ID. The key that you use needs an attached policy that enables AWS Identity and Access Management (IAM) user permissions and allows use of the key.
    --
    -- Here is a CLI example: @aws dms create-endpoint --endpoint-identifier /value/ --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=/value/ ,BucketFolder=/value/ ,BucketName=/value/ ,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=/value/ @
    serverSideEncryptionKmsKeyId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) used by the service access IAM role. It is a required parameter that enables DMS to write and read objects from an 3S bucket.
    serviceAccessRoleArn :: Core.Maybe Types.String,
    -- | A value that when nonblank causes AWS DMS to add a column with timestamp information to the endpoint data for an Amazon S3 target.
    --
    -- DMS includes an additional @STRING@ column in the .csv or .parquet object files of your migrated data when you set @TimestampColumnName@ to a nonblank value.
    -- For a full load, each row of this timestamp column contains a timestamp for when the data was transferred from the source to the target by DMS.
    -- For a change data capture (CDC) load, each row of the timestamp column contains the timestamp for the commit of that row in the source database.
    -- The string format for this timestamp column value is @yyyy-MM-dd HH:mm:ss.SSSSSS@ . By default, the precision of this value is in microseconds. For a CDC load, the rounding of the precision depends on the commit timestamp supported by DMS for the source database.
    -- When the @AddColumnName@ parameter is set to @true@ , DMS also includes a name for the timestamp column that you set with @TimestampColumnName@ .
    timestampColumnName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Settings' value with any optional fields omitted.
mkS3Settings ::
  S3Settings
mkS3Settings =
  S3Settings'
    { bucketFolder = Core.Nothing,
      bucketName = Core.Nothing,
      cdcInsertsAndUpdates = Core.Nothing,
      cdcInsertsOnly = Core.Nothing,
      compressionType = Core.Nothing,
      csvDelimiter = Core.Nothing,
      csvRowDelimiter = Core.Nothing,
      dataFormat = Core.Nothing,
      dataPageSize = Core.Nothing,
      datePartitionDelimiter = Core.Nothing,
      datePartitionEnabled = Core.Nothing,
      datePartitionSequence = Core.Nothing,
      dictPageSizeLimit = Core.Nothing,
      enableStatistics = Core.Nothing,
      encodingType = Core.Nothing,
      encryptionMode = Core.Nothing,
      externalTableDefinition = Core.Nothing,
      includeOpForFullLoad = Core.Nothing,
      parquetTimestampInMillisecond = Core.Nothing,
      parquetVersion = Core.Nothing,
      rowGroupLength = Core.Nothing,
      serverSideEncryptionKmsKeyId = Core.Nothing,
      serviceAccessRoleArn = Core.Nothing,
      timestampColumnName = Core.Nothing
    }

-- | An optional parameter to set a folder name in the S3 bucket. If provided, tables are created in the path @/bucketFolder/ //schema_name/ //table_name/ /@ . If this parameter isn't specified, then the path used is @/schema_name/ //table_name/ /@ .
--
-- /Note:/ Consider using 'bucketFolder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssBucketFolder :: Lens.Lens' S3Settings (Core.Maybe Types.String)
ssBucketFolder = Lens.field @"bucketFolder"
{-# DEPRECATED ssBucketFolder "Use generic-lens or generic-optics with 'bucketFolder' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssBucketName :: Lens.Lens' S3Settings (Core.Maybe Types.String)
ssBucketName = Lens.field @"bucketName"
{-# DEPRECATED ssBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | A value that enables a change data capture (CDC) load to write INSERT and UPDATE operations to .csv or .parquet (columnar storage) output files. The default setting is @false@ , but when @CdcInsertsAndUpdates@ is set to @true@ or @y@ , only INSERTs and UPDATEs from the source database are migrated to the .csv or .parquet file.
--
-- For .csv file format only, how these INSERTs and UPDATEs are recorded depends on the value of the @IncludeOpForFullLoad@ parameter. If @IncludeOpForFullLoad@ is set to @true@ , the first field of every CDC record is set to either @I@ or @U@ to indicate INSERT and UPDATE operations at the source. But if @IncludeOpForFullLoad@ is set to @false@ , CDC records are written without an indication of INSERT or UPDATE operations at the source. For more information about how these settings work together, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data> in the /AWS Database Migration Service User Guide./ .
--
-- /Note:/ Consider using 'cdcInsertsAndUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCdcInsertsAndUpdates :: Lens.Lens' S3Settings (Core.Maybe Core.Bool)
ssCdcInsertsAndUpdates = Lens.field @"cdcInsertsAndUpdates"
{-# DEPRECATED ssCdcInsertsAndUpdates "Use generic-lens or generic-optics with 'cdcInsertsAndUpdates' instead." #-}

-- | A value that enables a change data capture (CDC) load to write only INSERT operations to .csv or columnar storage (.parquet) output files. By default (the @false@ setting), the first field in a .csv or .parquet record contains the letter I (INSERT), U (UPDATE), or D (DELETE). These values indicate whether the row was inserted, updated, or deleted at the source database for a CDC load to the target.
--
-- If @CdcInsertsOnly@ is set to @true@ or @y@ , only INSERTs from the source database are migrated to the .csv or .parquet file. For .csv format only, how these INSERTs are recorded depends on the value of @IncludeOpForFullLoad@ . If @IncludeOpForFullLoad@ is set to @true@ , the first field of every CDC record is set to I to indicate the INSERT operation at the source. If @IncludeOpForFullLoad@ is set to @false@ , every CDC record is written without a first field to indicate the INSERT operation at the source. For more information about how these settings work together, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data> in the /AWS Database Migration Service User Guide./ .
--
-- /Note:/ Consider using 'cdcInsertsOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCdcInsertsOnly :: Lens.Lens' S3Settings (Core.Maybe Core.Bool)
ssCdcInsertsOnly = Lens.field @"cdcInsertsOnly"
{-# DEPRECATED ssCdcInsertsOnly "Use generic-lens or generic-optics with 'cdcInsertsOnly' instead." #-}

-- | An optional parameter to use GZIP to compress the target files. Set to GZIP to compress the target files. Either set this parameter to NONE (the default) or don't use it to leave the files uncompressed. This parameter applies to both .csv and .parquet file formats.
--
-- /Note:/ Consider using 'compressionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCompressionType :: Lens.Lens' S3Settings (Core.Maybe Types.CompressionTypeValue)
ssCompressionType = Lens.field @"compressionType"
{-# DEPRECATED ssCompressionType "Use generic-lens or generic-optics with 'compressionType' instead." #-}

-- | The delimiter used to separate columns in the .csv file for both source and target. The default is a comma.
--
-- /Note:/ Consider using 'csvDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCsvDelimiter :: Lens.Lens' S3Settings (Core.Maybe Types.String)
ssCsvDelimiter = Lens.field @"csvDelimiter"
{-# DEPRECATED ssCsvDelimiter "Use generic-lens or generic-optics with 'csvDelimiter' instead." #-}

-- | The delimiter used to separate rows in the .csv file for both source and target. The default is a carriage return (@\n@ ).
--
-- /Note:/ Consider using 'csvRowDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCsvRowDelimiter :: Lens.Lens' S3Settings (Core.Maybe Types.String)
ssCsvRowDelimiter = Lens.field @"csvRowDelimiter"
{-# DEPRECATED ssCsvRowDelimiter "Use generic-lens or generic-optics with 'csvRowDelimiter' instead." #-}

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
ssDataFormat :: Lens.Lens' S3Settings (Core.Maybe Types.DataFormatValue)
ssDataFormat = Lens.field @"dataFormat"
{-# DEPRECATED ssDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The size of one data page in bytes. This parameter defaults to 1024 * 1024 bytes (1 MiB). This number is used for .parquet file format only.
--
-- /Note:/ Consider using 'dataPageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDataPageSize :: Lens.Lens' S3Settings (Core.Maybe Core.Int)
ssDataPageSize = Lens.field @"dataPageSize"
{-# DEPRECATED ssDataPageSize "Use generic-lens or generic-optics with 'dataPageSize' instead." #-}

-- | Specifies a date separating delimiter to use during folder partitioning. The default value is @SLASH@ . Use this parameter when @DatePartitionedEnabled@ is set to @true@ .
--
-- /Note:/ Consider using 'datePartitionDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDatePartitionDelimiter :: Lens.Lens' S3Settings (Core.Maybe Types.DatePartitionDelimiterValue)
ssDatePartitionDelimiter = Lens.field @"datePartitionDelimiter"
{-# DEPRECATED ssDatePartitionDelimiter "Use generic-lens or generic-optics with 'datePartitionDelimiter' instead." #-}

-- | When set to @true@ , this parameter partitions S3 bucket folders based on transaction commit dates. The default value is @false@ . For more information about date-based folder partitoning, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning> .
--
-- /Note:/ Consider using 'datePartitionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDatePartitionEnabled :: Lens.Lens' S3Settings (Core.Maybe Core.Bool)
ssDatePartitionEnabled = Lens.field @"datePartitionEnabled"
{-# DEPRECATED ssDatePartitionEnabled "Use generic-lens or generic-optics with 'datePartitionEnabled' instead." #-}

-- | Identifies the sequence of the date format to use during folder partitioning. The default value is @YYYYMMDD@ . Use this parameter when @DatePartitionedEnabled@ is set to @true@ .
--
-- /Note:/ Consider using 'datePartitionSequence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDatePartitionSequence :: Lens.Lens' S3Settings (Core.Maybe Types.DatePartitionSequenceValue)
ssDatePartitionSequence = Lens.field @"datePartitionSequence"
{-# DEPRECATED ssDatePartitionSequence "Use generic-lens or generic-optics with 'datePartitionSequence' instead." #-}

-- | The maximum size of an encoded dictionary page of a column. If the dictionary page exceeds this, this column is stored using an encoding type of @PLAIN@ . This parameter defaults to 1024 * 1024 bytes (1 MiB), the maximum size of a dictionary page before it reverts to @PLAIN@ encoding. This size is used for .parquet file format only.
--
-- /Note:/ Consider using 'dictPageSizeLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDictPageSizeLimit :: Lens.Lens' S3Settings (Core.Maybe Core.Int)
ssDictPageSizeLimit = Lens.field @"dictPageSizeLimit"
{-# DEPRECATED ssDictPageSizeLimit "Use generic-lens or generic-optics with 'dictPageSizeLimit' instead." #-}

-- | A value that enables statistics for Parquet pages and row groups. Choose @true@ to enable statistics, @false@ to disable. Statistics include @NULL@ , @DISTINCT@ , @MAX@ , and @MIN@ values. This parameter defaults to @true@ . This value is used for .parquet file format only.
--
-- /Note:/ Consider using 'enableStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEnableStatistics :: Lens.Lens' S3Settings (Core.Maybe Core.Bool)
ssEnableStatistics = Lens.field @"enableStatistics"
{-# DEPRECATED ssEnableStatistics "Use generic-lens or generic-optics with 'enableStatistics' instead." #-}

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
ssEncodingType :: Lens.Lens' S3Settings (Core.Maybe Types.EncodingTypeValue)
ssEncodingType = Lens.field @"encodingType"
{-# DEPRECATED ssEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

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
ssEncryptionMode :: Lens.Lens' S3Settings (Core.Maybe Types.EncryptionModeValue)
ssEncryptionMode = Lens.field @"encryptionMode"
{-# DEPRECATED ssEncryptionMode "Use generic-lens or generic-optics with 'encryptionMode' instead." #-}

-- | Specifies how tables are defined in the S3 source files only.
--
-- /Note:/ Consider using 'externalTableDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssExternalTableDefinition :: Lens.Lens' S3Settings (Core.Maybe Types.String)
ssExternalTableDefinition = Lens.field @"externalTableDefinition"
{-# DEPRECATED ssExternalTableDefinition "Use generic-lens or generic-optics with 'externalTableDefinition' instead." #-}

-- | A value that enables a full load to write INSERT operations to the comma-separated value (.csv) output files only to indicate how the rows were added to the source database.
--
-- For full load, records can only be inserted. By default (the @false@ setting), no information is recorded in these output files for a full load to indicate that the rows were inserted at the source database. If @IncludeOpForFullLoad@ is set to @true@ or @y@ , the INSERT is recorded as an I annotation in the first field of the .csv file. This allows the format of your target records from a full load to be consistent with the target records from a CDC load.
--
-- /Note:/ Consider using 'includeOpForFullLoad' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssIncludeOpForFullLoad :: Lens.Lens' S3Settings (Core.Maybe Core.Bool)
ssIncludeOpForFullLoad = Lens.field @"includeOpForFullLoad"
{-# DEPRECATED ssIncludeOpForFullLoad "Use generic-lens or generic-optics with 'includeOpForFullLoad' instead." #-}

-- | A value that specifies the precision of any @TIMESTAMP@ column values that are written to an Amazon S3 object file in .parquet format.
--
-- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@ , AWS DMS writes all @TIMESTAMP@ columns in a .parquet formatted file with millisecond precision. Otherwise, DMS writes them with microsecond precision.
-- Currently, Amazon Athena and AWS Glue can handle only millisecond precision for @TIMESTAMP@ values. Set this parameter to @true@ for S3 endpoint object files that are .parquet formatted only if you plan to query or process the data with Athena or AWS Glue.
--
-- /Note:/ Consider using 'parquetTimestampInMillisecond' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParquetTimestampInMillisecond :: Lens.Lens' S3Settings (Core.Maybe Core.Bool)
ssParquetTimestampInMillisecond = Lens.field @"parquetTimestampInMillisecond"
{-# DEPRECATED ssParquetTimestampInMillisecond "Use generic-lens or generic-optics with 'parquetTimestampInMillisecond' instead." #-}

-- | The version of the Apache Parquet format that you want to use: @parquet_1_0@ (the default) or @parquet_2_0@ .
--
-- /Note:/ Consider using 'parquetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssParquetVersion :: Lens.Lens' S3Settings (Core.Maybe Types.ParquetVersionValue)
ssParquetVersion = Lens.field @"parquetVersion"
{-# DEPRECATED ssParquetVersion "Use generic-lens or generic-optics with 'parquetVersion' instead." #-}

-- | The number of rows in a row group. A smaller row group size provides faster reads. But as the number of row groups grows, the slower writes become. This parameter defaults to 10,000 rows. This number is used for .parquet file format only.
--
-- If you choose a value larger than the maximum, @RowGroupLength@ is set to the max row group length in bytes (64 * 1024 * 1024).
--
-- /Note:/ Consider using 'rowGroupLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssRowGroupLength :: Lens.Lens' S3Settings (Core.Maybe Core.Int)
ssRowGroupLength = Lens.field @"rowGroupLength"
{-# DEPRECATED ssRowGroupLength "Use generic-lens or generic-optics with 'rowGroupLength' instead." #-}

-- | If you are using @SSE_KMS@ for the @EncryptionMode@ , provide the AWS KMS key ID. The key that you use needs an attached policy that enables AWS Identity and Access Management (IAM) user permissions and allows use of the key.
--
-- Here is a CLI example: @aws dms create-endpoint --endpoint-identifier /value/ --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=/value/ ,BucketFolder=/value/ ,BucketName=/value/ ,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=/value/ @
--
-- /Note:/ Consider using 'serverSideEncryptionKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssServerSideEncryptionKmsKeyId :: Lens.Lens' S3Settings (Core.Maybe Types.String)
ssServerSideEncryptionKmsKeyId = Lens.field @"serverSideEncryptionKmsKeyId"
{-# DEPRECATED ssServerSideEncryptionKmsKeyId "Use generic-lens or generic-optics with 'serverSideEncryptionKmsKeyId' instead." #-}

-- | The Amazon Resource Name (ARN) used by the service access IAM role. It is a required parameter that enables DMS to write and read objects from an 3S bucket.
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssServiceAccessRoleArn :: Lens.Lens' S3Settings (Core.Maybe Types.String)
ssServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# DEPRECATED ssServiceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead." #-}

-- | A value that when nonblank causes AWS DMS to add a column with timestamp information to the endpoint data for an Amazon S3 target.
--
-- DMS includes an additional @STRING@ column in the .csv or .parquet object files of your migrated data when you set @TimestampColumnName@ to a nonblank value.
-- For a full load, each row of this timestamp column contains a timestamp for when the data was transferred from the source to the target by DMS.
-- For a change data capture (CDC) load, each row of the timestamp column contains the timestamp for the commit of that row in the source database.
-- The string format for this timestamp column value is @yyyy-MM-dd HH:mm:ss.SSSSSS@ . By default, the precision of this value is in microseconds. For a CDC load, the rounding of the precision depends on the commit timestamp supported by DMS for the source database.
-- When the @AddColumnName@ parameter is set to @true@ , DMS also includes a name for the timestamp column that you set with @TimestampColumnName@ .
--
-- /Note:/ Consider using 'timestampColumnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssTimestampColumnName :: Lens.Lens' S3Settings (Core.Maybe Types.String)
ssTimestampColumnName = Lens.field @"timestampColumnName"
{-# DEPRECATED ssTimestampColumnName "Use generic-lens or generic-optics with 'timestampColumnName' instead." #-}

instance Core.FromJSON S3Settings where
  toJSON S3Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("BucketFolder" Core..=) Core.<$> bucketFolder,
            ("BucketName" Core..=) Core.<$> bucketName,
            ("CdcInsertsAndUpdates" Core..=) Core.<$> cdcInsertsAndUpdates,
            ("CdcInsertsOnly" Core..=) Core.<$> cdcInsertsOnly,
            ("CompressionType" Core..=) Core.<$> compressionType,
            ("CsvDelimiter" Core..=) Core.<$> csvDelimiter,
            ("CsvRowDelimiter" Core..=) Core.<$> csvRowDelimiter,
            ("DataFormat" Core..=) Core.<$> dataFormat,
            ("DataPageSize" Core..=) Core.<$> dataPageSize,
            ("DatePartitionDelimiter" Core..=) Core.<$> datePartitionDelimiter,
            ("DatePartitionEnabled" Core..=) Core.<$> datePartitionEnabled,
            ("DatePartitionSequence" Core..=) Core.<$> datePartitionSequence,
            ("DictPageSizeLimit" Core..=) Core.<$> dictPageSizeLimit,
            ("EnableStatistics" Core..=) Core.<$> enableStatistics,
            ("EncodingType" Core..=) Core.<$> encodingType,
            ("EncryptionMode" Core..=) Core.<$> encryptionMode,
            ("ExternalTableDefinition" Core..=)
              Core.<$> externalTableDefinition,
            ("IncludeOpForFullLoad" Core..=) Core.<$> includeOpForFullLoad,
            ("ParquetTimestampInMillisecond" Core..=)
              Core.<$> parquetTimestampInMillisecond,
            ("ParquetVersion" Core..=) Core.<$> parquetVersion,
            ("RowGroupLength" Core..=) Core.<$> rowGroupLength,
            ("ServerSideEncryptionKmsKeyId" Core..=)
              Core.<$> serverSideEncryptionKmsKeyId,
            ("ServiceAccessRoleArn" Core..=) Core.<$> serviceAccessRoleArn,
            ("TimestampColumnName" Core..=) Core.<$> timestampColumnName
          ]
      )

instance Core.FromJSON S3Settings where
  parseJSON =
    Core.withObject "S3Settings" Core.$
      \x ->
        S3Settings'
          Core.<$> (x Core..:? "BucketFolder")
          Core.<*> (x Core..:? "BucketName")
          Core.<*> (x Core..:? "CdcInsertsAndUpdates")
          Core.<*> (x Core..:? "CdcInsertsOnly")
          Core.<*> (x Core..:? "CompressionType")
          Core.<*> (x Core..:? "CsvDelimiter")
          Core.<*> (x Core..:? "CsvRowDelimiter")
          Core.<*> (x Core..:? "DataFormat")
          Core.<*> (x Core..:? "DataPageSize")
          Core.<*> (x Core..:? "DatePartitionDelimiter")
          Core.<*> (x Core..:? "DatePartitionEnabled")
          Core.<*> (x Core..:? "DatePartitionSequence")
          Core.<*> (x Core..:? "DictPageSizeLimit")
          Core.<*> (x Core..:? "EnableStatistics")
          Core.<*> (x Core..:? "EncodingType")
          Core.<*> (x Core..:? "EncryptionMode")
          Core.<*> (x Core..:? "ExternalTableDefinition")
          Core.<*> (x Core..:? "IncludeOpForFullLoad")
          Core.<*> (x Core..:? "ParquetTimestampInMillisecond")
          Core.<*> (x Core..:? "ParquetVersion")
          Core.<*> (x Core..:? "RowGroupLength")
          Core.<*> (x Core..:? "ServerSideEncryptionKmsKeyId")
          Core.<*> (x Core..:? "ServiceAccessRoleArn")
          Core.<*> (x Core..:? "TimestampColumnName")

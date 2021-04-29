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
-- Module      : Network.AWS.DMS.Types.S3Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.S3Settings where

import Network.AWS.DMS.Types.CompressionTypeValue
import Network.AWS.DMS.Types.DataFormatValue
import Network.AWS.DMS.Types.DatePartitionDelimiterValue
import Network.AWS.DMS.Types.DatePartitionSequenceValue
import Network.AWS.DMS.Types.EncodingTypeValue
import Network.AWS.DMS.Types.EncryptionModeValue
import Network.AWS.DMS.Types.ParquetVersionValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Settings for exporting data to Amazon S3.
--
-- /See:/ 'newS3Settings' smart constructor.
data S3Settings = S3Settings'
  { -- | A value that when nonblank causes AWS DMS to add a column with timestamp
    -- information to the endpoint data for an Amazon S3 target.
    --
    -- AWS DMS supports the @TimestampColumnName@ parameter in versions 3.1.4
    -- and later.
    --
    -- DMS includes an additional @STRING@ column in the .csv or .parquet
    -- object files of your migrated data when you set @TimestampColumnName@ to
    -- a nonblank value.
    --
    -- For a full load, each row of this timestamp column contains a timestamp
    -- for when the data was transferred from the source to the target by DMS.
    --
    -- For a change data capture (CDC) load, each row of the timestamp column
    -- contains the timestamp for the commit of that row in the source
    -- database.
    --
    -- The string format for this timestamp column value is
    -- @yyyy-MM-dd HH:mm:ss.SSSSSS@. By default, the precision of this value is
    -- in microseconds. For a CDC load, the rounding of the precision depends
    -- on the commit timestamp supported by DMS for the source database.
    --
    -- When the @AddColumnName@ parameter is set to @true@, DMS also includes a
    -- name for the timestamp column that you set with @TimestampColumnName@.
    timestampColumnName :: Prelude.Maybe Prelude.Text,
    -- | If set to @true@, AWS DMS saves the transaction order for a change data
    -- capture (CDC) load on the Amazon S3 target specified by
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CdcPath CdcPath>
    -- . For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
    --
    -- This setting is supported in AWS DMS versions 3.4.2 and later.
    preserveTransactions :: Prelude.Maybe Prelude.Bool,
    -- | The delimiter used to separate rows in the .csv file for both source and
    -- target. The default is a carriage return (@\\n@).
    csvRowDelimiter :: Prelude.Maybe Prelude.Text,
    -- | The version of the Apache Parquet format that you want to use:
    -- @parquet_1_0@ (the default) or @parquet_2_0@.
    parquetVersion :: Prelude.Maybe ParquetVersionValue,
    -- | Identifies the sequence of the date format to use during folder
    -- partitioning. The default value is @YYYYMMDD@. Use this parameter when
    -- @DatePartitionedEnabled@ is set to @true@.
    datePartitionSequence :: Prelude.Maybe DatePartitionSequenceValue,
    -- | The name of the S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the folder path of CDC files. For an S3 source, this setting
    -- is required if a task captures change data; otherwise, it\'s optional.
    -- If @CdcPath@ is set, AWS DMS reads CDC files from this path and
    -- replicates the data changes to the target endpoint. For an S3 target if
    -- you set
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-PreserveTransactions PreserveTransactions>
    -- to @true@, AWS DMS verifies that you have set this parameter to a folder
    -- path on your S3 target where AWS DMS can save the transaction order for
    -- the CDC load. AWS DMS creates this CDC folder path in either your S3
    -- target working directory or the S3 target location specified by
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketFolder BucketFolder>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketName BucketName>
    -- .
    --
    -- For example, if you specify @CdcPath@ as @MyChangedData@, and you
    -- specify @BucketName@ as @MyTargetBucket@ but do not specify
    -- @BucketFolder@, AWS DMS creates the CDC folder path following:
    -- @MyTargetBucket\/MyChangedData@.
    --
    -- If you specify the same @CdcPath@, and you specify @BucketName@ as
    -- @MyTargetBucket@ and @BucketFolder@ as @MyTargetData@, AWS DMS creates
    -- the CDC folder path following:
    -- @MyTargetBucket\/MyTargetData\/MyChangedData@.
    --
    -- For more information on CDC including transaction order on an S3 target,
    -- see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
    --
    -- This setting is supported in AWS DMS versions 3.4.2 and later.
    cdcPath :: Prelude.Maybe Prelude.Text,
    -- | Specifies how tables are defined in the S3 source files only.
    externalTableDefinition :: Prelude.Maybe Prelude.Text,
    -- | If you are using @SSE_KMS@ for the @EncryptionMode@, provide the AWS KMS
    -- key ID. The key that you use needs an attached policy that enables AWS
    -- Identity and Access Management (IAM) user permissions and allows use of
    -- the key.
    --
    -- Here is a CLI example:
    -- @aws dms create-endpoint --endpoint-identifier value --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=value,BucketFolder=value,BucketName=value,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=value @
    serverSideEncryptionKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The size of one data page in bytes. This parameter defaults to 1024 *
    -- 1024 bytes (1 MiB). This number is used for .parquet file format only.
    dataPageSize :: Prelude.Maybe Prelude.Int,
    -- | The type of encoding you are using:
    --
    -- -   @RLE_DICTIONARY@ uses a combination of bit-packing and run-length
    --     encoding to store repeated values more efficiently. This is the
    --     default.
    --
    -- -   @PLAIN@ doesn\'t use encoding at all. Values are stored as they are.
    --
    -- -   @PLAIN_DICTIONARY@ builds a dictionary of the values encountered in
    --     a given column. The dictionary is stored in a dictionary page for
    --     each column chunk.
    encodingType :: Prelude.Maybe EncodingTypeValue,
    -- | When set to @true@, this parameter partitions S3 bucket folders based on
    -- transaction commit dates. The default value is @false@. For more
    -- information about date-based folder partitoning, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning>.
    datePartitionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The format of the data that you want to use for output. You can choose
    -- one of the following:
    --
    -- -   @csv@ : This is a row-based file format with comma-separated values
    --     (.csv).
    --
    -- -   @parquet@ : Apache Parquet (.parquet) is a columnar storage file
    --     format that features efficient compression and provides faster query
    --     response.
    dataFormat :: Prelude.Maybe DataFormatValue,
    -- | The Amazon Resource Name (ARN) used by the service access IAM role. It
    -- is a required parameter that enables DMS to write and read objects from
    -- an 3S bucket.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter to set a folder name in the S3 bucket. If
    -- provided, tables are created in the path
    -- @ bucketFolder\/schema_name\/table_name\/@. If this parameter isn\'t
    -- specified, then the path used is @ schema_name\/table_name\/@.
    bucketFolder :: Prelude.Maybe Prelude.Text,
    -- | Specifies a date separating delimiter to use during folder partitioning.
    -- The default value is @SLASH@. Use this parameter when
    -- @DatePartitionedEnabled@ is set to @true@.
    datePartitionDelimiter :: Prelude.Maybe DatePartitionDelimiterValue,
    -- | A value that enables statistics for Parquet pages and row groups. Choose
    -- @true@ to enable statistics, @false@ to disable. Statistics include
    -- @NULL@, @DISTINCT@, @MAX@, and @MIN@ values. This parameter defaults to
    -- @true@. This value is used for .parquet file format only.
    enableStatistics :: Prelude.Maybe Prelude.Bool,
    -- | The type of server-side encryption that you want to use for your data.
    -- This encryption type is part of the endpoint settings or the extra
    -- connections attributes for Amazon S3. You can choose either @SSE_S3@
    -- (the default) or @SSE_KMS@.
    --
    -- For the @ModifyEndpoint@ operation, you can change the existing value of
    -- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
    -- change the existing value from @SSE_S3@ to @SSE_KMS@.
    --
    -- To use @SSE_S3@, you need an AWS Identity and Access Management (IAM)
    -- role with permission to allow @\"arn:aws:s3:::dms-*\"@ to use the
    -- following actions:
    --
    -- -   @s3:CreateBucket@
    --
    -- -   @s3:ListBucket@
    --
    -- -   @s3:DeleteBucket@
    --
    -- -   @s3:GetBucketLocation@
    --
    -- -   @s3:GetObject@
    --
    -- -   @s3:PutObject@
    --
    -- -   @s3:DeleteObject@
    --
    -- -   @s3:GetObjectVersion@
    --
    -- -   @s3:GetBucketPolicy@
    --
    -- -   @s3:PutBucketPolicy@
    --
    -- -   @s3:DeleteBucketPolicy@
    encryptionMode :: Prelude.Maybe EncryptionModeValue,
    -- | A value that enables a change data capture (CDC) load to write only
    -- INSERT operations to .csv or columnar storage (.parquet) output files.
    -- By default (the @false@ setting), the first field in a .csv or .parquet
    -- record contains the letter I (INSERT), U (UPDATE), or D (DELETE). These
    -- values indicate whether the row was inserted, updated, or deleted at the
    -- source database for a CDC load to the target.
    --
    -- If @CdcInsertsOnly@ is set to @true@ or @y@, only INSERTs from the
    -- source database are migrated to the .csv or .parquet file. For .csv
    -- format only, how these INSERTs are recorded depends on the value of
    -- @IncludeOpForFullLoad@. If @IncludeOpForFullLoad@ is set to @true@, the
    -- first field of every CDC record is set to I to indicate the INSERT
    -- operation at the source. If @IncludeOpForFullLoad@ is set to @false@,
    -- every CDC record is written without a first field to indicate the INSERT
    -- operation at the source. For more information about how these settings
    -- work together, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
    -- in the /AWS Database Migration Service User Guide./.
    --
    -- AWS DMS supports the interaction described preceding between the
    -- @CdcInsertsOnly@ and @IncludeOpForFullLoad@ parameters in versions 3.1.4
    -- and later.
    --
    -- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
    -- for the same endpoint. Set either @CdcInsertsOnly@ or
    -- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
    cdcInsertsOnly :: Prelude.Maybe Prelude.Bool,
    -- | A value that enables a change data capture (CDC) load to write INSERT
    -- and UPDATE operations to .csv or .parquet (columnar storage) output
    -- files. The default setting is @false@, but when @CdcInsertsAndUpdates@
    -- is set to @true@ or @y@, only INSERTs and UPDATEs from the source
    -- database are migrated to the .csv or .parquet file.
    --
    -- For .csv file format only, how these INSERTs and UPDATEs are recorded
    -- depends on the value of the @IncludeOpForFullLoad@ parameter. If
    -- @IncludeOpForFullLoad@ is set to @true@, the first field of every CDC
    -- record is set to either @I@ or @U@ to indicate INSERT and UPDATE
    -- operations at the source. But if @IncludeOpForFullLoad@ is set to
    -- @false@, CDC records are written without an indication of INSERT or
    -- UPDATE operations at the source. For more information about how these
    -- settings work together, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
    -- in the /AWS Database Migration Service User Guide./.
    --
    -- AWS DMS supports the use of the @CdcInsertsAndUpdates@ parameter in
    -- versions 3.3.1 and later.
    --
    -- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
    -- for the same endpoint. Set either @CdcInsertsOnly@ or
    -- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
    cdcInsertsAndUpdates :: Prelude.Maybe Prelude.Bool,
    -- | This setting applies if the S3 output files during a change data capture
    -- (CDC) load are written in .csv format. If set to @true@ for columns not
    -- included in the supplemental log, AWS DMS uses the value specified by
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CsvNoSupValue CsvNoSupValue>
    -- . If not set or set to @false@, AWS DMS uses the null value for these
    -- columns.
    --
    -- This setting is supported in AWS DMS versions 3.4.1 and later.
    useCsvNoSupValue :: Prelude.Maybe Prelude.Bool,
    -- | The maximum size of an encoded dictionary page of a column. If the
    -- dictionary page exceeds this, this column is stored using an encoding
    -- type of @PLAIN@. This parameter defaults to 1024 * 1024 bytes (1 MiB),
    -- the maximum size of a dictionary page before it reverts to @PLAIN@
    -- encoding. This size is used for .parquet file format only.
    dictPageSizeLimit :: Prelude.Maybe Prelude.Int,
    -- | The number of rows in a row group. A smaller row group size provides
    -- faster reads. But as the number of row groups grows, the slower writes
    -- become. This parameter defaults to 10,000 rows. This number is used for
    -- .parquet file format only.
    --
    -- If you choose a value larger than the maximum, @RowGroupLength@ is set
    -- to the max row group length in bytes (64 * 1024 * 1024).
    rowGroupLength :: Prelude.Maybe Prelude.Int,
    -- | An optional parameter to use GZIP to compress the target files. Set to
    -- GZIP to compress the target files. Either set this parameter to NONE
    -- (the default) or don\'t use it to leave the files uncompressed. This
    -- parameter applies to both .csv and .parquet file formats.
    compressionType :: Prelude.Maybe CompressionTypeValue,
    -- | A value that enables a full load to write INSERT operations to the
    -- comma-separated value (.csv) output files only to indicate how the rows
    -- were added to the source database.
    --
    -- AWS DMS supports the @IncludeOpForFullLoad@ parameter in versions 3.1.4
    -- and later.
    --
    -- For full load, records can only be inserted. By default (the @false@
    -- setting), no information is recorded in these output files for a full
    -- load to indicate that the rows were inserted at the source database. If
    -- @IncludeOpForFullLoad@ is set to @true@ or @y@, the INSERT is recorded
    -- as an I annotation in the first field of the .csv file. This allows the
    -- format of your target records from a full load to be consistent with the
    -- target records from a CDC load.
    --
    -- This setting works together with the @CdcInsertsOnly@ and the
    -- @CdcInsertsAndUpdates@ parameters for output to .csv files only. For
    -- more information about how these settings work together, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
    -- in the /AWS Database Migration Service User Guide./.
    includeOpForFullLoad :: Prelude.Maybe Prelude.Bool,
    -- | The delimiter used to separate columns in the .csv file for both source
    -- and target. The default is a comma.
    csvDelimiter :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the precision of any @TIMESTAMP@ column values
    -- that are written to an Amazon S3 object file in .parquet format.
    --
    -- AWS DMS supports the @ParquetTimestampInMillisecond@ parameter in
    -- versions 3.1.4 and later.
    --
    -- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@, AWS DMS
    -- writes all @TIMESTAMP@ columns in a .parquet formatted file with
    -- millisecond precision. Otherwise, DMS writes them with microsecond
    -- precision.
    --
    -- Currently, Amazon Athena and AWS Glue can handle only millisecond
    -- precision for @TIMESTAMP@ values. Set this parameter to @true@ for S3
    -- endpoint object files that are .parquet formatted only if you plan to
    -- query or process the data with Athena or AWS Glue.
    --
    -- AWS DMS writes any @TIMESTAMP@ column values written to an S3 file in
    -- .csv format with microsecond precision.
    --
    -- Setting @ParquetTimestampInMillisecond@ has no effect on the string
    -- format of the timestamp column value that is inserted by setting the
    -- @TimestampColumnName@ parameter.
    parquetTimestampInMillisecond :: Prelude.Maybe Prelude.Bool,
    -- | This setting only applies if your Amazon S3 output files during a change
    -- data capture (CDC) load are written in .csv format. If
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-UseCsvNoSupValue UseCsvNoSupValue>
    -- is set to true, specify a string value that you want AWS DMS to use for
    -- all columns not included in the supplemental log. If you do not specify
    -- a string value, AWS DMS uses the null value for these columns regardless
    -- of the @UseCsvNoSupValue@ setting.
    --
    -- This setting is supported in AWS DMS versions 3.4.1 and later.
    csvNoSupValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestampColumnName', 's3Settings_timestampColumnName' - A value that when nonblank causes AWS DMS to add a column with timestamp
-- information to the endpoint data for an Amazon S3 target.
--
-- AWS DMS supports the @TimestampColumnName@ parameter in versions 3.1.4
-- and later.
--
-- DMS includes an additional @STRING@ column in the .csv or .parquet
-- object files of your migrated data when you set @TimestampColumnName@ to
-- a nonblank value.
--
-- For a full load, each row of this timestamp column contains a timestamp
-- for when the data was transferred from the source to the target by DMS.
--
-- For a change data capture (CDC) load, each row of the timestamp column
-- contains the timestamp for the commit of that row in the source
-- database.
--
-- The string format for this timestamp column value is
-- @yyyy-MM-dd HH:mm:ss.SSSSSS@. By default, the precision of this value is
-- in microseconds. For a CDC load, the rounding of the precision depends
-- on the commit timestamp supported by DMS for the source database.
--
-- When the @AddColumnName@ parameter is set to @true@, DMS also includes a
-- name for the timestamp column that you set with @TimestampColumnName@.
--
-- 'preserveTransactions', 's3Settings_preserveTransactions' - If set to @true@, AWS DMS saves the transaction order for a change data
-- capture (CDC) load on the Amazon S3 target specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CdcPath CdcPath>
-- . For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
--
-- This setting is supported in AWS DMS versions 3.4.2 and later.
--
-- 'csvRowDelimiter', 's3Settings_csvRowDelimiter' - The delimiter used to separate rows in the .csv file for both source and
-- target. The default is a carriage return (@\\n@).
--
-- 'parquetVersion', 's3Settings_parquetVersion' - The version of the Apache Parquet format that you want to use:
-- @parquet_1_0@ (the default) or @parquet_2_0@.
--
-- 'datePartitionSequence', 's3Settings_datePartitionSequence' - Identifies the sequence of the date format to use during folder
-- partitioning. The default value is @YYYYMMDD@. Use this parameter when
-- @DatePartitionedEnabled@ is set to @true@.
--
-- 'bucketName', 's3Settings_bucketName' - The name of the S3 bucket.
--
-- 'cdcPath', 's3Settings_cdcPath' - Specifies the folder path of CDC files. For an S3 source, this setting
-- is required if a task captures change data; otherwise, it\'s optional.
-- If @CdcPath@ is set, AWS DMS reads CDC files from this path and
-- replicates the data changes to the target endpoint. For an S3 target if
-- you set
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-PreserveTransactions PreserveTransactions>
-- to @true@, AWS DMS verifies that you have set this parameter to a folder
-- path on your S3 target where AWS DMS can save the transaction order for
-- the CDC load. AWS DMS creates this CDC folder path in either your S3
-- target working directory or the S3 target location specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketFolder BucketFolder>
-- and
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketName BucketName>
-- .
--
-- For example, if you specify @CdcPath@ as @MyChangedData@, and you
-- specify @BucketName@ as @MyTargetBucket@ but do not specify
-- @BucketFolder@, AWS DMS creates the CDC folder path following:
-- @MyTargetBucket\/MyChangedData@.
--
-- If you specify the same @CdcPath@, and you specify @BucketName@ as
-- @MyTargetBucket@ and @BucketFolder@ as @MyTargetData@, AWS DMS creates
-- the CDC folder path following:
-- @MyTargetBucket\/MyTargetData\/MyChangedData@.
--
-- For more information on CDC including transaction order on an S3 target,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
--
-- This setting is supported in AWS DMS versions 3.4.2 and later.
--
-- 'externalTableDefinition', 's3Settings_externalTableDefinition' - Specifies how tables are defined in the S3 source files only.
--
-- 'serverSideEncryptionKmsKeyId', 's3Settings_serverSideEncryptionKmsKeyId' - If you are using @SSE_KMS@ for the @EncryptionMode@, provide the AWS KMS
-- key ID. The key that you use needs an attached policy that enables AWS
-- Identity and Access Management (IAM) user permissions and allows use of
-- the key.
--
-- Here is a CLI example:
-- @aws dms create-endpoint --endpoint-identifier value --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=value,BucketFolder=value,BucketName=value,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=value @
--
-- 'dataPageSize', 's3Settings_dataPageSize' - The size of one data page in bytes. This parameter defaults to 1024 *
-- 1024 bytes (1 MiB). This number is used for .parquet file format only.
--
-- 'encodingType', 's3Settings_encodingType' - The type of encoding you are using:
--
-- -   @RLE_DICTIONARY@ uses a combination of bit-packing and run-length
--     encoding to store repeated values more efficiently. This is the
--     default.
--
-- -   @PLAIN@ doesn\'t use encoding at all. Values are stored as they are.
--
-- -   @PLAIN_DICTIONARY@ builds a dictionary of the values encountered in
--     a given column. The dictionary is stored in a dictionary page for
--     each column chunk.
--
-- 'datePartitionEnabled', 's3Settings_datePartitionEnabled' - When set to @true@, this parameter partitions S3 bucket folders based on
-- transaction commit dates. The default value is @false@. For more
-- information about date-based folder partitoning, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning>.
--
-- 'dataFormat', 's3Settings_dataFormat' - The format of the data that you want to use for output. You can choose
-- one of the following:
--
-- -   @csv@ : This is a row-based file format with comma-separated values
--     (.csv).
--
-- -   @parquet@ : Apache Parquet (.parquet) is a columnar storage file
--     format that features efficient compression and provides faster query
--     response.
--
-- 'serviceAccessRoleArn', 's3Settings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by the service access IAM role. It
-- is a required parameter that enables DMS to write and read objects from
-- an 3S bucket.
--
-- 'bucketFolder', 's3Settings_bucketFolder' - An optional parameter to set a folder name in the S3 bucket. If
-- provided, tables are created in the path
-- @ bucketFolder\/schema_name\/table_name\/@. If this parameter isn\'t
-- specified, then the path used is @ schema_name\/table_name\/@.
--
-- 'datePartitionDelimiter', 's3Settings_datePartitionDelimiter' - Specifies a date separating delimiter to use during folder partitioning.
-- The default value is @SLASH@. Use this parameter when
-- @DatePartitionedEnabled@ is set to @true@.
--
-- 'enableStatistics', 's3Settings_enableStatistics' - A value that enables statistics for Parquet pages and row groups. Choose
-- @true@ to enable statistics, @false@ to disable. Statistics include
-- @NULL@, @DISTINCT@, @MAX@, and @MIN@ values. This parameter defaults to
-- @true@. This value is used for .parquet file format only.
--
-- 'encryptionMode', 's3Settings_encryptionMode' - The type of server-side encryption that you want to use for your data.
-- This encryption type is part of the endpoint settings or the extra
-- connections attributes for Amazon S3. You can choose either @SSE_S3@
-- (the default) or @SSE_KMS@.
--
-- For the @ModifyEndpoint@ operation, you can change the existing value of
-- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
-- change the existing value from @SSE_S3@ to @SSE_KMS@.
--
-- To use @SSE_S3@, you need an AWS Identity and Access Management (IAM)
-- role with permission to allow @\"arn:aws:s3:::dms-*\"@ to use the
-- following actions:
--
-- -   @s3:CreateBucket@
--
-- -   @s3:ListBucket@
--
-- -   @s3:DeleteBucket@
--
-- -   @s3:GetBucketLocation@
--
-- -   @s3:GetObject@
--
-- -   @s3:PutObject@
--
-- -   @s3:DeleteObject@
--
-- -   @s3:GetObjectVersion@
--
-- -   @s3:GetBucketPolicy@
--
-- -   @s3:PutBucketPolicy@
--
-- -   @s3:DeleteBucketPolicy@
--
-- 'cdcInsertsOnly', 's3Settings_cdcInsertsOnly' - A value that enables a change data capture (CDC) load to write only
-- INSERT operations to .csv or columnar storage (.parquet) output files.
-- By default (the @false@ setting), the first field in a .csv or .parquet
-- record contains the letter I (INSERT), U (UPDATE), or D (DELETE). These
-- values indicate whether the row was inserted, updated, or deleted at the
-- source database for a CDC load to the target.
--
-- If @CdcInsertsOnly@ is set to @true@ or @y@, only INSERTs from the
-- source database are migrated to the .csv or .parquet file. For .csv
-- format only, how these INSERTs are recorded depends on the value of
-- @IncludeOpForFullLoad@. If @IncludeOpForFullLoad@ is set to @true@, the
-- first field of every CDC record is set to I to indicate the INSERT
-- operation at the source. If @IncludeOpForFullLoad@ is set to @false@,
-- every CDC record is written without a first field to indicate the INSERT
-- operation at the source. For more information about how these settings
-- work together, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
-- in the /AWS Database Migration Service User Guide./.
--
-- AWS DMS supports the interaction described preceding between the
-- @CdcInsertsOnly@ and @IncludeOpForFullLoad@ parameters in versions 3.1.4
-- and later.
--
-- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
-- for the same endpoint. Set either @CdcInsertsOnly@ or
-- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
--
-- 'cdcInsertsAndUpdates', 's3Settings_cdcInsertsAndUpdates' - A value that enables a change data capture (CDC) load to write INSERT
-- and UPDATE operations to .csv or .parquet (columnar storage) output
-- files. The default setting is @false@, but when @CdcInsertsAndUpdates@
-- is set to @true@ or @y@, only INSERTs and UPDATEs from the source
-- database are migrated to the .csv or .parquet file.
--
-- For .csv file format only, how these INSERTs and UPDATEs are recorded
-- depends on the value of the @IncludeOpForFullLoad@ parameter. If
-- @IncludeOpForFullLoad@ is set to @true@, the first field of every CDC
-- record is set to either @I@ or @U@ to indicate INSERT and UPDATE
-- operations at the source. But if @IncludeOpForFullLoad@ is set to
-- @false@, CDC records are written without an indication of INSERT or
-- UPDATE operations at the source. For more information about how these
-- settings work together, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
-- in the /AWS Database Migration Service User Guide./.
--
-- AWS DMS supports the use of the @CdcInsertsAndUpdates@ parameter in
-- versions 3.3.1 and later.
--
-- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
-- for the same endpoint. Set either @CdcInsertsOnly@ or
-- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
--
-- 'useCsvNoSupValue', 's3Settings_useCsvNoSupValue' - This setting applies if the S3 output files during a change data capture
-- (CDC) load are written in .csv format. If set to @true@ for columns not
-- included in the supplemental log, AWS DMS uses the value specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CsvNoSupValue CsvNoSupValue>
-- . If not set or set to @false@, AWS DMS uses the null value for these
-- columns.
--
-- This setting is supported in AWS DMS versions 3.4.1 and later.
--
-- 'dictPageSizeLimit', 's3Settings_dictPageSizeLimit' - The maximum size of an encoded dictionary page of a column. If the
-- dictionary page exceeds this, this column is stored using an encoding
-- type of @PLAIN@. This parameter defaults to 1024 * 1024 bytes (1 MiB),
-- the maximum size of a dictionary page before it reverts to @PLAIN@
-- encoding. This size is used for .parquet file format only.
--
-- 'rowGroupLength', 's3Settings_rowGroupLength' - The number of rows in a row group. A smaller row group size provides
-- faster reads. But as the number of row groups grows, the slower writes
-- become. This parameter defaults to 10,000 rows. This number is used for
-- .parquet file format only.
--
-- If you choose a value larger than the maximum, @RowGroupLength@ is set
-- to the max row group length in bytes (64 * 1024 * 1024).
--
-- 'compressionType', 's3Settings_compressionType' - An optional parameter to use GZIP to compress the target files. Set to
-- GZIP to compress the target files. Either set this parameter to NONE
-- (the default) or don\'t use it to leave the files uncompressed. This
-- parameter applies to both .csv and .parquet file formats.
--
-- 'includeOpForFullLoad', 's3Settings_includeOpForFullLoad' - A value that enables a full load to write INSERT operations to the
-- comma-separated value (.csv) output files only to indicate how the rows
-- were added to the source database.
--
-- AWS DMS supports the @IncludeOpForFullLoad@ parameter in versions 3.1.4
-- and later.
--
-- For full load, records can only be inserted. By default (the @false@
-- setting), no information is recorded in these output files for a full
-- load to indicate that the rows were inserted at the source database. If
-- @IncludeOpForFullLoad@ is set to @true@ or @y@, the INSERT is recorded
-- as an I annotation in the first field of the .csv file. This allows the
-- format of your target records from a full load to be consistent with the
-- target records from a CDC load.
--
-- This setting works together with the @CdcInsertsOnly@ and the
-- @CdcInsertsAndUpdates@ parameters for output to .csv files only. For
-- more information about how these settings work together, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
-- in the /AWS Database Migration Service User Guide./.
--
-- 'csvDelimiter', 's3Settings_csvDelimiter' - The delimiter used to separate columns in the .csv file for both source
-- and target. The default is a comma.
--
-- 'parquetTimestampInMillisecond', 's3Settings_parquetTimestampInMillisecond' - A value that specifies the precision of any @TIMESTAMP@ column values
-- that are written to an Amazon S3 object file in .parquet format.
--
-- AWS DMS supports the @ParquetTimestampInMillisecond@ parameter in
-- versions 3.1.4 and later.
--
-- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@, AWS DMS
-- writes all @TIMESTAMP@ columns in a .parquet formatted file with
-- millisecond precision. Otherwise, DMS writes them with microsecond
-- precision.
--
-- Currently, Amazon Athena and AWS Glue can handle only millisecond
-- precision for @TIMESTAMP@ values. Set this parameter to @true@ for S3
-- endpoint object files that are .parquet formatted only if you plan to
-- query or process the data with Athena or AWS Glue.
--
-- AWS DMS writes any @TIMESTAMP@ column values written to an S3 file in
-- .csv format with microsecond precision.
--
-- Setting @ParquetTimestampInMillisecond@ has no effect on the string
-- format of the timestamp column value that is inserted by setting the
-- @TimestampColumnName@ parameter.
--
-- 'csvNoSupValue', 's3Settings_csvNoSupValue' - This setting only applies if your Amazon S3 output files during a change
-- data capture (CDC) load are written in .csv format. If
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-UseCsvNoSupValue UseCsvNoSupValue>
-- is set to true, specify a string value that you want AWS DMS to use for
-- all columns not included in the supplemental log. If you do not specify
-- a string value, AWS DMS uses the null value for these columns regardless
-- of the @UseCsvNoSupValue@ setting.
--
-- This setting is supported in AWS DMS versions 3.4.1 and later.
newS3Settings ::
  S3Settings
newS3Settings =
  S3Settings'
    { timestampColumnName = Prelude.Nothing,
      preserveTransactions = Prelude.Nothing,
      csvRowDelimiter = Prelude.Nothing,
      parquetVersion = Prelude.Nothing,
      datePartitionSequence = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      cdcPath = Prelude.Nothing,
      externalTableDefinition = Prelude.Nothing,
      serverSideEncryptionKmsKeyId = Prelude.Nothing,
      dataPageSize = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      datePartitionEnabled = Prelude.Nothing,
      dataFormat = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      bucketFolder = Prelude.Nothing,
      datePartitionDelimiter = Prelude.Nothing,
      enableStatistics = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      cdcInsertsOnly = Prelude.Nothing,
      cdcInsertsAndUpdates = Prelude.Nothing,
      useCsvNoSupValue = Prelude.Nothing,
      dictPageSizeLimit = Prelude.Nothing,
      rowGroupLength = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      includeOpForFullLoad = Prelude.Nothing,
      csvDelimiter = Prelude.Nothing,
      parquetTimestampInMillisecond = Prelude.Nothing,
      csvNoSupValue = Prelude.Nothing
    }

-- | A value that when nonblank causes AWS DMS to add a column with timestamp
-- information to the endpoint data for an Amazon S3 target.
--
-- AWS DMS supports the @TimestampColumnName@ parameter in versions 3.1.4
-- and later.
--
-- DMS includes an additional @STRING@ column in the .csv or .parquet
-- object files of your migrated data when you set @TimestampColumnName@ to
-- a nonblank value.
--
-- For a full load, each row of this timestamp column contains a timestamp
-- for when the data was transferred from the source to the target by DMS.
--
-- For a change data capture (CDC) load, each row of the timestamp column
-- contains the timestamp for the commit of that row in the source
-- database.
--
-- The string format for this timestamp column value is
-- @yyyy-MM-dd HH:mm:ss.SSSSSS@. By default, the precision of this value is
-- in microseconds. For a CDC load, the rounding of the precision depends
-- on the commit timestamp supported by DMS for the source database.
--
-- When the @AddColumnName@ parameter is set to @true@, DMS also includes a
-- name for the timestamp column that you set with @TimestampColumnName@.
s3Settings_timestampColumnName :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_timestampColumnName = Lens.lens (\S3Settings' {timestampColumnName} -> timestampColumnName) (\s@S3Settings' {} a -> s {timestampColumnName = a} :: S3Settings)

-- | If set to @true@, AWS DMS saves the transaction order for a change data
-- capture (CDC) load on the Amazon S3 target specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CdcPath CdcPath>
-- . For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
--
-- This setting is supported in AWS DMS versions 3.4.2 and later.
s3Settings_preserveTransactions :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_preserveTransactions = Lens.lens (\S3Settings' {preserveTransactions} -> preserveTransactions) (\s@S3Settings' {} a -> s {preserveTransactions = a} :: S3Settings)

-- | The delimiter used to separate rows in the .csv file for both source and
-- target. The default is a carriage return (@\\n@).
s3Settings_csvRowDelimiter :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_csvRowDelimiter = Lens.lens (\S3Settings' {csvRowDelimiter} -> csvRowDelimiter) (\s@S3Settings' {} a -> s {csvRowDelimiter = a} :: S3Settings)

-- | The version of the Apache Parquet format that you want to use:
-- @parquet_1_0@ (the default) or @parquet_2_0@.
s3Settings_parquetVersion :: Lens.Lens' S3Settings (Prelude.Maybe ParquetVersionValue)
s3Settings_parquetVersion = Lens.lens (\S3Settings' {parquetVersion} -> parquetVersion) (\s@S3Settings' {} a -> s {parquetVersion = a} :: S3Settings)

-- | Identifies the sequence of the date format to use during folder
-- partitioning. The default value is @YYYYMMDD@. Use this parameter when
-- @DatePartitionedEnabled@ is set to @true@.
s3Settings_datePartitionSequence :: Lens.Lens' S3Settings (Prelude.Maybe DatePartitionSequenceValue)
s3Settings_datePartitionSequence = Lens.lens (\S3Settings' {datePartitionSequence} -> datePartitionSequence) (\s@S3Settings' {} a -> s {datePartitionSequence = a} :: S3Settings)

-- | The name of the S3 bucket.
s3Settings_bucketName :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_bucketName = Lens.lens (\S3Settings' {bucketName} -> bucketName) (\s@S3Settings' {} a -> s {bucketName = a} :: S3Settings)

-- | Specifies the folder path of CDC files. For an S3 source, this setting
-- is required if a task captures change data; otherwise, it\'s optional.
-- If @CdcPath@ is set, AWS DMS reads CDC files from this path and
-- replicates the data changes to the target endpoint. For an S3 target if
-- you set
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-PreserveTransactions PreserveTransactions>
-- to @true@, AWS DMS verifies that you have set this parameter to a folder
-- path on your S3 target where AWS DMS can save the transaction order for
-- the CDC load. AWS DMS creates this CDC folder path in either your S3
-- target working directory or the S3 target location specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketFolder BucketFolder>
-- and
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketName BucketName>
-- .
--
-- For example, if you specify @CdcPath@ as @MyChangedData@, and you
-- specify @BucketName@ as @MyTargetBucket@ but do not specify
-- @BucketFolder@, AWS DMS creates the CDC folder path following:
-- @MyTargetBucket\/MyChangedData@.
--
-- If you specify the same @CdcPath@, and you specify @BucketName@ as
-- @MyTargetBucket@ and @BucketFolder@ as @MyTargetData@, AWS DMS creates
-- the CDC folder path following:
-- @MyTargetBucket\/MyTargetData\/MyChangedData@.
--
-- For more information on CDC including transaction order on an S3 target,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
--
-- This setting is supported in AWS DMS versions 3.4.2 and later.
s3Settings_cdcPath :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_cdcPath = Lens.lens (\S3Settings' {cdcPath} -> cdcPath) (\s@S3Settings' {} a -> s {cdcPath = a} :: S3Settings)

-- | Specifies how tables are defined in the S3 source files only.
s3Settings_externalTableDefinition :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_externalTableDefinition = Lens.lens (\S3Settings' {externalTableDefinition} -> externalTableDefinition) (\s@S3Settings' {} a -> s {externalTableDefinition = a} :: S3Settings)

-- | If you are using @SSE_KMS@ for the @EncryptionMode@, provide the AWS KMS
-- key ID. The key that you use needs an attached policy that enables AWS
-- Identity and Access Management (IAM) user permissions and allows use of
-- the key.
--
-- Here is a CLI example:
-- @aws dms create-endpoint --endpoint-identifier value --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=value,BucketFolder=value,BucketName=value,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=value @
s3Settings_serverSideEncryptionKmsKeyId :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_serverSideEncryptionKmsKeyId = Lens.lens (\S3Settings' {serverSideEncryptionKmsKeyId} -> serverSideEncryptionKmsKeyId) (\s@S3Settings' {} a -> s {serverSideEncryptionKmsKeyId = a} :: S3Settings)

-- | The size of one data page in bytes. This parameter defaults to 1024 *
-- 1024 bytes (1 MiB). This number is used for .parquet file format only.
s3Settings_dataPageSize :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_dataPageSize = Lens.lens (\S3Settings' {dataPageSize} -> dataPageSize) (\s@S3Settings' {} a -> s {dataPageSize = a} :: S3Settings)

-- | The type of encoding you are using:
--
-- -   @RLE_DICTIONARY@ uses a combination of bit-packing and run-length
--     encoding to store repeated values more efficiently. This is the
--     default.
--
-- -   @PLAIN@ doesn\'t use encoding at all. Values are stored as they are.
--
-- -   @PLAIN_DICTIONARY@ builds a dictionary of the values encountered in
--     a given column. The dictionary is stored in a dictionary page for
--     each column chunk.
s3Settings_encodingType :: Lens.Lens' S3Settings (Prelude.Maybe EncodingTypeValue)
s3Settings_encodingType = Lens.lens (\S3Settings' {encodingType} -> encodingType) (\s@S3Settings' {} a -> s {encodingType = a} :: S3Settings)

-- | When set to @true@, this parameter partitions S3 bucket folders based on
-- transaction commit dates. The default value is @false@. For more
-- information about date-based folder partitoning, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning>.
s3Settings_datePartitionEnabled :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_datePartitionEnabled = Lens.lens (\S3Settings' {datePartitionEnabled} -> datePartitionEnabled) (\s@S3Settings' {} a -> s {datePartitionEnabled = a} :: S3Settings)

-- | The format of the data that you want to use for output. You can choose
-- one of the following:
--
-- -   @csv@ : This is a row-based file format with comma-separated values
--     (.csv).
--
-- -   @parquet@ : Apache Parquet (.parquet) is a columnar storage file
--     format that features efficient compression and provides faster query
--     response.
s3Settings_dataFormat :: Lens.Lens' S3Settings (Prelude.Maybe DataFormatValue)
s3Settings_dataFormat = Lens.lens (\S3Settings' {dataFormat} -> dataFormat) (\s@S3Settings' {} a -> s {dataFormat = a} :: S3Settings)

-- | The Amazon Resource Name (ARN) used by the service access IAM role. It
-- is a required parameter that enables DMS to write and read objects from
-- an 3S bucket.
s3Settings_serviceAccessRoleArn :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_serviceAccessRoleArn = Lens.lens (\S3Settings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@S3Settings' {} a -> s {serviceAccessRoleArn = a} :: S3Settings)

-- | An optional parameter to set a folder name in the S3 bucket. If
-- provided, tables are created in the path
-- @ bucketFolder\/schema_name\/table_name\/@. If this parameter isn\'t
-- specified, then the path used is @ schema_name\/table_name\/@.
s3Settings_bucketFolder :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_bucketFolder = Lens.lens (\S3Settings' {bucketFolder} -> bucketFolder) (\s@S3Settings' {} a -> s {bucketFolder = a} :: S3Settings)

-- | Specifies a date separating delimiter to use during folder partitioning.
-- The default value is @SLASH@. Use this parameter when
-- @DatePartitionedEnabled@ is set to @true@.
s3Settings_datePartitionDelimiter :: Lens.Lens' S3Settings (Prelude.Maybe DatePartitionDelimiterValue)
s3Settings_datePartitionDelimiter = Lens.lens (\S3Settings' {datePartitionDelimiter} -> datePartitionDelimiter) (\s@S3Settings' {} a -> s {datePartitionDelimiter = a} :: S3Settings)

-- | A value that enables statistics for Parquet pages and row groups. Choose
-- @true@ to enable statistics, @false@ to disable. Statistics include
-- @NULL@, @DISTINCT@, @MAX@, and @MIN@ values. This parameter defaults to
-- @true@. This value is used for .parquet file format only.
s3Settings_enableStatistics :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_enableStatistics = Lens.lens (\S3Settings' {enableStatistics} -> enableStatistics) (\s@S3Settings' {} a -> s {enableStatistics = a} :: S3Settings)

-- | The type of server-side encryption that you want to use for your data.
-- This encryption type is part of the endpoint settings or the extra
-- connections attributes for Amazon S3. You can choose either @SSE_S3@
-- (the default) or @SSE_KMS@.
--
-- For the @ModifyEndpoint@ operation, you can change the existing value of
-- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
-- change the existing value from @SSE_S3@ to @SSE_KMS@.
--
-- To use @SSE_S3@, you need an AWS Identity and Access Management (IAM)
-- role with permission to allow @\"arn:aws:s3:::dms-*\"@ to use the
-- following actions:
--
-- -   @s3:CreateBucket@
--
-- -   @s3:ListBucket@
--
-- -   @s3:DeleteBucket@
--
-- -   @s3:GetBucketLocation@
--
-- -   @s3:GetObject@
--
-- -   @s3:PutObject@
--
-- -   @s3:DeleteObject@
--
-- -   @s3:GetObjectVersion@
--
-- -   @s3:GetBucketPolicy@
--
-- -   @s3:PutBucketPolicy@
--
-- -   @s3:DeleteBucketPolicy@
s3Settings_encryptionMode :: Lens.Lens' S3Settings (Prelude.Maybe EncryptionModeValue)
s3Settings_encryptionMode = Lens.lens (\S3Settings' {encryptionMode} -> encryptionMode) (\s@S3Settings' {} a -> s {encryptionMode = a} :: S3Settings)

-- | A value that enables a change data capture (CDC) load to write only
-- INSERT operations to .csv or columnar storage (.parquet) output files.
-- By default (the @false@ setting), the first field in a .csv or .parquet
-- record contains the letter I (INSERT), U (UPDATE), or D (DELETE). These
-- values indicate whether the row was inserted, updated, or deleted at the
-- source database for a CDC load to the target.
--
-- If @CdcInsertsOnly@ is set to @true@ or @y@, only INSERTs from the
-- source database are migrated to the .csv or .parquet file. For .csv
-- format only, how these INSERTs are recorded depends on the value of
-- @IncludeOpForFullLoad@. If @IncludeOpForFullLoad@ is set to @true@, the
-- first field of every CDC record is set to I to indicate the INSERT
-- operation at the source. If @IncludeOpForFullLoad@ is set to @false@,
-- every CDC record is written without a first field to indicate the INSERT
-- operation at the source. For more information about how these settings
-- work together, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
-- in the /AWS Database Migration Service User Guide./.
--
-- AWS DMS supports the interaction described preceding between the
-- @CdcInsertsOnly@ and @IncludeOpForFullLoad@ parameters in versions 3.1.4
-- and later.
--
-- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
-- for the same endpoint. Set either @CdcInsertsOnly@ or
-- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
s3Settings_cdcInsertsOnly :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_cdcInsertsOnly = Lens.lens (\S3Settings' {cdcInsertsOnly} -> cdcInsertsOnly) (\s@S3Settings' {} a -> s {cdcInsertsOnly = a} :: S3Settings)

-- | A value that enables a change data capture (CDC) load to write INSERT
-- and UPDATE operations to .csv or .parquet (columnar storage) output
-- files. The default setting is @false@, but when @CdcInsertsAndUpdates@
-- is set to @true@ or @y@, only INSERTs and UPDATEs from the source
-- database are migrated to the .csv or .parquet file.
--
-- For .csv file format only, how these INSERTs and UPDATEs are recorded
-- depends on the value of the @IncludeOpForFullLoad@ parameter. If
-- @IncludeOpForFullLoad@ is set to @true@, the first field of every CDC
-- record is set to either @I@ or @U@ to indicate INSERT and UPDATE
-- operations at the source. But if @IncludeOpForFullLoad@ is set to
-- @false@, CDC records are written without an indication of INSERT or
-- UPDATE operations at the source. For more information about how these
-- settings work together, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
-- in the /AWS Database Migration Service User Guide./.
--
-- AWS DMS supports the use of the @CdcInsertsAndUpdates@ parameter in
-- versions 3.3.1 and later.
--
-- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
-- for the same endpoint. Set either @CdcInsertsOnly@ or
-- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
s3Settings_cdcInsertsAndUpdates :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_cdcInsertsAndUpdates = Lens.lens (\S3Settings' {cdcInsertsAndUpdates} -> cdcInsertsAndUpdates) (\s@S3Settings' {} a -> s {cdcInsertsAndUpdates = a} :: S3Settings)

-- | This setting applies if the S3 output files during a change data capture
-- (CDC) load are written in .csv format. If set to @true@ for columns not
-- included in the supplemental log, AWS DMS uses the value specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CsvNoSupValue CsvNoSupValue>
-- . If not set or set to @false@, AWS DMS uses the null value for these
-- columns.
--
-- This setting is supported in AWS DMS versions 3.4.1 and later.
s3Settings_useCsvNoSupValue :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_useCsvNoSupValue = Lens.lens (\S3Settings' {useCsvNoSupValue} -> useCsvNoSupValue) (\s@S3Settings' {} a -> s {useCsvNoSupValue = a} :: S3Settings)

-- | The maximum size of an encoded dictionary page of a column. If the
-- dictionary page exceeds this, this column is stored using an encoding
-- type of @PLAIN@. This parameter defaults to 1024 * 1024 bytes (1 MiB),
-- the maximum size of a dictionary page before it reverts to @PLAIN@
-- encoding. This size is used for .parquet file format only.
s3Settings_dictPageSizeLimit :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_dictPageSizeLimit = Lens.lens (\S3Settings' {dictPageSizeLimit} -> dictPageSizeLimit) (\s@S3Settings' {} a -> s {dictPageSizeLimit = a} :: S3Settings)

-- | The number of rows in a row group. A smaller row group size provides
-- faster reads. But as the number of row groups grows, the slower writes
-- become. This parameter defaults to 10,000 rows. This number is used for
-- .parquet file format only.
--
-- If you choose a value larger than the maximum, @RowGroupLength@ is set
-- to the max row group length in bytes (64 * 1024 * 1024).
s3Settings_rowGroupLength :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_rowGroupLength = Lens.lens (\S3Settings' {rowGroupLength} -> rowGroupLength) (\s@S3Settings' {} a -> s {rowGroupLength = a} :: S3Settings)

-- | An optional parameter to use GZIP to compress the target files. Set to
-- GZIP to compress the target files. Either set this parameter to NONE
-- (the default) or don\'t use it to leave the files uncompressed. This
-- parameter applies to both .csv and .parquet file formats.
s3Settings_compressionType :: Lens.Lens' S3Settings (Prelude.Maybe CompressionTypeValue)
s3Settings_compressionType = Lens.lens (\S3Settings' {compressionType} -> compressionType) (\s@S3Settings' {} a -> s {compressionType = a} :: S3Settings)

-- | A value that enables a full load to write INSERT operations to the
-- comma-separated value (.csv) output files only to indicate how the rows
-- were added to the source database.
--
-- AWS DMS supports the @IncludeOpForFullLoad@ parameter in versions 3.1.4
-- and later.
--
-- For full load, records can only be inserted. By default (the @false@
-- setting), no information is recorded in these output files for a full
-- load to indicate that the rows were inserted at the source database. If
-- @IncludeOpForFullLoad@ is set to @true@ or @y@, the INSERT is recorded
-- as an I annotation in the first field of the .csv file. This allows the
-- format of your target records from a full load to be consistent with the
-- target records from a CDC load.
--
-- This setting works together with the @CdcInsertsOnly@ and the
-- @CdcInsertsAndUpdates@ parameters for output to .csv files only. For
-- more information about how these settings work together, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.Configuring.InsertOps Indicating Source DB Operations in Migrated S3 Data>
-- in the /AWS Database Migration Service User Guide./.
s3Settings_includeOpForFullLoad :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_includeOpForFullLoad = Lens.lens (\S3Settings' {includeOpForFullLoad} -> includeOpForFullLoad) (\s@S3Settings' {} a -> s {includeOpForFullLoad = a} :: S3Settings)

-- | The delimiter used to separate columns in the .csv file for both source
-- and target. The default is a comma.
s3Settings_csvDelimiter :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_csvDelimiter = Lens.lens (\S3Settings' {csvDelimiter} -> csvDelimiter) (\s@S3Settings' {} a -> s {csvDelimiter = a} :: S3Settings)

-- | A value that specifies the precision of any @TIMESTAMP@ column values
-- that are written to an Amazon S3 object file in .parquet format.
--
-- AWS DMS supports the @ParquetTimestampInMillisecond@ parameter in
-- versions 3.1.4 and later.
--
-- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@, AWS DMS
-- writes all @TIMESTAMP@ columns in a .parquet formatted file with
-- millisecond precision. Otherwise, DMS writes them with microsecond
-- precision.
--
-- Currently, Amazon Athena and AWS Glue can handle only millisecond
-- precision for @TIMESTAMP@ values. Set this parameter to @true@ for S3
-- endpoint object files that are .parquet formatted only if you plan to
-- query or process the data with Athena or AWS Glue.
--
-- AWS DMS writes any @TIMESTAMP@ column values written to an S3 file in
-- .csv format with microsecond precision.
--
-- Setting @ParquetTimestampInMillisecond@ has no effect on the string
-- format of the timestamp column value that is inserted by setting the
-- @TimestampColumnName@ parameter.
s3Settings_parquetTimestampInMillisecond :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_parquetTimestampInMillisecond = Lens.lens (\S3Settings' {parquetTimestampInMillisecond} -> parquetTimestampInMillisecond) (\s@S3Settings' {} a -> s {parquetTimestampInMillisecond = a} :: S3Settings)

-- | This setting only applies if your Amazon S3 output files during a change
-- data capture (CDC) load are written in .csv format. If
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-UseCsvNoSupValue UseCsvNoSupValue>
-- is set to true, specify a string value that you want AWS DMS to use for
-- all columns not included in the supplemental log. If you do not specify
-- a string value, AWS DMS uses the null value for these columns regardless
-- of the @UseCsvNoSupValue@ setting.
--
-- This setting is supported in AWS DMS versions 3.4.1 and later.
s3Settings_csvNoSupValue :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_csvNoSupValue = Lens.lens (\S3Settings' {csvNoSupValue} -> csvNoSupValue) (\s@S3Settings' {} a -> s {csvNoSupValue = a} :: S3Settings)

instance Prelude.FromJSON S3Settings where
  parseJSON =
    Prelude.withObject
      "S3Settings"
      ( \x ->
          S3Settings'
            Prelude.<$> (x Prelude..:? "TimestampColumnName")
            Prelude.<*> (x Prelude..:? "PreserveTransactions")
            Prelude.<*> (x Prelude..:? "CsvRowDelimiter")
            Prelude.<*> (x Prelude..:? "ParquetVersion")
            Prelude.<*> (x Prelude..:? "DatePartitionSequence")
            Prelude.<*> (x Prelude..:? "BucketName")
            Prelude.<*> (x Prelude..:? "CdcPath")
            Prelude.<*> (x Prelude..:? "ExternalTableDefinition")
            Prelude.<*> (x Prelude..:? "ServerSideEncryptionKmsKeyId")
            Prelude.<*> (x Prelude..:? "DataPageSize")
            Prelude.<*> (x Prelude..:? "EncodingType")
            Prelude.<*> (x Prelude..:? "DatePartitionEnabled")
            Prelude.<*> (x Prelude..:? "DataFormat")
            Prelude.<*> (x Prelude..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Prelude..:? "BucketFolder")
            Prelude.<*> (x Prelude..:? "DatePartitionDelimiter")
            Prelude.<*> (x Prelude..:? "EnableStatistics")
            Prelude.<*> (x Prelude..:? "EncryptionMode")
            Prelude.<*> (x Prelude..:? "CdcInsertsOnly")
            Prelude.<*> (x Prelude..:? "CdcInsertsAndUpdates")
            Prelude.<*> (x Prelude..:? "UseCsvNoSupValue")
            Prelude.<*> (x Prelude..:? "DictPageSizeLimit")
            Prelude.<*> (x Prelude..:? "RowGroupLength")
            Prelude.<*> (x Prelude..:? "CompressionType")
            Prelude.<*> (x Prelude..:? "IncludeOpForFullLoad")
            Prelude.<*> (x Prelude..:? "CsvDelimiter")
            Prelude.<*> (x Prelude..:? "ParquetTimestampInMillisecond")
            Prelude.<*> (x Prelude..:? "CsvNoSupValue")
      )

instance Prelude.Hashable S3Settings

instance Prelude.NFData S3Settings

instance Prelude.ToJSON S3Settings where
  toJSON S3Settings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TimestampColumnName" Prelude..=)
              Prelude.<$> timestampColumnName,
            ("PreserveTransactions" Prelude..=)
              Prelude.<$> preserveTransactions,
            ("CsvRowDelimiter" Prelude..=)
              Prelude.<$> csvRowDelimiter,
            ("ParquetVersion" Prelude..=)
              Prelude.<$> parquetVersion,
            ("DatePartitionSequence" Prelude..=)
              Prelude.<$> datePartitionSequence,
            ("BucketName" Prelude..=) Prelude.<$> bucketName,
            ("CdcPath" Prelude..=) Prelude.<$> cdcPath,
            ("ExternalTableDefinition" Prelude..=)
              Prelude.<$> externalTableDefinition,
            ("ServerSideEncryptionKmsKeyId" Prelude..=)
              Prelude.<$> serverSideEncryptionKmsKeyId,
            ("DataPageSize" Prelude..=) Prelude.<$> dataPageSize,
            ("EncodingType" Prelude..=) Prelude.<$> encodingType,
            ("DatePartitionEnabled" Prelude..=)
              Prelude.<$> datePartitionEnabled,
            ("DataFormat" Prelude..=) Prelude.<$> dataFormat,
            ("ServiceAccessRoleArn" Prelude..=)
              Prelude.<$> serviceAccessRoleArn,
            ("BucketFolder" Prelude..=) Prelude.<$> bucketFolder,
            ("DatePartitionDelimiter" Prelude..=)
              Prelude.<$> datePartitionDelimiter,
            ("EnableStatistics" Prelude..=)
              Prelude.<$> enableStatistics,
            ("EncryptionMode" Prelude..=)
              Prelude.<$> encryptionMode,
            ("CdcInsertsOnly" Prelude..=)
              Prelude.<$> cdcInsertsOnly,
            ("CdcInsertsAndUpdates" Prelude..=)
              Prelude.<$> cdcInsertsAndUpdates,
            ("UseCsvNoSupValue" Prelude..=)
              Prelude.<$> useCsvNoSupValue,
            ("DictPageSizeLimit" Prelude..=)
              Prelude.<$> dictPageSizeLimit,
            ("RowGroupLength" Prelude..=)
              Prelude.<$> rowGroupLength,
            ("CompressionType" Prelude..=)
              Prelude.<$> compressionType,
            ("IncludeOpForFullLoad" Prelude..=)
              Prelude.<$> includeOpForFullLoad,
            ("CsvDelimiter" Prelude..=) Prelude.<$> csvDelimiter,
            ("ParquetTimestampInMillisecond" Prelude..=)
              Prelude.<$> parquetTimestampInMillisecond,
            ("CsvNoSupValue" Prelude..=)
              Prelude.<$> csvNoSupValue
          ]
      )

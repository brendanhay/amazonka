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
-- Module      : Amazonka.DMS.Types.S3Settings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.S3Settings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.CannedAclForObjectsValue
import Amazonka.DMS.Types.CompressionTypeValue
import Amazonka.DMS.Types.DataFormatValue
import Amazonka.DMS.Types.DatePartitionDelimiterValue
import Amazonka.DMS.Types.DatePartitionSequenceValue
import Amazonka.DMS.Types.EncodingTypeValue
import Amazonka.DMS.Types.EncryptionModeValue
import Amazonka.DMS.Types.ParquetVersionValue
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings for exporting data to Amazon S3.
--
-- /See:/ 'newS3Settings' smart constructor.
data S3Settings = S3Settings'
  { -- | An optional parameter that, when set to @true@ or @y@, you can use to
    -- add column name information to the .csv output file.
    --
    -- The default value is @false@. Valid values are @true@, @false@, @y@, and
    -- @n@.
    addColumnName :: Prelude.Maybe Prelude.Bool,
    -- | Use the S3 target endpoint setting @AddTrailingPaddingCharacter@ to add
    -- padding on string data. The default value is @false@.
    addTrailingPaddingCharacter :: Prelude.Maybe Prelude.Bool,
    -- | An optional parameter to set a folder name in the S3 bucket. If
    -- provided, tables are created in the path
    -- @ bucketFolder\/schema_name\/table_name\/@. If this parameter isn\'t
    -- specified, then the path used is @ schema_name\/table_name\/@.
    bucketFolder :: Prelude.Maybe Prelude.Text,
    -- | The name of the S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | A value that enables DMS to specify a predefined (canned) access control
    -- list for objects created in an Amazon S3 bucket as .csv or .parquet
    -- files. For more information about Amazon S3 canned ACLs, see
    -- <http://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACL>
    -- in the /Amazon S3 Developer Guide./
    --
    -- The default value is NONE. Valid values include NONE, PRIVATE,
    -- PUBLIC_READ, PUBLIC_READ_WRITE, AUTHENTICATED_READ, AWS_EXEC_READ,
    -- BUCKET_OWNER_READ, and BUCKET_OWNER_FULL_CONTROL.
    cannedAclForObjects :: Prelude.Maybe CannedAclForObjectsValue,
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
    -- in the /Database Migration Service User Guide./.
    --
    -- DMS supports the use of the @CdcInsertsAndUpdates@ parameter in versions
    -- 3.3.1 and later.
    --
    -- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
    -- for the same endpoint. Set either @CdcInsertsOnly@ or
    -- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
    cdcInsertsAndUpdates :: Prelude.Maybe Prelude.Bool,
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
    -- in the /Database Migration Service User Guide./.
    --
    -- DMS supports the interaction described preceding between the
    -- @CdcInsertsOnly@ and @IncludeOpForFullLoad@ parameters in versions 3.1.4
    -- and later.
    --
    -- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
    -- for the same endpoint. Set either @CdcInsertsOnly@ or
    -- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
    cdcInsertsOnly :: Prelude.Maybe Prelude.Bool,
    -- | Maximum length of the interval, defined in seconds, after which to
    -- output a file to Amazon S3.
    --
    -- When @CdcMaxBatchInterval@ and @CdcMinFileSize@ are both specified, the
    -- file write is triggered by whichever parameter condition is met first
    -- within an DMS CloudFormation template.
    --
    -- The default value is 60 seconds.
    cdcMaxBatchInterval :: Prelude.Maybe Prelude.Int,
    -- | Minimum file size, defined in kilobytes, to reach for a file output to
    -- Amazon S3.
    --
    -- When @CdcMinFileSize@ and @CdcMaxBatchInterval@ are both specified, the
    -- file write is triggered by whichever parameter condition is met first
    -- within an DMS CloudFormation template.
    --
    -- The default value is 32 MB.
    cdcMinFileSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies the folder path of CDC files. For an S3 source, this setting
    -- is required if a task captures change data; otherwise, it\'s optional.
    -- If @CdcPath@ is set, DMS reads CDC files from this path and replicates
    -- the data changes to the target endpoint. For an S3 target if you set
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-PreserveTransactions PreserveTransactions>
    -- to @true@, DMS verifies that you have set this parameter to a folder
    -- path on your S3 target where DMS can save the transaction order for the
    -- CDC load. DMS creates this CDC folder path in either your S3 target
    -- working directory or the S3 target location specified by
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketFolder BucketFolder>
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketName BucketName>
    -- .
    --
    -- For example, if you specify @CdcPath@ as @MyChangedData@, and you
    -- specify @BucketName@ as @MyTargetBucket@ but do not specify
    -- @BucketFolder@, DMS creates the CDC folder path following:
    -- @MyTargetBucket\/MyChangedData@.
    --
    -- If you specify the same @CdcPath@, and you specify @BucketName@ as
    -- @MyTargetBucket@ and @BucketFolder@ as @MyTargetData@, DMS creates the
    -- CDC folder path following:
    -- @MyTargetBucket\/MyTargetData\/MyChangedData@.
    --
    -- For more information on CDC including transaction order on an S3 target,
    -- see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
    --
    -- This setting is supported in DMS versions 3.4.2 and later.
    cdcPath :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter to use GZIP to compress the target files. Set to
    -- GZIP to compress the target files. Either set this parameter to NONE
    -- (the default) or don\'t use it to leave the files uncompressed. This
    -- parameter applies to both .csv and .parquet file formats.
    compressionType :: Prelude.Maybe CompressionTypeValue,
    -- | The delimiter used to separate columns in the .csv file for both source
    -- and target. The default is a comma.
    csvDelimiter :: Prelude.Maybe Prelude.Text,
    -- | This setting only applies if your Amazon S3 output files during a change
    -- data capture (CDC) load are written in .csv format. If
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-UseCsvNoSupValue UseCsvNoSupValue>
    -- is set to true, specify a string value that you want DMS to use for all
    -- columns not included in the supplemental log. If you do not specify a
    -- string value, DMS uses the null value for these columns regardless of
    -- the @UseCsvNoSupValue@ setting.
    --
    -- This setting is supported in DMS versions 3.4.1 and later.
    csvNoSupValue :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies how DMS treats null values. While
    -- handling the null value, you can use this parameter to pass a
    -- user-defined string as null when writing to the target. For example,
    -- when target columns are not nullable, you can use this option to
    -- differentiate between the empty string value and the null value. So, if
    -- you set this parameter value to the empty string (\"\" or \'\'), DMS
    -- treats the empty string as the null value instead of @NULL@.
    --
    -- The default value is @NULL@. Valid values include any valid string.
    csvNullValue :: Prelude.Maybe Prelude.Text,
    -- | The delimiter used to separate rows in the .csv file for both source and
    -- target. The default is a carriage return (@\\n@).
    csvRowDelimiter :: Prelude.Maybe Prelude.Text,
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
    -- | The size of one data page in bytes. This parameter defaults to 1024 *
    -- 1024 bytes (1 MiB). This number is used for .parquet file format only.
    dataPageSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies a date separating delimiter to use during folder partitioning.
    -- The default value is @SLASH@. Use this parameter when
    -- @DatePartitionedEnabled@ is set to @true@.
    datePartitionDelimiter :: Prelude.Maybe DatePartitionDelimiterValue,
    -- | When set to @true@, this parameter partitions S3 bucket folders based on
    -- transaction commit dates. The default value is @false@. For more
    -- information about date-based folder partitioning, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning>.
    datePartitionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Identifies the sequence of the date format to use during folder
    -- partitioning. The default value is @YYYYMMDD@. Use this parameter when
    -- @DatePartitionedEnabled@ is set to @true@.
    datePartitionSequence :: Prelude.Maybe DatePartitionSequenceValue,
    -- | When creating an S3 target endpoint, set @DatePartitionTimezone@ to
    -- convert the current UTC time into a specified time zone. The conversion
    -- occurs when a date partition folder is created and a CDC filename is
    -- generated. The time zone format is Area\/Location. Use this parameter
    -- when @DatePartitionedEnabled@ is set to @true@, as shown in the
    -- following example.
    --
    -- @s3-settings=\'{\"DatePartitionEnabled\": true, \"DatePartitionSequence\": \"YYYYMMDDHH\", \"DatePartitionDelimiter\": \"SLASH\", \"DatePartitionTimezone\":\"Asia\/Seoul\", \"BucketName\": \"dms-nattarat-test\"}\'@
    datePartitionTimezone :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of an encoded dictionary page of a column. If the
    -- dictionary page exceeds this, this column is stored using an encoding
    -- type of @PLAIN@. This parameter defaults to 1024 * 1024 bytes (1 MiB),
    -- the maximum size of a dictionary page before it reverts to @PLAIN@
    -- encoding. This size is used for .parquet file format only.
    dictPageSizeLimit :: Prelude.Maybe Prelude.Int,
    -- | A value that enables statistics for Parquet pages and row groups. Choose
    -- @true@ to enable statistics, @false@ to disable. Statistics include
    -- @NULL@, @DISTINCT@, @MAX@, and @MIN@ values. This parameter defaults to
    -- @true@. This value is used for .parquet file format only.
    enableStatistics :: Prelude.Maybe Prelude.Bool,
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
    -- | The type of server-side encryption that you want to use for your data.
    -- This encryption type is part of the endpoint settings or the extra
    -- connections attributes for Amazon S3. You can choose either @SSE_S3@
    -- (the default) or @SSE_KMS@.
    --
    -- For the @ModifyEndpoint@ operation, you can change the existing value of
    -- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
    -- change the existing value from @SSE_S3@ to @SSE_KMS@.
    --
    -- To use @SSE_S3@, you need an Identity and Access Management (IAM) role
    -- with permission to allow @\"arn:aws:s3:::dms-*\"@ to use the following
    -- actions:
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
    -- | To specify a bucket owner and prevent sniping, you can use the
    -- @ExpectedBucketOwner@ endpoint setting.
    --
    -- Example:
    -- @--s3-settings=\'{\"ExpectedBucketOwner\": \"AWS_Account_ID\"}\'@
    --
    -- When you make a request to test a connection or perform a migration, S3
    -- checks the account ID of the bucket owner against the specified
    -- parameter.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies how tables are defined in the S3 source files only.
    externalTableDefinition :: Prelude.Maybe Prelude.Text,
    -- | When this value is set to 1, DMS ignores the first row header in a .csv
    -- file. A value of 1 turns on the feature; a value of 0 turns off the
    -- feature.
    --
    -- The default is 0.
    ignoreHeaderRows :: Prelude.Maybe Prelude.Int,
    -- | A value that enables a full load to write INSERT operations to the
    -- comma-separated value (.csv) output files only to indicate how the rows
    -- were added to the source database.
    --
    -- DMS supports the @IncludeOpForFullLoad@ parameter in versions 3.1.4 and
    -- later.
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
    -- in the /Database Migration Service User Guide./.
    includeOpForFullLoad :: Prelude.Maybe Prelude.Bool,
    -- | A value that specifies the maximum size (in KB) of any .csv file to be
    -- created while migrating to an S3 target during full load.
    --
    -- The default value is 1,048,576 KB (1 GB). Valid values include 1 to
    -- 1,048,576.
    maxFileSize :: Prelude.Maybe Prelude.Int,
    -- | A value that specifies the precision of any @TIMESTAMP@ column values
    -- that are written to an Amazon S3 object file in .parquet format.
    --
    -- DMS supports the @ParquetTimestampInMillisecond@ parameter in versions
    -- 3.1.4 and later.
    --
    -- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@, DMS writes
    -- all @TIMESTAMP@ columns in a .parquet formatted file with millisecond
    -- precision. Otherwise, DMS writes them with microsecond precision.
    --
    -- Currently, Amazon Athena and Glue can handle only millisecond precision
    -- for @TIMESTAMP@ values. Set this parameter to @true@ for S3 endpoint
    -- object files that are .parquet formatted only if you plan to query or
    -- process the data with Athena or Glue.
    --
    -- DMS writes any @TIMESTAMP@ column values written to an S3 file in .csv
    -- format with microsecond precision.
    --
    -- Setting @ParquetTimestampInMillisecond@ has no effect on the string
    -- format of the timestamp column value that is inserted by setting the
    -- @TimestampColumnName@ parameter.
    parquetTimestampInMillisecond :: Prelude.Maybe Prelude.Bool,
    -- | The version of the Apache Parquet format that you want to use:
    -- @parquet_1_0@ (the default) or @parquet_2_0@.
    parquetVersion :: Prelude.Maybe ParquetVersionValue,
    -- | If set to @true@, DMS saves the transaction order for a change data
    -- capture (CDC) load on the Amazon S3 target specified by
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CdcPath CdcPath>
    -- . For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
    --
    -- This setting is supported in DMS versions 3.4.2 and later.
    preserveTransactions :: Prelude.Maybe Prelude.Bool,
    -- | For an S3 source, when this value is set to @true@ or @y@, each leading
    -- double quotation mark has to be followed by an ending double quotation
    -- mark. This formatting complies with RFC 4180. When this value is set to
    -- @false@ or @n@, string literals are copied to the target as is. In this
    -- case, a delimiter (row or column) signals the end of the field. Thus,
    -- you can\'t use a delimiter as part of the string, because it signals the
    -- end of the value.
    --
    -- For an S3 target, an optional parameter used to set behavior to comply
    -- with RFC 4180 for data migrated to Amazon S3 using .csv file format
    -- only. When this value is set to @true@ or @y@ using Amazon S3 as a
    -- target, if the data has quotation marks or newline characters in it, DMS
    -- encloses the entire column with an additional pair of double quotation
    -- marks (\"). Every quotation mark within the data is repeated twice.
    --
    -- The default value is @true@. Valid values include @true@, @false@, @y@,
    -- and @n@.
    rfc4180 :: Prelude.Maybe Prelude.Bool,
    -- | The number of rows in a row group. A smaller row group size provides
    -- faster reads. But as the number of row groups grows, the slower writes
    -- become. This parameter defaults to 10,000 rows. This number is used for
    -- .parquet file format only.
    --
    -- If you choose a value larger than the maximum, @RowGroupLength@ is set
    -- to the max row group length in bytes (64 * 1024 * 1024).
    rowGroupLength :: Prelude.Maybe Prelude.Int,
    -- | If you are using @SSE_KMS@ for the @EncryptionMode@, provide the KMS key
    -- ID. The key that you use needs an attached policy that enables Identity
    -- and Access Management (IAM) user permissions and allows use of the key.
    --
    -- Here is a CLI example:
    -- @aws dms create-endpoint --endpoint-identifier value --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=value,BucketFolder=value,BucketName=value,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=value @
    serverSideEncryptionKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) used by the service to access the IAM
    -- role. The role must allow the @iam:PassRole@ action. It is a required
    -- parameter that enables DMS to write and read objects from an S3 bucket.
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A value that when nonblank causes DMS to add a column with timestamp
    -- information to the endpoint data for an Amazon S3 target.
    --
    -- DMS supports the @TimestampColumnName@ parameter in versions 3.1.4 and
    -- later.
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
    -- | This setting applies if the S3 output files during a change data capture
    -- (CDC) load are written in .csv format. If set to @true@ for columns not
    -- included in the supplemental log, DMS uses the value specified by
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CsvNoSupValue CsvNoSupValue>
    -- . If not set or set to @false@, DMS uses the null value for these
    -- columns.
    --
    -- This setting is supported in DMS versions 3.4.1 and later.
    useCsvNoSupValue :: Prelude.Maybe Prelude.Bool,
    -- | When set to true, this parameter uses the task start time as the
    -- timestamp column value instead of the time data is written to target.
    -- For full load, when @useTaskStartTimeForFullLoadTimestamp@ is set to
    -- @true@, each row of the timestamp column contains the task start time.
    -- For CDC loads, each row of the timestamp column contains the transaction
    -- commit time.
    --
    -- When @useTaskStartTimeForFullLoadTimestamp@ is set to @false@, the full
    -- load timestamp in the timestamp column increments with the time data
    -- arrives at the target.
    useTaskStartTimeForFullLoadTimestamp :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addColumnName', 's3Settings_addColumnName' - An optional parameter that, when set to @true@ or @y@, you can use to
-- add column name information to the .csv output file.
--
-- The default value is @false@. Valid values are @true@, @false@, @y@, and
-- @n@.
--
-- 'addTrailingPaddingCharacter', 's3Settings_addTrailingPaddingCharacter' - Use the S3 target endpoint setting @AddTrailingPaddingCharacter@ to add
-- padding on string data. The default value is @false@.
--
-- 'bucketFolder', 's3Settings_bucketFolder' - An optional parameter to set a folder name in the S3 bucket. If
-- provided, tables are created in the path
-- @ bucketFolder\/schema_name\/table_name\/@. If this parameter isn\'t
-- specified, then the path used is @ schema_name\/table_name\/@.
--
-- 'bucketName', 's3Settings_bucketName' - The name of the S3 bucket.
--
-- 'cannedAclForObjects', 's3Settings_cannedAclForObjects' - A value that enables DMS to specify a predefined (canned) access control
-- list for objects created in an Amazon S3 bucket as .csv or .parquet
-- files. For more information about Amazon S3 canned ACLs, see
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACL>
-- in the /Amazon S3 Developer Guide./
--
-- The default value is NONE. Valid values include NONE, PRIVATE,
-- PUBLIC_READ, PUBLIC_READ_WRITE, AUTHENTICATED_READ, AWS_EXEC_READ,
-- BUCKET_OWNER_READ, and BUCKET_OWNER_FULL_CONTROL.
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
-- in the /Database Migration Service User Guide./.
--
-- DMS supports the use of the @CdcInsertsAndUpdates@ parameter in versions
-- 3.3.1 and later.
--
-- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
-- for the same endpoint. Set either @CdcInsertsOnly@ or
-- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
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
-- in the /Database Migration Service User Guide./.
--
-- DMS supports the interaction described preceding between the
-- @CdcInsertsOnly@ and @IncludeOpForFullLoad@ parameters in versions 3.1.4
-- and later.
--
-- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
-- for the same endpoint. Set either @CdcInsertsOnly@ or
-- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
--
-- 'cdcMaxBatchInterval', 's3Settings_cdcMaxBatchInterval' - Maximum length of the interval, defined in seconds, after which to
-- output a file to Amazon S3.
--
-- When @CdcMaxBatchInterval@ and @CdcMinFileSize@ are both specified, the
-- file write is triggered by whichever parameter condition is met first
-- within an DMS CloudFormation template.
--
-- The default value is 60 seconds.
--
-- 'cdcMinFileSize', 's3Settings_cdcMinFileSize' - Minimum file size, defined in kilobytes, to reach for a file output to
-- Amazon S3.
--
-- When @CdcMinFileSize@ and @CdcMaxBatchInterval@ are both specified, the
-- file write is triggered by whichever parameter condition is met first
-- within an DMS CloudFormation template.
--
-- The default value is 32 MB.
--
-- 'cdcPath', 's3Settings_cdcPath' - Specifies the folder path of CDC files. For an S3 source, this setting
-- is required if a task captures change data; otherwise, it\'s optional.
-- If @CdcPath@ is set, DMS reads CDC files from this path and replicates
-- the data changes to the target endpoint. For an S3 target if you set
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-PreserveTransactions PreserveTransactions>
-- to @true@, DMS verifies that you have set this parameter to a folder
-- path on your S3 target where DMS can save the transaction order for the
-- CDC load. DMS creates this CDC folder path in either your S3 target
-- working directory or the S3 target location specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketFolder BucketFolder>
-- and
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketName BucketName>
-- .
--
-- For example, if you specify @CdcPath@ as @MyChangedData@, and you
-- specify @BucketName@ as @MyTargetBucket@ but do not specify
-- @BucketFolder@, DMS creates the CDC folder path following:
-- @MyTargetBucket\/MyChangedData@.
--
-- If you specify the same @CdcPath@, and you specify @BucketName@ as
-- @MyTargetBucket@ and @BucketFolder@ as @MyTargetData@, DMS creates the
-- CDC folder path following:
-- @MyTargetBucket\/MyTargetData\/MyChangedData@.
--
-- For more information on CDC including transaction order on an S3 target,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
--
-- This setting is supported in DMS versions 3.4.2 and later.
--
-- 'compressionType', 's3Settings_compressionType' - An optional parameter to use GZIP to compress the target files. Set to
-- GZIP to compress the target files. Either set this parameter to NONE
-- (the default) or don\'t use it to leave the files uncompressed. This
-- parameter applies to both .csv and .parquet file formats.
--
-- 'csvDelimiter', 's3Settings_csvDelimiter' - The delimiter used to separate columns in the .csv file for both source
-- and target. The default is a comma.
--
-- 'csvNoSupValue', 's3Settings_csvNoSupValue' - This setting only applies if your Amazon S3 output files during a change
-- data capture (CDC) load are written in .csv format. If
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-UseCsvNoSupValue UseCsvNoSupValue>
-- is set to true, specify a string value that you want DMS to use for all
-- columns not included in the supplemental log. If you do not specify a
-- string value, DMS uses the null value for these columns regardless of
-- the @UseCsvNoSupValue@ setting.
--
-- This setting is supported in DMS versions 3.4.1 and later.
--
-- 'csvNullValue', 's3Settings_csvNullValue' - An optional parameter that specifies how DMS treats null values. While
-- handling the null value, you can use this parameter to pass a
-- user-defined string as null when writing to the target. For example,
-- when target columns are not nullable, you can use this option to
-- differentiate between the empty string value and the null value. So, if
-- you set this parameter value to the empty string (\"\" or \'\'), DMS
-- treats the empty string as the null value instead of @NULL@.
--
-- The default value is @NULL@. Valid values include any valid string.
--
-- 'csvRowDelimiter', 's3Settings_csvRowDelimiter' - The delimiter used to separate rows in the .csv file for both source and
-- target. The default is a carriage return (@\\n@).
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
-- 'dataPageSize', 's3Settings_dataPageSize' - The size of one data page in bytes. This parameter defaults to 1024 *
-- 1024 bytes (1 MiB). This number is used for .parquet file format only.
--
-- 'datePartitionDelimiter', 's3Settings_datePartitionDelimiter' - Specifies a date separating delimiter to use during folder partitioning.
-- The default value is @SLASH@. Use this parameter when
-- @DatePartitionedEnabled@ is set to @true@.
--
-- 'datePartitionEnabled', 's3Settings_datePartitionEnabled' - When set to @true@, this parameter partitions S3 bucket folders based on
-- transaction commit dates. The default value is @false@. For more
-- information about date-based folder partitioning, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning>.
--
-- 'datePartitionSequence', 's3Settings_datePartitionSequence' - Identifies the sequence of the date format to use during folder
-- partitioning. The default value is @YYYYMMDD@. Use this parameter when
-- @DatePartitionedEnabled@ is set to @true@.
--
-- 'datePartitionTimezone', 's3Settings_datePartitionTimezone' - When creating an S3 target endpoint, set @DatePartitionTimezone@ to
-- convert the current UTC time into a specified time zone. The conversion
-- occurs when a date partition folder is created and a CDC filename is
-- generated. The time zone format is Area\/Location. Use this parameter
-- when @DatePartitionedEnabled@ is set to @true@, as shown in the
-- following example.
--
-- @s3-settings=\'{\"DatePartitionEnabled\": true, \"DatePartitionSequence\": \"YYYYMMDDHH\", \"DatePartitionDelimiter\": \"SLASH\", \"DatePartitionTimezone\":\"Asia\/Seoul\", \"BucketName\": \"dms-nattarat-test\"}\'@
--
-- 'dictPageSizeLimit', 's3Settings_dictPageSizeLimit' - The maximum size of an encoded dictionary page of a column. If the
-- dictionary page exceeds this, this column is stored using an encoding
-- type of @PLAIN@. This parameter defaults to 1024 * 1024 bytes (1 MiB),
-- the maximum size of a dictionary page before it reverts to @PLAIN@
-- encoding. This size is used for .parquet file format only.
--
-- 'enableStatistics', 's3Settings_enableStatistics' - A value that enables statistics for Parquet pages and row groups. Choose
-- @true@ to enable statistics, @false@ to disable. Statistics include
-- @NULL@, @DISTINCT@, @MAX@, and @MIN@ values. This parameter defaults to
-- @true@. This value is used for .parquet file format only.
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
-- 'encryptionMode', 's3Settings_encryptionMode' - The type of server-side encryption that you want to use for your data.
-- This encryption type is part of the endpoint settings or the extra
-- connections attributes for Amazon S3. You can choose either @SSE_S3@
-- (the default) or @SSE_KMS@.
--
-- For the @ModifyEndpoint@ operation, you can change the existing value of
-- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
-- change the existing value from @SSE_S3@ to @SSE_KMS@.
--
-- To use @SSE_S3@, you need an Identity and Access Management (IAM) role
-- with permission to allow @\"arn:aws:s3:::dms-*\"@ to use the following
-- actions:
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
-- 'expectedBucketOwner', 's3Settings_expectedBucketOwner' - To specify a bucket owner and prevent sniping, you can use the
-- @ExpectedBucketOwner@ endpoint setting.
--
-- Example:
-- @--s3-settings=\'{\"ExpectedBucketOwner\": \"AWS_Account_ID\"}\'@
--
-- When you make a request to test a connection or perform a migration, S3
-- checks the account ID of the bucket owner against the specified
-- parameter.
--
-- 'externalTableDefinition', 's3Settings_externalTableDefinition' - Specifies how tables are defined in the S3 source files only.
--
-- 'ignoreHeaderRows', 's3Settings_ignoreHeaderRows' - When this value is set to 1, DMS ignores the first row header in a .csv
-- file. A value of 1 turns on the feature; a value of 0 turns off the
-- feature.
--
-- The default is 0.
--
-- 'includeOpForFullLoad', 's3Settings_includeOpForFullLoad' - A value that enables a full load to write INSERT operations to the
-- comma-separated value (.csv) output files only to indicate how the rows
-- were added to the source database.
--
-- DMS supports the @IncludeOpForFullLoad@ parameter in versions 3.1.4 and
-- later.
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
-- in the /Database Migration Service User Guide./.
--
-- 'maxFileSize', 's3Settings_maxFileSize' - A value that specifies the maximum size (in KB) of any .csv file to be
-- created while migrating to an S3 target during full load.
--
-- The default value is 1,048,576 KB (1 GB). Valid values include 1 to
-- 1,048,576.
--
-- 'parquetTimestampInMillisecond', 's3Settings_parquetTimestampInMillisecond' - A value that specifies the precision of any @TIMESTAMP@ column values
-- that are written to an Amazon S3 object file in .parquet format.
--
-- DMS supports the @ParquetTimestampInMillisecond@ parameter in versions
-- 3.1.4 and later.
--
-- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@, DMS writes
-- all @TIMESTAMP@ columns in a .parquet formatted file with millisecond
-- precision. Otherwise, DMS writes them with microsecond precision.
--
-- Currently, Amazon Athena and Glue can handle only millisecond precision
-- for @TIMESTAMP@ values. Set this parameter to @true@ for S3 endpoint
-- object files that are .parquet formatted only if you plan to query or
-- process the data with Athena or Glue.
--
-- DMS writes any @TIMESTAMP@ column values written to an S3 file in .csv
-- format with microsecond precision.
--
-- Setting @ParquetTimestampInMillisecond@ has no effect on the string
-- format of the timestamp column value that is inserted by setting the
-- @TimestampColumnName@ parameter.
--
-- 'parquetVersion', 's3Settings_parquetVersion' - The version of the Apache Parquet format that you want to use:
-- @parquet_1_0@ (the default) or @parquet_2_0@.
--
-- 'preserveTransactions', 's3Settings_preserveTransactions' - If set to @true@, DMS saves the transaction order for a change data
-- capture (CDC) load on the Amazon S3 target specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CdcPath CdcPath>
-- . For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
--
-- This setting is supported in DMS versions 3.4.2 and later.
--
-- 'rfc4180', 's3Settings_rfc4180' - For an S3 source, when this value is set to @true@ or @y@, each leading
-- double quotation mark has to be followed by an ending double quotation
-- mark. This formatting complies with RFC 4180. When this value is set to
-- @false@ or @n@, string literals are copied to the target as is. In this
-- case, a delimiter (row or column) signals the end of the field. Thus,
-- you can\'t use a delimiter as part of the string, because it signals the
-- end of the value.
--
-- For an S3 target, an optional parameter used to set behavior to comply
-- with RFC 4180 for data migrated to Amazon S3 using .csv file format
-- only. When this value is set to @true@ or @y@ using Amazon S3 as a
-- target, if the data has quotation marks or newline characters in it, DMS
-- encloses the entire column with an additional pair of double quotation
-- marks (\"). Every quotation mark within the data is repeated twice.
--
-- The default value is @true@. Valid values include @true@, @false@, @y@,
-- and @n@.
--
-- 'rowGroupLength', 's3Settings_rowGroupLength' - The number of rows in a row group. A smaller row group size provides
-- faster reads. But as the number of row groups grows, the slower writes
-- become. This parameter defaults to 10,000 rows. This number is used for
-- .parquet file format only.
--
-- If you choose a value larger than the maximum, @RowGroupLength@ is set
-- to the max row group length in bytes (64 * 1024 * 1024).
--
-- 'serverSideEncryptionKmsKeyId', 's3Settings_serverSideEncryptionKmsKeyId' - If you are using @SSE_KMS@ for the @EncryptionMode@, provide the KMS key
-- ID. The key that you use needs an attached policy that enables Identity
-- and Access Management (IAM) user permissions and allows use of the key.
--
-- Here is a CLI example:
-- @aws dms create-endpoint --endpoint-identifier value --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=value,BucketFolder=value,BucketName=value,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=value @
--
-- 'serviceAccessRoleArn', 's3Settings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) used by the service to access the IAM
-- role. The role must allow the @iam:PassRole@ action. It is a required
-- parameter that enables DMS to write and read objects from an S3 bucket.
--
-- 'timestampColumnName', 's3Settings_timestampColumnName' - A value that when nonblank causes DMS to add a column with timestamp
-- information to the endpoint data for an Amazon S3 target.
--
-- DMS supports the @TimestampColumnName@ parameter in versions 3.1.4 and
-- later.
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
-- 'useCsvNoSupValue', 's3Settings_useCsvNoSupValue' - This setting applies if the S3 output files during a change data capture
-- (CDC) load are written in .csv format. If set to @true@ for columns not
-- included in the supplemental log, DMS uses the value specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CsvNoSupValue CsvNoSupValue>
-- . If not set or set to @false@, DMS uses the null value for these
-- columns.
--
-- This setting is supported in DMS versions 3.4.1 and later.
--
-- 'useTaskStartTimeForFullLoadTimestamp', 's3Settings_useTaskStartTimeForFullLoadTimestamp' - When set to true, this parameter uses the task start time as the
-- timestamp column value instead of the time data is written to target.
-- For full load, when @useTaskStartTimeForFullLoadTimestamp@ is set to
-- @true@, each row of the timestamp column contains the task start time.
-- For CDC loads, each row of the timestamp column contains the transaction
-- commit time.
--
-- When @useTaskStartTimeForFullLoadTimestamp@ is set to @false@, the full
-- load timestamp in the timestamp column increments with the time data
-- arrives at the target.
newS3Settings ::
  S3Settings
newS3Settings =
  S3Settings'
    { addColumnName = Prelude.Nothing,
      addTrailingPaddingCharacter = Prelude.Nothing,
      bucketFolder = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      cannedAclForObjects = Prelude.Nothing,
      cdcInsertsAndUpdates = Prelude.Nothing,
      cdcInsertsOnly = Prelude.Nothing,
      cdcMaxBatchInterval = Prelude.Nothing,
      cdcMinFileSize = Prelude.Nothing,
      cdcPath = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      csvDelimiter = Prelude.Nothing,
      csvNoSupValue = Prelude.Nothing,
      csvNullValue = Prelude.Nothing,
      csvRowDelimiter = Prelude.Nothing,
      dataFormat = Prelude.Nothing,
      dataPageSize = Prelude.Nothing,
      datePartitionDelimiter = Prelude.Nothing,
      datePartitionEnabled = Prelude.Nothing,
      datePartitionSequence = Prelude.Nothing,
      datePartitionTimezone = Prelude.Nothing,
      dictPageSizeLimit = Prelude.Nothing,
      enableStatistics = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      externalTableDefinition = Prelude.Nothing,
      ignoreHeaderRows = Prelude.Nothing,
      includeOpForFullLoad = Prelude.Nothing,
      maxFileSize = Prelude.Nothing,
      parquetTimestampInMillisecond = Prelude.Nothing,
      parquetVersion = Prelude.Nothing,
      preserveTransactions = Prelude.Nothing,
      rfc4180 = Prelude.Nothing,
      rowGroupLength = Prelude.Nothing,
      serverSideEncryptionKmsKeyId = Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      timestampColumnName = Prelude.Nothing,
      useCsvNoSupValue = Prelude.Nothing,
      useTaskStartTimeForFullLoadTimestamp =
        Prelude.Nothing
    }

-- | An optional parameter that, when set to @true@ or @y@, you can use to
-- add column name information to the .csv output file.
--
-- The default value is @false@. Valid values are @true@, @false@, @y@, and
-- @n@.
s3Settings_addColumnName :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_addColumnName = Lens.lens (\S3Settings' {addColumnName} -> addColumnName) (\s@S3Settings' {} a -> s {addColumnName = a} :: S3Settings)

-- | Use the S3 target endpoint setting @AddTrailingPaddingCharacter@ to add
-- padding on string data. The default value is @false@.
s3Settings_addTrailingPaddingCharacter :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_addTrailingPaddingCharacter = Lens.lens (\S3Settings' {addTrailingPaddingCharacter} -> addTrailingPaddingCharacter) (\s@S3Settings' {} a -> s {addTrailingPaddingCharacter = a} :: S3Settings)

-- | An optional parameter to set a folder name in the S3 bucket. If
-- provided, tables are created in the path
-- @ bucketFolder\/schema_name\/table_name\/@. If this parameter isn\'t
-- specified, then the path used is @ schema_name\/table_name\/@.
s3Settings_bucketFolder :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_bucketFolder = Lens.lens (\S3Settings' {bucketFolder} -> bucketFolder) (\s@S3Settings' {} a -> s {bucketFolder = a} :: S3Settings)

-- | The name of the S3 bucket.
s3Settings_bucketName :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_bucketName = Lens.lens (\S3Settings' {bucketName} -> bucketName) (\s@S3Settings' {} a -> s {bucketName = a} :: S3Settings)

-- | A value that enables DMS to specify a predefined (canned) access control
-- list for objects created in an Amazon S3 bucket as .csv or .parquet
-- files. For more information about Amazon S3 canned ACLs, see
-- <http://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACL>
-- in the /Amazon S3 Developer Guide./
--
-- The default value is NONE. Valid values include NONE, PRIVATE,
-- PUBLIC_READ, PUBLIC_READ_WRITE, AUTHENTICATED_READ, AWS_EXEC_READ,
-- BUCKET_OWNER_READ, and BUCKET_OWNER_FULL_CONTROL.
s3Settings_cannedAclForObjects :: Lens.Lens' S3Settings (Prelude.Maybe CannedAclForObjectsValue)
s3Settings_cannedAclForObjects = Lens.lens (\S3Settings' {cannedAclForObjects} -> cannedAclForObjects) (\s@S3Settings' {} a -> s {cannedAclForObjects = a} :: S3Settings)

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
-- in the /Database Migration Service User Guide./.
--
-- DMS supports the use of the @CdcInsertsAndUpdates@ parameter in versions
-- 3.3.1 and later.
--
-- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
-- for the same endpoint. Set either @CdcInsertsOnly@ or
-- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
s3Settings_cdcInsertsAndUpdates :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_cdcInsertsAndUpdates = Lens.lens (\S3Settings' {cdcInsertsAndUpdates} -> cdcInsertsAndUpdates) (\s@S3Settings' {} a -> s {cdcInsertsAndUpdates = a} :: S3Settings)

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
-- in the /Database Migration Service User Guide./.
--
-- DMS supports the interaction described preceding between the
-- @CdcInsertsOnly@ and @IncludeOpForFullLoad@ parameters in versions 3.1.4
-- and later.
--
-- @CdcInsertsOnly@ and @CdcInsertsAndUpdates@ can\'t both be set to @true@
-- for the same endpoint. Set either @CdcInsertsOnly@ or
-- @CdcInsertsAndUpdates@ to @true@ for the same endpoint, but not both.
s3Settings_cdcInsertsOnly :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_cdcInsertsOnly = Lens.lens (\S3Settings' {cdcInsertsOnly} -> cdcInsertsOnly) (\s@S3Settings' {} a -> s {cdcInsertsOnly = a} :: S3Settings)

-- | Maximum length of the interval, defined in seconds, after which to
-- output a file to Amazon S3.
--
-- When @CdcMaxBatchInterval@ and @CdcMinFileSize@ are both specified, the
-- file write is triggered by whichever parameter condition is met first
-- within an DMS CloudFormation template.
--
-- The default value is 60 seconds.
s3Settings_cdcMaxBatchInterval :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_cdcMaxBatchInterval = Lens.lens (\S3Settings' {cdcMaxBatchInterval} -> cdcMaxBatchInterval) (\s@S3Settings' {} a -> s {cdcMaxBatchInterval = a} :: S3Settings)

-- | Minimum file size, defined in kilobytes, to reach for a file output to
-- Amazon S3.
--
-- When @CdcMinFileSize@ and @CdcMaxBatchInterval@ are both specified, the
-- file write is triggered by whichever parameter condition is met first
-- within an DMS CloudFormation template.
--
-- The default value is 32 MB.
s3Settings_cdcMinFileSize :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_cdcMinFileSize = Lens.lens (\S3Settings' {cdcMinFileSize} -> cdcMinFileSize) (\s@S3Settings' {} a -> s {cdcMinFileSize = a} :: S3Settings)

-- | Specifies the folder path of CDC files. For an S3 source, this setting
-- is required if a task captures change data; otherwise, it\'s optional.
-- If @CdcPath@ is set, DMS reads CDC files from this path and replicates
-- the data changes to the target endpoint. For an S3 target if you set
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-PreserveTransactions PreserveTransactions>
-- to @true@, DMS verifies that you have set this parameter to a folder
-- path on your S3 target where DMS can save the transaction order for the
-- CDC load. DMS creates this CDC folder path in either your S3 target
-- working directory or the S3 target location specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketFolder BucketFolder>
-- and
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-BucketName BucketName>
-- .
--
-- For example, if you specify @CdcPath@ as @MyChangedData@, and you
-- specify @BucketName@ as @MyTargetBucket@ but do not specify
-- @BucketFolder@, DMS creates the CDC folder path following:
-- @MyTargetBucket\/MyChangedData@.
--
-- If you specify the same @CdcPath@, and you specify @BucketName@ as
-- @MyTargetBucket@ and @BucketFolder@ as @MyTargetData@, DMS creates the
-- CDC folder path following:
-- @MyTargetBucket\/MyTargetData\/MyChangedData@.
--
-- For more information on CDC including transaction order on an S3 target,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
--
-- This setting is supported in DMS versions 3.4.2 and later.
s3Settings_cdcPath :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_cdcPath = Lens.lens (\S3Settings' {cdcPath} -> cdcPath) (\s@S3Settings' {} a -> s {cdcPath = a} :: S3Settings)

-- | An optional parameter to use GZIP to compress the target files. Set to
-- GZIP to compress the target files. Either set this parameter to NONE
-- (the default) or don\'t use it to leave the files uncompressed. This
-- parameter applies to both .csv and .parquet file formats.
s3Settings_compressionType :: Lens.Lens' S3Settings (Prelude.Maybe CompressionTypeValue)
s3Settings_compressionType = Lens.lens (\S3Settings' {compressionType} -> compressionType) (\s@S3Settings' {} a -> s {compressionType = a} :: S3Settings)

-- | The delimiter used to separate columns in the .csv file for both source
-- and target. The default is a comma.
s3Settings_csvDelimiter :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_csvDelimiter = Lens.lens (\S3Settings' {csvDelimiter} -> csvDelimiter) (\s@S3Settings' {} a -> s {csvDelimiter = a} :: S3Settings)

-- | This setting only applies if your Amazon S3 output files during a change
-- data capture (CDC) load are written in .csv format. If
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-UseCsvNoSupValue UseCsvNoSupValue>
-- is set to true, specify a string value that you want DMS to use for all
-- columns not included in the supplemental log. If you do not specify a
-- string value, DMS uses the null value for these columns regardless of
-- the @UseCsvNoSupValue@ setting.
--
-- This setting is supported in DMS versions 3.4.1 and later.
s3Settings_csvNoSupValue :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_csvNoSupValue = Lens.lens (\S3Settings' {csvNoSupValue} -> csvNoSupValue) (\s@S3Settings' {} a -> s {csvNoSupValue = a} :: S3Settings)

-- | An optional parameter that specifies how DMS treats null values. While
-- handling the null value, you can use this parameter to pass a
-- user-defined string as null when writing to the target. For example,
-- when target columns are not nullable, you can use this option to
-- differentiate between the empty string value and the null value. So, if
-- you set this parameter value to the empty string (\"\" or \'\'), DMS
-- treats the empty string as the null value instead of @NULL@.
--
-- The default value is @NULL@. Valid values include any valid string.
s3Settings_csvNullValue :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_csvNullValue = Lens.lens (\S3Settings' {csvNullValue} -> csvNullValue) (\s@S3Settings' {} a -> s {csvNullValue = a} :: S3Settings)

-- | The delimiter used to separate rows in the .csv file for both source and
-- target. The default is a carriage return (@\\n@).
s3Settings_csvRowDelimiter :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_csvRowDelimiter = Lens.lens (\S3Settings' {csvRowDelimiter} -> csvRowDelimiter) (\s@S3Settings' {} a -> s {csvRowDelimiter = a} :: S3Settings)

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

-- | The size of one data page in bytes. This parameter defaults to 1024 *
-- 1024 bytes (1 MiB). This number is used for .parquet file format only.
s3Settings_dataPageSize :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_dataPageSize = Lens.lens (\S3Settings' {dataPageSize} -> dataPageSize) (\s@S3Settings' {} a -> s {dataPageSize = a} :: S3Settings)

-- | Specifies a date separating delimiter to use during folder partitioning.
-- The default value is @SLASH@. Use this parameter when
-- @DatePartitionedEnabled@ is set to @true@.
s3Settings_datePartitionDelimiter :: Lens.Lens' S3Settings (Prelude.Maybe DatePartitionDelimiterValue)
s3Settings_datePartitionDelimiter = Lens.lens (\S3Settings' {datePartitionDelimiter} -> datePartitionDelimiter) (\s@S3Settings' {} a -> s {datePartitionDelimiter = a} :: S3Settings)

-- | When set to @true@, this parameter partitions S3 bucket folders based on
-- transaction commit dates. The default value is @false@. For more
-- information about date-based folder partitioning, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.DatePartitioning Using date-based folder partitioning>.
s3Settings_datePartitionEnabled :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_datePartitionEnabled = Lens.lens (\S3Settings' {datePartitionEnabled} -> datePartitionEnabled) (\s@S3Settings' {} a -> s {datePartitionEnabled = a} :: S3Settings)

-- | Identifies the sequence of the date format to use during folder
-- partitioning. The default value is @YYYYMMDD@. Use this parameter when
-- @DatePartitionedEnabled@ is set to @true@.
s3Settings_datePartitionSequence :: Lens.Lens' S3Settings (Prelude.Maybe DatePartitionSequenceValue)
s3Settings_datePartitionSequence = Lens.lens (\S3Settings' {datePartitionSequence} -> datePartitionSequence) (\s@S3Settings' {} a -> s {datePartitionSequence = a} :: S3Settings)

-- | When creating an S3 target endpoint, set @DatePartitionTimezone@ to
-- convert the current UTC time into a specified time zone. The conversion
-- occurs when a date partition folder is created and a CDC filename is
-- generated. The time zone format is Area\/Location. Use this parameter
-- when @DatePartitionedEnabled@ is set to @true@, as shown in the
-- following example.
--
-- @s3-settings=\'{\"DatePartitionEnabled\": true, \"DatePartitionSequence\": \"YYYYMMDDHH\", \"DatePartitionDelimiter\": \"SLASH\", \"DatePartitionTimezone\":\"Asia\/Seoul\", \"BucketName\": \"dms-nattarat-test\"}\'@
s3Settings_datePartitionTimezone :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_datePartitionTimezone = Lens.lens (\S3Settings' {datePartitionTimezone} -> datePartitionTimezone) (\s@S3Settings' {} a -> s {datePartitionTimezone = a} :: S3Settings)

-- | The maximum size of an encoded dictionary page of a column. If the
-- dictionary page exceeds this, this column is stored using an encoding
-- type of @PLAIN@. This parameter defaults to 1024 * 1024 bytes (1 MiB),
-- the maximum size of a dictionary page before it reverts to @PLAIN@
-- encoding. This size is used for .parquet file format only.
s3Settings_dictPageSizeLimit :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_dictPageSizeLimit = Lens.lens (\S3Settings' {dictPageSizeLimit} -> dictPageSizeLimit) (\s@S3Settings' {} a -> s {dictPageSizeLimit = a} :: S3Settings)

-- | A value that enables statistics for Parquet pages and row groups. Choose
-- @true@ to enable statistics, @false@ to disable. Statistics include
-- @NULL@, @DISTINCT@, @MAX@, and @MIN@ values. This parameter defaults to
-- @true@. This value is used for .parquet file format only.
s3Settings_enableStatistics :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_enableStatistics = Lens.lens (\S3Settings' {enableStatistics} -> enableStatistics) (\s@S3Settings' {} a -> s {enableStatistics = a} :: S3Settings)

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

-- | The type of server-side encryption that you want to use for your data.
-- This encryption type is part of the endpoint settings or the extra
-- connections attributes for Amazon S3. You can choose either @SSE_S3@
-- (the default) or @SSE_KMS@.
--
-- For the @ModifyEndpoint@ operation, you can change the existing value of
-- the @EncryptionMode@ parameter from @SSE_KMS@ to @SSE_S3@. But you can’t
-- change the existing value from @SSE_S3@ to @SSE_KMS@.
--
-- To use @SSE_S3@, you need an Identity and Access Management (IAM) role
-- with permission to allow @\"arn:aws:s3:::dms-*\"@ to use the following
-- actions:
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

-- | To specify a bucket owner and prevent sniping, you can use the
-- @ExpectedBucketOwner@ endpoint setting.
--
-- Example:
-- @--s3-settings=\'{\"ExpectedBucketOwner\": \"AWS_Account_ID\"}\'@
--
-- When you make a request to test a connection or perform a migration, S3
-- checks the account ID of the bucket owner against the specified
-- parameter.
s3Settings_expectedBucketOwner :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_expectedBucketOwner = Lens.lens (\S3Settings' {expectedBucketOwner} -> expectedBucketOwner) (\s@S3Settings' {} a -> s {expectedBucketOwner = a} :: S3Settings)

-- | Specifies how tables are defined in the S3 source files only.
s3Settings_externalTableDefinition :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_externalTableDefinition = Lens.lens (\S3Settings' {externalTableDefinition} -> externalTableDefinition) (\s@S3Settings' {} a -> s {externalTableDefinition = a} :: S3Settings)

-- | When this value is set to 1, DMS ignores the first row header in a .csv
-- file. A value of 1 turns on the feature; a value of 0 turns off the
-- feature.
--
-- The default is 0.
s3Settings_ignoreHeaderRows :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_ignoreHeaderRows = Lens.lens (\S3Settings' {ignoreHeaderRows} -> ignoreHeaderRows) (\s@S3Settings' {} a -> s {ignoreHeaderRows = a} :: S3Settings)

-- | A value that enables a full load to write INSERT operations to the
-- comma-separated value (.csv) output files only to indicate how the rows
-- were added to the source database.
--
-- DMS supports the @IncludeOpForFullLoad@ parameter in versions 3.1.4 and
-- later.
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
-- in the /Database Migration Service User Guide./.
s3Settings_includeOpForFullLoad :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_includeOpForFullLoad = Lens.lens (\S3Settings' {includeOpForFullLoad} -> includeOpForFullLoad) (\s@S3Settings' {} a -> s {includeOpForFullLoad = a} :: S3Settings)

-- | A value that specifies the maximum size (in KB) of any .csv file to be
-- created while migrating to an S3 target during full load.
--
-- The default value is 1,048,576 KB (1 GB). Valid values include 1 to
-- 1,048,576.
s3Settings_maxFileSize :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_maxFileSize = Lens.lens (\S3Settings' {maxFileSize} -> maxFileSize) (\s@S3Settings' {} a -> s {maxFileSize = a} :: S3Settings)

-- | A value that specifies the precision of any @TIMESTAMP@ column values
-- that are written to an Amazon S3 object file in .parquet format.
--
-- DMS supports the @ParquetTimestampInMillisecond@ parameter in versions
-- 3.1.4 and later.
--
-- When @ParquetTimestampInMillisecond@ is set to @true@ or @y@, DMS writes
-- all @TIMESTAMP@ columns in a .parquet formatted file with millisecond
-- precision. Otherwise, DMS writes them with microsecond precision.
--
-- Currently, Amazon Athena and Glue can handle only millisecond precision
-- for @TIMESTAMP@ values. Set this parameter to @true@ for S3 endpoint
-- object files that are .parquet formatted only if you plan to query or
-- process the data with Athena or Glue.
--
-- DMS writes any @TIMESTAMP@ column values written to an S3 file in .csv
-- format with microsecond precision.
--
-- Setting @ParquetTimestampInMillisecond@ has no effect on the string
-- format of the timestamp column value that is inserted by setting the
-- @TimestampColumnName@ parameter.
s3Settings_parquetTimestampInMillisecond :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_parquetTimestampInMillisecond = Lens.lens (\S3Settings' {parquetTimestampInMillisecond} -> parquetTimestampInMillisecond) (\s@S3Settings' {} a -> s {parquetTimestampInMillisecond = a} :: S3Settings)

-- | The version of the Apache Parquet format that you want to use:
-- @parquet_1_0@ (the default) or @parquet_2_0@.
s3Settings_parquetVersion :: Lens.Lens' S3Settings (Prelude.Maybe ParquetVersionValue)
s3Settings_parquetVersion = Lens.lens (\S3Settings' {parquetVersion} -> parquetVersion) (\s@S3Settings' {} a -> s {parquetVersion = a} :: S3Settings)

-- | If set to @true@, DMS saves the transaction order for a change data
-- capture (CDC) load on the Amazon S3 target specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CdcPath CdcPath>
-- . For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.S3.html#CHAP_Target.S3.EndpointSettings.CdcPath Capturing data changes (CDC) including transaction order on the S3 target>.
--
-- This setting is supported in DMS versions 3.4.2 and later.
s3Settings_preserveTransactions :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_preserveTransactions = Lens.lens (\S3Settings' {preserveTransactions} -> preserveTransactions) (\s@S3Settings' {} a -> s {preserveTransactions = a} :: S3Settings)

-- | For an S3 source, when this value is set to @true@ or @y@, each leading
-- double quotation mark has to be followed by an ending double quotation
-- mark. This formatting complies with RFC 4180. When this value is set to
-- @false@ or @n@, string literals are copied to the target as is. In this
-- case, a delimiter (row or column) signals the end of the field. Thus,
-- you can\'t use a delimiter as part of the string, because it signals the
-- end of the value.
--
-- For an S3 target, an optional parameter used to set behavior to comply
-- with RFC 4180 for data migrated to Amazon S3 using .csv file format
-- only. When this value is set to @true@ or @y@ using Amazon S3 as a
-- target, if the data has quotation marks or newline characters in it, DMS
-- encloses the entire column with an additional pair of double quotation
-- marks (\"). Every quotation mark within the data is repeated twice.
--
-- The default value is @true@. Valid values include @true@, @false@, @y@,
-- and @n@.
s3Settings_rfc4180 :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_rfc4180 = Lens.lens (\S3Settings' {rfc4180} -> rfc4180) (\s@S3Settings' {} a -> s {rfc4180 = a} :: S3Settings)

-- | The number of rows in a row group. A smaller row group size provides
-- faster reads. But as the number of row groups grows, the slower writes
-- become. This parameter defaults to 10,000 rows. This number is used for
-- .parquet file format only.
--
-- If you choose a value larger than the maximum, @RowGroupLength@ is set
-- to the max row group length in bytes (64 * 1024 * 1024).
s3Settings_rowGroupLength :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Int)
s3Settings_rowGroupLength = Lens.lens (\S3Settings' {rowGroupLength} -> rowGroupLength) (\s@S3Settings' {} a -> s {rowGroupLength = a} :: S3Settings)

-- | If you are using @SSE_KMS@ for the @EncryptionMode@, provide the KMS key
-- ID. The key that you use needs an attached policy that enables Identity
-- and Access Management (IAM) user permissions and allows use of the key.
--
-- Here is a CLI example:
-- @aws dms create-endpoint --endpoint-identifier value --endpoint-type target --engine-name s3 --s3-settings ServiceAccessRoleArn=value,BucketFolder=value,BucketName=value,EncryptionMode=SSE_KMS,ServerSideEncryptionKmsKeyId=value @
s3Settings_serverSideEncryptionKmsKeyId :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_serverSideEncryptionKmsKeyId = Lens.lens (\S3Settings' {serverSideEncryptionKmsKeyId} -> serverSideEncryptionKmsKeyId) (\s@S3Settings' {} a -> s {serverSideEncryptionKmsKeyId = a} :: S3Settings)

-- | The Amazon Resource Name (ARN) used by the service to access the IAM
-- role. The role must allow the @iam:PassRole@ action. It is a required
-- parameter that enables DMS to write and read objects from an S3 bucket.
s3Settings_serviceAccessRoleArn :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Text)
s3Settings_serviceAccessRoleArn = Lens.lens (\S3Settings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@S3Settings' {} a -> s {serviceAccessRoleArn = a} :: S3Settings)

-- | A value that when nonblank causes DMS to add a column with timestamp
-- information to the endpoint data for an Amazon S3 target.
--
-- DMS supports the @TimestampColumnName@ parameter in versions 3.1.4 and
-- later.
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

-- | This setting applies if the S3 output files during a change data capture
-- (CDC) load are written in .csv format. If set to @true@ for columns not
-- included in the supplemental log, DMS uses the value specified by
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_S3Settings.html#DMS-Type-S3Settings-CsvNoSupValue CsvNoSupValue>
-- . If not set or set to @false@, DMS uses the null value for these
-- columns.
--
-- This setting is supported in DMS versions 3.4.1 and later.
s3Settings_useCsvNoSupValue :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_useCsvNoSupValue = Lens.lens (\S3Settings' {useCsvNoSupValue} -> useCsvNoSupValue) (\s@S3Settings' {} a -> s {useCsvNoSupValue = a} :: S3Settings)

-- | When set to true, this parameter uses the task start time as the
-- timestamp column value instead of the time data is written to target.
-- For full load, when @useTaskStartTimeForFullLoadTimestamp@ is set to
-- @true@, each row of the timestamp column contains the task start time.
-- For CDC loads, each row of the timestamp column contains the transaction
-- commit time.
--
-- When @useTaskStartTimeForFullLoadTimestamp@ is set to @false@, the full
-- load timestamp in the timestamp column increments with the time data
-- arrives at the target.
s3Settings_useTaskStartTimeForFullLoadTimestamp :: Lens.Lens' S3Settings (Prelude.Maybe Prelude.Bool)
s3Settings_useTaskStartTimeForFullLoadTimestamp = Lens.lens (\S3Settings' {useTaskStartTimeForFullLoadTimestamp} -> useTaskStartTimeForFullLoadTimestamp) (\s@S3Settings' {} a -> s {useTaskStartTimeForFullLoadTimestamp = a} :: S3Settings)

instance Data.FromJSON S3Settings where
  parseJSON =
    Data.withObject
      "S3Settings"
      ( \x ->
          S3Settings'
            Prelude.<$> (x Data..:? "AddColumnName")
            Prelude.<*> (x Data..:? "AddTrailingPaddingCharacter")
            Prelude.<*> (x Data..:? "BucketFolder")
            Prelude.<*> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "CannedAclForObjects")
            Prelude.<*> (x Data..:? "CdcInsertsAndUpdates")
            Prelude.<*> (x Data..:? "CdcInsertsOnly")
            Prelude.<*> (x Data..:? "CdcMaxBatchInterval")
            Prelude.<*> (x Data..:? "CdcMinFileSize")
            Prelude.<*> (x Data..:? "CdcPath")
            Prelude.<*> (x Data..:? "CompressionType")
            Prelude.<*> (x Data..:? "CsvDelimiter")
            Prelude.<*> (x Data..:? "CsvNoSupValue")
            Prelude.<*> (x Data..:? "CsvNullValue")
            Prelude.<*> (x Data..:? "CsvRowDelimiter")
            Prelude.<*> (x Data..:? "DataFormat")
            Prelude.<*> (x Data..:? "DataPageSize")
            Prelude.<*> (x Data..:? "DatePartitionDelimiter")
            Prelude.<*> (x Data..:? "DatePartitionEnabled")
            Prelude.<*> (x Data..:? "DatePartitionSequence")
            Prelude.<*> (x Data..:? "DatePartitionTimezone")
            Prelude.<*> (x Data..:? "DictPageSizeLimit")
            Prelude.<*> (x Data..:? "EnableStatistics")
            Prelude.<*> (x Data..:? "EncodingType")
            Prelude.<*> (x Data..:? "EncryptionMode")
            Prelude.<*> (x Data..:? "ExpectedBucketOwner")
            Prelude.<*> (x Data..:? "ExternalTableDefinition")
            Prelude.<*> (x Data..:? "IgnoreHeaderRows")
            Prelude.<*> (x Data..:? "IncludeOpForFullLoad")
            Prelude.<*> (x Data..:? "MaxFileSize")
            Prelude.<*> (x Data..:? "ParquetTimestampInMillisecond")
            Prelude.<*> (x Data..:? "ParquetVersion")
            Prelude.<*> (x Data..:? "PreserveTransactions")
            Prelude.<*> (x Data..:? "Rfc4180")
            Prelude.<*> (x Data..:? "RowGroupLength")
            Prelude.<*> (x Data..:? "ServerSideEncryptionKmsKeyId")
            Prelude.<*> (x Data..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Data..:? "TimestampColumnName")
            Prelude.<*> (x Data..:? "UseCsvNoSupValue")
            Prelude.<*> (x Data..:? "UseTaskStartTimeForFullLoadTimestamp")
      )

instance Prelude.Hashable S3Settings where
  hashWithSalt _salt S3Settings' {..} =
    _salt `Prelude.hashWithSalt` addColumnName
      `Prelude.hashWithSalt` addTrailingPaddingCharacter
      `Prelude.hashWithSalt` bucketFolder
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` cannedAclForObjects
      `Prelude.hashWithSalt` cdcInsertsAndUpdates
      `Prelude.hashWithSalt` cdcInsertsOnly
      `Prelude.hashWithSalt` cdcMaxBatchInterval
      `Prelude.hashWithSalt` cdcMinFileSize
      `Prelude.hashWithSalt` cdcPath
      `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` csvDelimiter
      `Prelude.hashWithSalt` csvNoSupValue
      `Prelude.hashWithSalt` csvNullValue
      `Prelude.hashWithSalt` csvRowDelimiter
      `Prelude.hashWithSalt` dataFormat
      `Prelude.hashWithSalt` dataPageSize
      `Prelude.hashWithSalt` datePartitionDelimiter
      `Prelude.hashWithSalt` datePartitionEnabled
      `Prelude.hashWithSalt` datePartitionSequence
      `Prelude.hashWithSalt` datePartitionTimezone
      `Prelude.hashWithSalt` dictPageSizeLimit
      `Prelude.hashWithSalt` enableStatistics
      `Prelude.hashWithSalt` encodingType
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` externalTableDefinition
      `Prelude.hashWithSalt` ignoreHeaderRows
      `Prelude.hashWithSalt` includeOpForFullLoad
      `Prelude.hashWithSalt` maxFileSize
      `Prelude.hashWithSalt` parquetTimestampInMillisecond
      `Prelude.hashWithSalt` parquetVersion
      `Prelude.hashWithSalt` preserveTransactions
      `Prelude.hashWithSalt` rfc4180
      `Prelude.hashWithSalt` rowGroupLength
      `Prelude.hashWithSalt` serverSideEncryptionKmsKeyId
      `Prelude.hashWithSalt` serviceAccessRoleArn
      `Prelude.hashWithSalt` timestampColumnName
      `Prelude.hashWithSalt` useCsvNoSupValue
      `Prelude.hashWithSalt` useTaskStartTimeForFullLoadTimestamp

instance Prelude.NFData S3Settings where
  rnf S3Settings' {..} =
    Prelude.rnf addColumnName
      `Prelude.seq` Prelude.rnf addTrailingPaddingCharacter
      `Prelude.seq` Prelude.rnf bucketFolder
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf cannedAclForObjects
      `Prelude.seq` Prelude.rnf cdcInsertsAndUpdates
      `Prelude.seq` Prelude.rnf cdcInsertsOnly
      `Prelude.seq` Prelude.rnf cdcMaxBatchInterval
      `Prelude.seq` Prelude.rnf cdcMinFileSize
      `Prelude.seq` Prelude.rnf cdcPath
      `Prelude.seq` Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf csvDelimiter
      `Prelude.seq` Prelude.rnf csvNoSupValue
      `Prelude.seq` Prelude.rnf csvNullValue
      `Prelude.seq` Prelude.rnf csvRowDelimiter
      `Prelude.seq` Prelude.rnf dataFormat
      `Prelude.seq` Prelude.rnf dataPageSize
      `Prelude.seq` Prelude.rnf datePartitionDelimiter
      `Prelude.seq` Prelude.rnf datePartitionEnabled
      `Prelude.seq` Prelude.rnf
        datePartitionSequence
      `Prelude.seq` Prelude.rnf
        datePartitionTimezone
      `Prelude.seq` Prelude.rnf
        dictPageSizeLimit
      `Prelude.seq` Prelude.rnf
        enableStatistics
      `Prelude.seq` Prelude.rnf
        encodingType
      `Prelude.seq` Prelude.rnf
        encryptionMode
      `Prelude.seq` Prelude.rnf
        expectedBucketOwner
      `Prelude.seq` Prelude.rnf
        externalTableDefinition
      `Prelude.seq` Prelude.rnf
        ignoreHeaderRows
      `Prelude.seq` Prelude.rnf
        includeOpForFullLoad
      `Prelude.seq` Prelude.rnf
        maxFileSize
      `Prelude.seq` Prelude.rnf
        parquetTimestampInMillisecond
      `Prelude.seq` Prelude.rnf
        parquetVersion
      `Prelude.seq` Prelude.rnf
        preserveTransactions
      `Prelude.seq` Prelude.rnf
        rfc4180
      `Prelude.seq` Prelude.rnf
        rowGroupLength
      `Prelude.seq` Prelude.rnf
        serverSideEncryptionKmsKeyId
      `Prelude.seq` Prelude.rnf
        serviceAccessRoleArn
      `Prelude.seq` Prelude.rnf
        timestampColumnName
      `Prelude.seq` Prelude.rnf
        useCsvNoSupValue
      `Prelude.seq` Prelude.rnf
        useTaskStartTimeForFullLoadTimestamp

instance Data.ToJSON S3Settings where
  toJSON S3Settings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddColumnName" Data..=) Prelude.<$> addColumnName,
            ("AddTrailingPaddingCharacter" Data..=)
              Prelude.<$> addTrailingPaddingCharacter,
            ("BucketFolder" Data..=) Prelude.<$> bucketFolder,
            ("BucketName" Data..=) Prelude.<$> bucketName,
            ("CannedAclForObjects" Data..=)
              Prelude.<$> cannedAclForObjects,
            ("CdcInsertsAndUpdates" Data..=)
              Prelude.<$> cdcInsertsAndUpdates,
            ("CdcInsertsOnly" Data..=)
              Prelude.<$> cdcInsertsOnly,
            ("CdcMaxBatchInterval" Data..=)
              Prelude.<$> cdcMaxBatchInterval,
            ("CdcMinFileSize" Data..=)
              Prelude.<$> cdcMinFileSize,
            ("CdcPath" Data..=) Prelude.<$> cdcPath,
            ("CompressionType" Data..=)
              Prelude.<$> compressionType,
            ("CsvDelimiter" Data..=) Prelude.<$> csvDelimiter,
            ("CsvNoSupValue" Data..=) Prelude.<$> csvNoSupValue,
            ("CsvNullValue" Data..=) Prelude.<$> csvNullValue,
            ("CsvRowDelimiter" Data..=)
              Prelude.<$> csvRowDelimiter,
            ("DataFormat" Data..=) Prelude.<$> dataFormat,
            ("DataPageSize" Data..=) Prelude.<$> dataPageSize,
            ("DatePartitionDelimiter" Data..=)
              Prelude.<$> datePartitionDelimiter,
            ("DatePartitionEnabled" Data..=)
              Prelude.<$> datePartitionEnabled,
            ("DatePartitionSequence" Data..=)
              Prelude.<$> datePartitionSequence,
            ("DatePartitionTimezone" Data..=)
              Prelude.<$> datePartitionTimezone,
            ("DictPageSizeLimit" Data..=)
              Prelude.<$> dictPageSizeLimit,
            ("EnableStatistics" Data..=)
              Prelude.<$> enableStatistics,
            ("EncodingType" Data..=) Prelude.<$> encodingType,
            ("EncryptionMode" Data..=)
              Prelude.<$> encryptionMode,
            ("ExpectedBucketOwner" Data..=)
              Prelude.<$> expectedBucketOwner,
            ("ExternalTableDefinition" Data..=)
              Prelude.<$> externalTableDefinition,
            ("IgnoreHeaderRows" Data..=)
              Prelude.<$> ignoreHeaderRows,
            ("IncludeOpForFullLoad" Data..=)
              Prelude.<$> includeOpForFullLoad,
            ("MaxFileSize" Data..=) Prelude.<$> maxFileSize,
            ("ParquetTimestampInMillisecond" Data..=)
              Prelude.<$> parquetTimestampInMillisecond,
            ("ParquetVersion" Data..=)
              Prelude.<$> parquetVersion,
            ("PreserveTransactions" Data..=)
              Prelude.<$> preserveTransactions,
            ("Rfc4180" Data..=) Prelude.<$> rfc4180,
            ("RowGroupLength" Data..=)
              Prelude.<$> rowGroupLength,
            ("ServerSideEncryptionKmsKeyId" Data..=)
              Prelude.<$> serverSideEncryptionKmsKeyId,
            ("ServiceAccessRoleArn" Data..=)
              Prelude.<$> serviceAccessRoleArn,
            ("TimestampColumnName" Data..=)
              Prelude.<$> timestampColumnName,
            ("UseCsvNoSupValue" Data..=)
              Prelude.<$> useCsvNoSupValue,
            ("UseTaskStartTimeForFullLoadTimestamp" Data..=)
              Prelude.<$> useTaskStartTimeForFullLoadTimestamp
          ]
      )

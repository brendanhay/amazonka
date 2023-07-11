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
-- Module      : Amazonka.MacieV2.Types.ClassificationResultStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ClassificationResultStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the status of a sensitive data finding.
--
-- /See:/ 'newClassificationResultStatus' smart constructor.
data ClassificationResultStatus = ClassificationResultStatus'
  { -- | The status of the finding. Possible values are:
    --
    -- -   COMPLETE - Amazon Macie successfully completed its analysis of the
    --     S3 object that the finding applies to.
    --
    -- -   PARTIAL - Macie analyzed only a subset of the data in the S3 object
    --     that the finding applies to. For example, the object is an archive
    --     file that contains files in an unsupported format.
    --
    -- -   SKIPPED - Macie wasn\'t able to analyze the S3 object that the
    --     finding applies to. For example, the object is a file that uses an
    --     unsupported format.
    code :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the status of the finding. This value is null if
    -- the status (code) of the finding is COMPLETE.
    --
    -- Amazon Macie uses this value to notify you of any errors, warnings, or
    -- considerations that might impact your analysis of the finding and the
    -- affected S3 object. Possible values are:
    --
    -- -   ARCHIVE_CONTAINS_UNPROCESSED_FILES - The object is an archive file
    --     and Macie extracted and analyzed only some or none of the files in
    --     the archive. To determine which files Macie analyzed, if any, you
    --     can refer to the corresponding sensitive data discovery result for
    --     the finding (ClassificationDetails.detailedResultsLocation).
    --
    -- -   ARCHIVE_EXCEEDS_SIZE_LIMIT - The object is an archive file whose
    --     total storage size exceeds the size quota for this type of archive.
    --
    -- -   ARCHIVE_NESTING_LEVEL_OVER_LIMIT - The object is an archive file
    --     whose nested depth exceeds the quota for the maximum number of
    --     nested levels that Macie analyzes for this type of archive.
    --
    -- -   ARCHIVE_TOTAL_BYTES_EXTRACTED_OVER_LIMIT - The object is an archive
    --     file that exceeds the quota for the maximum amount of data that
    --     Macie extracts and analyzes for this type of archive.
    --
    -- -   ARCHIVE_TOTAL_DOCUMENTS_PROCESSED_OVER_LIMIT - The object is an
    --     archive file that contains more than the maximum number of files
    --     that Macie extracts and analyzes for this type of archive.
    --
    -- -   FILE_EXCEEDS_SIZE_LIMIT - The storage size of the object exceeds the
    --     size quota for this type of file.
    --
    -- -   INVALID_ENCRYPTION - The object is encrypted using server-side
    --     encryption but Macie isn’t allowed to use the key. Macie can’t
    --     decrypt and analyze the object.
    --
    -- -   INVALID_KMS_KEY - The object is encrypted with an KMS key that was
    --     disabled or is being deleted. Macie can’t decrypt and analyze the
    --     object.
    --
    -- -   INVALID_OBJECT_STATE - The object doesn’t use a supported Amazon S3
    --     storage class. For more information, see
    --     <https://docs.aws.amazon.com/macie/latest/user/data-classification.html Discovering sensitive data>
    --     in the /Amazon Macie User Guide/.
    --
    -- -   JSON_NESTING_LEVEL_OVER_LIMIT - The object contains JSON data and
    --     the nested depth of the data exceeds the quota for the number of
    --     nested levels that Macie analyzes for this type of file.
    --
    -- -   MALFORMED_FILE - The object is a malformed or corrupted file. An
    --     error occurred when Macie attempted to detect the file’s type or
    --     extract data from the file.
    --
    -- -   OBJECT_VERSION_MISMATCH - The object was changed while Macie was
    --     analyzing it.
    --
    -- -   NO_SUCH_BUCKET_AVAILABLE - The object was in a bucket that was
    --     deleted shortly before or when Macie attempted to analyze the
    --     object.
    --
    -- -   MALFORMED_OR_FILE_SIZE_EXCEEDS_LIMIT - The object is a Microsoft
    --     Office file that is malformed or exceeds the size quota for this
    --     type of file. If the file is malformed, an error occurred when Macie
    --     attempted to extract data from the file.
    --
    -- -   OOXML_UNCOMPRESSED_SIZE_EXCEEDS_LIMIT - The object is an Office Open
    --     XML file that exceeds the size quota for this type of file.
    --
    -- -   OOXML_UNCOMPRESSED_RATIO_EXCEEDS_LIMIT - The object is an Office
    --     Open XML file whose compression ratio exceeds the compression quota
    --     for this type of file.
    --
    -- -   PERMISSION_DENIED - Macie isn’t allowed to access the object. The
    --     object’s permissions settings prevent Macie from analyzing the
    --     object.
    --
    -- -   SOURCE_OBJECT_NO_LONGER_AVAILABLE - The object was deleted shortly
    --     before or when Macie attempted to analyze it.
    --
    -- -   UNABLE_TO_PARSE_FILE - The object is a file that contains structured
    --     data and an error occurred when Macie attempted to parse the data.
    --
    -- -   UNSUPPORTED_FILE_TYPE_EXCEPTION - The object is a file that uses an
    --     unsupported file or storage format. For more information, see
    --     <https://docs.aws.amazon.com/macie/latest/user/discovery-supported-formats.html Supported file and storage formats>
    --     in the /Amazon Macie User Guide/.
    --
    -- For information about sensitive data discovery quotas for files, see
    -- <https://docs.aws.amazon.com/macie/latest/user/macie-quotas.html Amazon Macie quotas>
    -- in the /Amazon Macie User Guide/.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationResultStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'classificationResultStatus_code' - The status of the finding. Possible values are:
--
-- -   COMPLETE - Amazon Macie successfully completed its analysis of the
--     S3 object that the finding applies to.
--
-- -   PARTIAL - Macie analyzed only a subset of the data in the S3 object
--     that the finding applies to. For example, the object is an archive
--     file that contains files in an unsupported format.
--
-- -   SKIPPED - Macie wasn\'t able to analyze the S3 object that the
--     finding applies to. For example, the object is a file that uses an
--     unsupported format.
--
-- 'reason', 'classificationResultStatus_reason' - A brief description of the status of the finding. This value is null if
-- the status (code) of the finding is COMPLETE.
--
-- Amazon Macie uses this value to notify you of any errors, warnings, or
-- considerations that might impact your analysis of the finding and the
-- affected S3 object. Possible values are:
--
-- -   ARCHIVE_CONTAINS_UNPROCESSED_FILES - The object is an archive file
--     and Macie extracted and analyzed only some or none of the files in
--     the archive. To determine which files Macie analyzed, if any, you
--     can refer to the corresponding sensitive data discovery result for
--     the finding (ClassificationDetails.detailedResultsLocation).
--
-- -   ARCHIVE_EXCEEDS_SIZE_LIMIT - The object is an archive file whose
--     total storage size exceeds the size quota for this type of archive.
--
-- -   ARCHIVE_NESTING_LEVEL_OVER_LIMIT - The object is an archive file
--     whose nested depth exceeds the quota for the maximum number of
--     nested levels that Macie analyzes for this type of archive.
--
-- -   ARCHIVE_TOTAL_BYTES_EXTRACTED_OVER_LIMIT - The object is an archive
--     file that exceeds the quota for the maximum amount of data that
--     Macie extracts and analyzes for this type of archive.
--
-- -   ARCHIVE_TOTAL_DOCUMENTS_PROCESSED_OVER_LIMIT - The object is an
--     archive file that contains more than the maximum number of files
--     that Macie extracts and analyzes for this type of archive.
--
-- -   FILE_EXCEEDS_SIZE_LIMIT - The storage size of the object exceeds the
--     size quota for this type of file.
--
-- -   INVALID_ENCRYPTION - The object is encrypted using server-side
--     encryption but Macie isn’t allowed to use the key. Macie can’t
--     decrypt and analyze the object.
--
-- -   INVALID_KMS_KEY - The object is encrypted with an KMS key that was
--     disabled or is being deleted. Macie can’t decrypt and analyze the
--     object.
--
-- -   INVALID_OBJECT_STATE - The object doesn’t use a supported Amazon S3
--     storage class. For more information, see
--     <https://docs.aws.amazon.com/macie/latest/user/data-classification.html Discovering sensitive data>
--     in the /Amazon Macie User Guide/.
--
-- -   JSON_NESTING_LEVEL_OVER_LIMIT - The object contains JSON data and
--     the nested depth of the data exceeds the quota for the number of
--     nested levels that Macie analyzes for this type of file.
--
-- -   MALFORMED_FILE - The object is a malformed or corrupted file. An
--     error occurred when Macie attempted to detect the file’s type or
--     extract data from the file.
--
-- -   OBJECT_VERSION_MISMATCH - The object was changed while Macie was
--     analyzing it.
--
-- -   NO_SUCH_BUCKET_AVAILABLE - The object was in a bucket that was
--     deleted shortly before or when Macie attempted to analyze the
--     object.
--
-- -   MALFORMED_OR_FILE_SIZE_EXCEEDS_LIMIT - The object is a Microsoft
--     Office file that is malformed or exceeds the size quota for this
--     type of file. If the file is malformed, an error occurred when Macie
--     attempted to extract data from the file.
--
-- -   OOXML_UNCOMPRESSED_SIZE_EXCEEDS_LIMIT - The object is an Office Open
--     XML file that exceeds the size quota for this type of file.
--
-- -   OOXML_UNCOMPRESSED_RATIO_EXCEEDS_LIMIT - The object is an Office
--     Open XML file whose compression ratio exceeds the compression quota
--     for this type of file.
--
-- -   PERMISSION_DENIED - Macie isn’t allowed to access the object. The
--     object’s permissions settings prevent Macie from analyzing the
--     object.
--
-- -   SOURCE_OBJECT_NO_LONGER_AVAILABLE - The object was deleted shortly
--     before or when Macie attempted to analyze it.
--
-- -   UNABLE_TO_PARSE_FILE - The object is a file that contains structured
--     data and an error occurred when Macie attempted to parse the data.
--
-- -   UNSUPPORTED_FILE_TYPE_EXCEPTION - The object is a file that uses an
--     unsupported file or storage format. For more information, see
--     <https://docs.aws.amazon.com/macie/latest/user/discovery-supported-formats.html Supported file and storage formats>
--     in the /Amazon Macie User Guide/.
--
-- For information about sensitive data discovery quotas for files, see
-- <https://docs.aws.amazon.com/macie/latest/user/macie-quotas.html Amazon Macie quotas>
-- in the /Amazon Macie User Guide/.
newClassificationResultStatus ::
  ClassificationResultStatus
newClassificationResultStatus =
  ClassificationResultStatus'
    { code = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The status of the finding. Possible values are:
--
-- -   COMPLETE - Amazon Macie successfully completed its analysis of the
--     S3 object that the finding applies to.
--
-- -   PARTIAL - Macie analyzed only a subset of the data in the S3 object
--     that the finding applies to. For example, the object is an archive
--     file that contains files in an unsupported format.
--
-- -   SKIPPED - Macie wasn\'t able to analyze the S3 object that the
--     finding applies to. For example, the object is a file that uses an
--     unsupported format.
classificationResultStatus_code :: Lens.Lens' ClassificationResultStatus (Prelude.Maybe Prelude.Text)
classificationResultStatus_code = Lens.lens (\ClassificationResultStatus' {code} -> code) (\s@ClassificationResultStatus' {} a -> s {code = a} :: ClassificationResultStatus)

-- | A brief description of the status of the finding. This value is null if
-- the status (code) of the finding is COMPLETE.
--
-- Amazon Macie uses this value to notify you of any errors, warnings, or
-- considerations that might impact your analysis of the finding and the
-- affected S3 object. Possible values are:
--
-- -   ARCHIVE_CONTAINS_UNPROCESSED_FILES - The object is an archive file
--     and Macie extracted and analyzed only some or none of the files in
--     the archive. To determine which files Macie analyzed, if any, you
--     can refer to the corresponding sensitive data discovery result for
--     the finding (ClassificationDetails.detailedResultsLocation).
--
-- -   ARCHIVE_EXCEEDS_SIZE_LIMIT - The object is an archive file whose
--     total storage size exceeds the size quota for this type of archive.
--
-- -   ARCHIVE_NESTING_LEVEL_OVER_LIMIT - The object is an archive file
--     whose nested depth exceeds the quota for the maximum number of
--     nested levels that Macie analyzes for this type of archive.
--
-- -   ARCHIVE_TOTAL_BYTES_EXTRACTED_OVER_LIMIT - The object is an archive
--     file that exceeds the quota for the maximum amount of data that
--     Macie extracts and analyzes for this type of archive.
--
-- -   ARCHIVE_TOTAL_DOCUMENTS_PROCESSED_OVER_LIMIT - The object is an
--     archive file that contains more than the maximum number of files
--     that Macie extracts and analyzes for this type of archive.
--
-- -   FILE_EXCEEDS_SIZE_LIMIT - The storage size of the object exceeds the
--     size quota for this type of file.
--
-- -   INVALID_ENCRYPTION - The object is encrypted using server-side
--     encryption but Macie isn’t allowed to use the key. Macie can’t
--     decrypt and analyze the object.
--
-- -   INVALID_KMS_KEY - The object is encrypted with an KMS key that was
--     disabled or is being deleted. Macie can’t decrypt and analyze the
--     object.
--
-- -   INVALID_OBJECT_STATE - The object doesn’t use a supported Amazon S3
--     storage class. For more information, see
--     <https://docs.aws.amazon.com/macie/latest/user/data-classification.html Discovering sensitive data>
--     in the /Amazon Macie User Guide/.
--
-- -   JSON_NESTING_LEVEL_OVER_LIMIT - The object contains JSON data and
--     the nested depth of the data exceeds the quota for the number of
--     nested levels that Macie analyzes for this type of file.
--
-- -   MALFORMED_FILE - The object is a malformed or corrupted file. An
--     error occurred when Macie attempted to detect the file’s type or
--     extract data from the file.
--
-- -   OBJECT_VERSION_MISMATCH - The object was changed while Macie was
--     analyzing it.
--
-- -   NO_SUCH_BUCKET_AVAILABLE - The object was in a bucket that was
--     deleted shortly before or when Macie attempted to analyze the
--     object.
--
-- -   MALFORMED_OR_FILE_SIZE_EXCEEDS_LIMIT - The object is a Microsoft
--     Office file that is malformed or exceeds the size quota for this
--     type of file. If the file is malformed, an error occurred when Macie
--     attempted to extract data from the file.
--
-- -   OOXML_UNCOMPRESSED_SIZE_EXCEEDS_LIMIT - The object is an Office Open
--     XML file that exceeds the size quota for this type of file.
--
-- -   OOXML_UNCOMPRESSED_RATIO_EXCEEDS_LIMIT - The object is an Office
--     Open XML file whose compression ratio exceeds the compression quota
--     for this type of file.
--
-- -   PERMISSION_DENIED - Macie isn’t allowed to access the object. The
--     object’s permissions settings prevent Macie from analyzing the
--     object.
--
-- -   SOURCE_OBJECT_NO_LONGER_AVAILABLE - The object was deleted shortly
--     before or when Macie attempted to analyze it.
--
-- -   UNABLE_TO_PARSE_FILE - The object is a file that contains structured
--     data and an error occurred when Macie attempted to parse the data.
--
-- -   UNSUPPORTED_FILE_TYPE_EXCEPTION - The object is a file that uses an
--     unsupported file or storage format. For more information, see
--     <https://docs.aws.amazon.com/macie/latest/user/discovery-supported-formats.html Supported file and storage formats>
--     in the /Amazon Macie User Guide/.
--
-- For information about sensitive data discovery quotas for files, see
-- <https://docs.aws.amazon.com/macie/latest/user/macie-quotas.html Amazon Macie quotas>
-- in the /Amazon Macie User Guide/.
classificationResultStatus_reason :: Lens.Lens' ClassificationResultStatus (Prelude.Maybe Prelude.Text)
classificationResultStatus_reason = Lens.lens (\ClassificationResultStatus' {reason} -> reason) (\s@ClassificationResultStatus' {} a -> s {reason = a} :: ClassificationResultStatus)

instance Data.FromJSON ClassificationResultStatus where
  parseJSON =
    Data.withObject
      "ClassificationResultStatus"
      ( \x ->
          ClassificationResultStatus'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "reason")
      )

instance Prelude.Hashable ClassificationResultStatus where
  hashWithSalt _salt ClassificationResultStatus' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` reason

instance Prelude.NFData ClassificationResultStatus where
  rnf ClassificationResultStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf reason

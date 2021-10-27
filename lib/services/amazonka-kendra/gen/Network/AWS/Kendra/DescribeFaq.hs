{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kendra.DescribeFaq
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an FAQ list.
module Network.AWS.Kendra.DescribeFaq
  ( -- * Creating a Request
    DescribeFaq (..),
    newDescribeFaq,

    -- * Request Lenses
    describeFaq_id,
    describeFaq_indexId,

    -- * Destructuring the Response
    DescribeFaqResponse (..),
    newDescribeFaqResponse,

    -- * Response Lenses
    describeFaqResponse_status,
    describeFaqResponse_languageCode,
    describeFaqResponse_createdAt,
    describeFaqResponse_fileFormat,
    describeFaqResponse_name,
    describeFaqResponse_id,
    describeFaqResponse_s3Path,
    describeFaqResponse_updatedAt,
    describeFaqResponse_errorMessage,
    describeFaqResponse_indexId,
    describeFaqResponse_description,
    describeFaqResponse_roleArn,
    describeFaqResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeFaq' smart constructor.
data DescribeFaq = DescribeFaq'
  { -- | The unique identifier of the FAQ.
    id :: Prelude.Text,
    -- | The identifier of the index that contains the FAQ.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFaq' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeFaq_id' - The unique identifier of the FAQ.
--
-- 'indexId', 'describeFaq_indexId' - The identifier of the index that contains the FAQ.
newDescribeFaq ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  DescribeFaq
newDescribeFaq pId_ pIndexId_ =
  DescribeFaq' {id = pId_, indexId = pIndexId_}

-- | The unique identifier of the FAQ.
describeFaq_id :: Lens.Lens' DescribeFaq Prelude.Text
describeFaq_id = Lens.lens (\DescribeFaq' {id} -> id) (\s@DescribeFaq' {} a -> s {id = a} :: DescribeFaq)

-- | The identifier of the index that contains the FAQ.
describeFaq_indexId :: Lens.Lens' DescribeFaq Prelude.Text
describeFaq_indexId = Lens.lens (\DescribeFaq' {indexId} -> indexId) (\s@DescribeFaq' {} a -> s {indexId = a} :: DescribeFaq)

instance Core.AWSRequest DescribeFaq where
  type AWSResponse DescribeFaq = DescribeFaqResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFaqResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "FileFormat")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "S3Path")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (x Core..?> "ErrorMessage")
            Prelude.<*> (x Core..?> "IndexId")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "RoleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFaq

instance Prelude.NFData DescribeFaq

instance Core.ToHeaders DescribeFaq where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DescribeFaq" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFaq where
  toJSON DescribeFaq' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Id" Core..= id),
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath DescribeFaq where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeFaq where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFaqResponse' smart constructor.
data DescribeFaqResponse = DescribeFaqResponse'
  { -- | The status of the FAQ. It is ready to use when the status is @ACTIVE@.
    status :: Prelude.Maybe FaqStatus,
    -- | The code for a language. This shows a supported language for the FAQ
    -- document. English is supported by default. For more information on
    -- supported languages, including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the FAQ was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The file format used by the input files for the FAQ.
    fileFormat :: Prelude.Maybe FaqFileFormat,
    -- | The name that you gave the FAQ when it was created.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the FAQ.
    id :: Prelude.Maybe Prelude.Text,
    s3Path :: Prelude.Maybe S3Path,
    -- | The date and time that the FAQ was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | If the @Status@ field is @FAILED@, the @ErrorMessage@ field contains the
    -- reason why the FAQ failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index that contains the FAQ.
    indexId :: Prelude.Maybe Prelude.Text,
    -- | The description of the FAQ that you provided when it was created.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role that provides access to the
    -- S3 bucket containing the input files for the FAQ.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFaqResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeFaqResponse_status' - The status of the FAQ. It is ready to use when the status is @ACTIVE@.
--
-- 'languageCode', 'describeFaqResponse_languageCode' - The code for a language. This shows a supported language for the FAQ
-- document. English is supported by default. For more information on
-- supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'createdAt', 'describeFaqResponse_createdAt' - The date and time that the FAQ was created.
--
-- 'fileFormat', 'describeFaqResponse_fileFormat' - The file format used by the input files for the FAQ.
--
-- 'name', 'describeFaqResponse_name' - The name that you gave the FAQ when it was created.
--
-- 'id', 'describeFaqResponse_id' - The identifier of the FAQ.
--
-- 's3Path', 'describeFaqResponse_s3Path' - Undocumented member.
--
-- 'updatedAt', 'describeFaqResponse_updatedAt' - The date and time that the FAQ was last updated.
--
-- 'errorMessage', 'describeFaqResponse_errorMessage' - If the @Status@ field is @FAILED@, the @ErrorMessage@ field contains the
-- reason why the FAQ failed.
--
-- 'indexId', 'describeFaqResponse_indexId' - The identifier of the index that contains the FAQ.
--
-- 'description', 'describeFaqResponse_description' - The description of the FAQ that you provided when it was created.
--
-- 'roleArn', 'describeFaqResponse_roleArn' - The Amazon Resource Name (ARN) of the role that provides access to the
-- S3 bucket containing the input files for the FAQ.
--
-- 'httpStatus', 'describeFaqResponse_httpStatus' - The response's http status code.
newDescribeFaqResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFaqResponse
newDescribeFaqResponse pHttpStatus_ =
  DescribeFaqResponse'
    { status = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      s3Path = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      indexId = Prelude.Nothing,
      description = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the FAQ. It is ready to use when the status is @ACTIVE@.
describeFaqResponse_status :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe FaqStatus)
describeFaqResponse_status = Lens.lens (\DescribeFaqResponse' {status} -> status) (\s@DescribeFaqResponse' {} a -> s {status = a} :: DescribeFaqResponse)

-- | The code for a language. This shows a supported language for the FAQ
-- document. English is supported by default. For more information on
-- supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
describeFaqResponse_languageCode :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.Text)
describeFaqResponse_languageCode = Lens.lens (\DescribeFaqResponse' {languageCode} -> languageCode) (\s@DescribeFaqResponse' {} a -> s {languageCode = a} :: DescribeFaqResponse)

-- | The date and time that the FAQ was created.
describeFaqResponse_createdAt :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.UTCTime)
describeFaqResponse_createdAt = Lens.lens (\DescribeFaqResponse' {createdAt} -> createdAt) (\s@DescribeFaqResponse' {} a -> s {createdAt = a} :: DescribeFaqResponse) Prelude.. Lens.mapping Core._Time

-- | The file format used by the input files for the FAQ.
describeFaqResponse_fileFormat :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe FaqFileFormat)
describeFaqResponse_fileFormat = Lens.lens (\DescribeFaqResponse' {fileFormat} -> fileFormat) (\s@DescribeFaqResponse' {} a -> s {fileFormat = a} :: DescribeFaqResponse)

-- | The name that you gave the FAQ when it was created.
describeFaqResponse_name :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.Text)
describeFaqResponse_name = Lens.lens (\DescribeFaqResponse' {name} -> name) (\s@DescribeFaqResponse' {} a -> s {name = a} :: DescribeFaqResponse)

-- | The identifier of the FAQ.
describeFaqResponse_id :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.Text)
describeFaqResponse_id = Lens.lens (\DescribeFaqResponse' {id} -> id) (\s@DescribeFaqResponse' {} a -> s {id = a} :: DescribeFaqResponse)

-- | Undocumented member.
describeFaqResponse_s3Path :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe S3Path)
describeFaqResponse_s3Path = Lens.lens (\DescribeFaqResponse' {s3Path} -> s3Path) (\s@DescribeFaqResponse' {} a -> s {s3Path = a} :: DescribeFaqResponse)

-- | The date and time that the FAQ was last updated.
describeFaqResponse_updatedAt :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.UTCTime)
describeFaqResponse_updatedAt = Lens.lens (\DescribeFaqResponse' {updatedAt} -> updatedAt) (\s@DescribeFaqResponse' {} a -> s {updatedAt = a} :: DescribeFaqResponse) Prelude.. Lens.mapping Core._Time

-- | If the @Status@ field is @FAILED@, the @ErrorMessage@ field contains the
-- reason why the FAQ failed.
describeFaqResponse_errorMessage :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.Text)
describeFaqResponse_errorMessage = Lens.lens (\DescribeFaqResponse' {errorMessage} -> errorMessage) (\s@DescribeFaqResponse' {} a -> s {errorMessage = a} :: DescribeFaqResponse)

-- | The identifier of the index that contains the FAQ.
describeFaqResponse_indexId :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.Text)
describeFaqResponse_indexId = Lens.lens (\DescribeFaqResponse' {indexId} -> indexId) (\s@DescribeFaqResponse' {} a -> s {indexId = a} :: DescribeFaqResponse)

-- | The description of the FAQ that you provided when it was created.
describeFaqResponse_description :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.Text)
describeFaqResponse_description = Lens.lens (\DescribeFaqResponse' {description} -> description) (\s@DescribeFaqResponse' {} a -> s {description = a} :: DescribeFaqResponse)

-- | The Amazon Resource Name (ARN) of the role that provides access to the
-- S3 bucket containing the input files for the FAQ.
describeFaqResponse_roleArn :: Lens.Lens' DescribeFaqResponse (Prelude.Maybe Prelude.Text)
describeFaqResponse_roleArn = Lens.lens (\DescribeFaqResponse' {roleArn} -> roleArn) (\s@DescribeFaqResponse' {} a -> s {roleArn = a} :: DescribeFaqResponse)

-- | The response's http status code.
describeFaqResponse_httpStatus :: Lens.Lens' DescribeFaqResponse Prelude.Int
describeFaqResponse_httpStatus = Lens.lens (\DescribeFaqResponse' {httpStatus} -> httpStatus) (\s@DescribeFaqResponse' {} a -> s {httpStatus = a} :: DescribeFaqResponse)

instance Prelude.NFData DescribeFaqResponse

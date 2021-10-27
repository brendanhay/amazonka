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
-- Module      : Network.AWS.Kendra.CreateFaq
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an new set of frequently asked question (FAQ) questions and
-- answers.
module Network.AWS.Kendra.CreateFaq
  ( -- * Creating a Request
    CreateFaq (..),
    newCreateFaq,

    -- * Request Lenses
    createFaq_languageCode,
    createFaq_clientToken,
    createFaq_fileFormat,
    createFaq_description,
    createFaq_tags,
    createFaq_indexId,
    createFaq_name,
    createFaq_s3Path,
    createFaq_roleArn,

    -- * Destructuring the Response
    CreateFaqResponse (..),
    newCreateFaqResponse,

    -- * Response Lenses
    createFaqResponse_id,
    createFaqResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFaq' smart constructor.
data CreateFaq = CreateFaq'
  { -- | The code for a language. This allows you to support a language for the
    -- FAQ document. English is supported by default. For more information on
    -- supported languages, including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | A token that you provide to identify the request to create a FAQ.
    -- Multiple calls to the @CreateFaqRequest@ operation with the same client
    -- token will create only one FAQ.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The format of the input file. You can choose between a basic CSV format,
    -- a CSV format that includes customs attributes in a header, and a JSON
    -- format that includes custom attributes.
    --
    -- The format must match the format of the file stored in the S3 bucket
    -- identified in the @S3Path@ parameter.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-creating-faq.html Adding questions and answers>.
    fileFormat :: Prelude.Maybe FaqFileFormat,
    -- | A description of the FAQ.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that identify the FAQ. You can use the tags to
    -- identify and organize your resources and to control access to resources.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the index that contains the FAQ.
    indexId :: Prelude.Text,
    -- | The name that should be associated with the FAQ.
    name :: Prelude.Text,
    -- | The S3 location of the FAQ input data.
    s3Path :: S3Path,
    -- | The Amazon Resource Name (ARN) of a role with permission to access the
    -- S3 bucket that contains the FAQs. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFaq' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'createFaq_languageCode' - The code for a language. This allows you to support a language for the
-- FAQ document. English is supported by default. For more information on
-- supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'clientToken', 'createFaq_clientToken' - A token that you provide to identify the request to create a FAQ.
-- Multiple calls to the @CreateFaqRequest@ operation with the same client
-- token will create only one FAQ.
--
-- 'fileFormat', 'createFaq_fileFormat' - The format of the input file. You can choose between a basic CSV format,
-- a CSV format that includes customs attributes in a header, and a JSON
-- format that includes custom attributes.
--
-- The format must match the format of the file stored in the S3 bucket
-- identified in the @S3Path@ parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-creating-faq.html Adding questions and answers>.
--
-- 'description', 'createFaq_description' - A description of the FAQ.
--
-- 'tags', 'createFaq_tags' - A list of key-value pairs that identify the FAQ. You can use the tags to
-- identify and organize your resources and to control access to resources.
--
-- 'indexId', 'createFaq_indexId' - The identifier of the index that contains the FAQ.
--
-- 'name', 'createFaq_name' - The name that should be associated with the FAQ.
--
-- 's3Path', 'createFaq_s3Path' - The S3 location of the FAQ input data.
--
-- 'roleArn', 'createFaq_roleArn' - The Amazon Resource Name (ARN) of a role with permission to access the
-- S3 bucket that contains the FAQs. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
newCreateFaq ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 's3Path'
  S3Path ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateFaq
newCreateFaq pIndexId_ pName_ pS3Path_ pRoleArn_ =
  CreateFaq'
    { languageCode = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      indexId = pIndexId_,
      name = pName_,
      s3Path = pS3Path_,
      roleArn = pRoleArn_
    }

-- | The code for a language. This allows you to support a language for the
-- FAQ document. English is supported by default. For more information on
-- supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
createFaq_languageCode :: Lens.Lens' CreateFaq (Prelude.Maybe Prelude.Text)
createFaq_languageCode = Lens.lens (\CreateFaq' {languageCode} -> languageCode) (\s@CreateFaq' {} a -> s {languageCode = a} :: CreateFaq)

-- | A token that you provide to identify the request to create a FAQ.
-- Multiple calls to the @CreateFaqRequest@ operation with the same client
-- token will create only one FAQ.
createFaq_clientToken :: Lens.Lens' CreateFaq (Prelude.Maybe Prelude.Text)
createFaq_clientToken = Lens.lens (\CreateFaq' {clientToken} -> clientToken) (\s@CreateFaq' {} a -> s {clientToken = a} :: CreateFaq)

-- | The format of the input file. You can choose between a basic CSV format,
-- a CSV format that includes customs attributes in a header, and a JSON
-- format that includes custom attributes.
--
-- The format must match the format of the file stored in the S3 bucket
-- identified in the @S3Path@ parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-creating-faq.html Adding questions and answers>.
createFaq_fileFormat :: Lens.Lens' CreateFaq (Prelude.Maybe FaqFileFormat)
createFaq_fileFormat = Lens.lens (\CreateFaq' {fileFormat} -> fileFormat) (\s@CreateFaq' {} a -> s {fileFormat = a} :: CreateFaq)

-- | A description of the FAQ.
createFaq_description :: Lens.Lens' CreateFaq (Prelude.Maybe Prelude.Text)
createFaq_description = Lens.lens (\CreateFaq' {description} -> description) (\s@CreateFaq' {} a -> s {description = a} :: CreateFaq)

-- | A list of key-value pairs that identify the FAQ. You can use the tags to
-- identify and organize your resources and to control access to resources.
createFaq_tags :: Lens.Lens' CreateFaq (Prelude.Maybe [Tag])
createFaq_tags = Lens.lens (\CreateFaq' {tags} -> tags) (\s@CreateFaq' {} a -> s {tags = a} :: CreateFaq) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the index that contains the FAQ.
createFaq_indexId :: Lens.Lens' CreateFaq Prelude.Text
createFaq_indexId = Lens.lens (\CreateFaq' {indexId} -> indexId) (\s@CreateFaq' {} a -> s {indexId = a} :: CreateFaq)

-- | The name that should be associated with the FAQ.
createFaq_name :: Lens.Lens' CreateFaq Prelude.Text
createFaq_name = Lens.lens (\CreateFaq' {name} -> name) (\s@CreateFaq' {} a -> s {name = a} :: CreateFaq)

-- | The S3 location of the FAQ input data.
createFaq_s3Path :: Lens.Lens' CreateFaq S3Path
createFaq_s3Path = Lens.lens (\CreateFaq' {s3Path} -> s3Path) (\s@CreateFaq' {} a -> s {s3Path = a} :: CreateFaq)

-- | The Amazon Resource Name (ARN) of a role with permission to access the
-- S3 bucket that contains the FAQs. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
createFaq_roleArn :: Lens.Lens' CreateFaq Prelude.Text
createFaq_roleArn = Lens.lens (\CreateFaq' {roleArn} -> roleArn) (\s@CreateFaq' {} a -> s {roleArn = a} :: CreateFaq)

instance Core.AWSRequest CreateFaq where
  type AWSResponse CreateFaq = CreateFaqResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFaqResponse'
            Prelude.<$> (x Core..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFaq

instance Prelude.NFData CreateFaq

instance Core.ToHeaders CreateFaq where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.CreateFaq" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFaq where
  toJSON CreateFaq' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LanguageCode" Core..=) Prelude.<$> languageCode,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("FileFormat" Core..=) Prelude.<$> fileFormat,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("S3Path" Core..= s3Path),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateFaq where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateFaq where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFaqResponse' smart constructor.
data CreateFaqResponse = CreateFaqResponse'
  { -- | The unique identifier of the FAQ.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFaqResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createFaqResponse_id' - The unique identifier of the FAQ.
--
-- 'httpStatus', 'createFaqResponse_httpStatus' - The response's http status code.
newCreateFaqResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFaqResponse
newCreateFaqResponse pHttpStatus_ =
  CreateFaqResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the FAQ.
createFaqResponse_id :: Lens.Lens' CreateFaqResponse (Prelude.Maybe Prelude.Text)
createFaqResponse_id = Lens.lens (\CreateFaqResponse' {id} -> id) (\s@CreateFaqResponse' {} a -> s {id = a} :: CreateFaqResponse)

-- | The response's http status code.
createFaqResponse_httpStatus :: Lens.Lens' CreateFaqResponse Prelude.Int
createFaqResponse_httpStatus = Lens.lens (\CreateFaqResponse' {httpStatus} -> httpStatus) (\s@CreateFaqResponse' {} a -> s {httpStatus = a} :: CreateFaqResponse)

instance Prelude.NFData CreateFaqResponse

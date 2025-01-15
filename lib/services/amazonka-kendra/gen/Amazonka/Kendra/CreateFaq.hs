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
-- Module      : Amazonka.Kendra.CreateFaq
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an new set of frequently asked question (FAQ) questions and
-- answers.
--
-- Adding FAQs to an index is an asynchronous operation.
--
-- For an example of adding an FAQ to an index using Python and Java SDKs,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-creating-faq.html#using-faq-file Using your FAQ file>.
module Amazonka.Kendra.CreateFaq
  ( -- * Creating a Request
    CreateFaq (..),
    newCreateFaq,

    -- * Request Lenses
    createFaq_clientToken,
    createFaq_description,
    createFaq_fileFormat,
    createFaq_languageCode,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFaq' smart constructor.
data CreateFaq = CreateFaq'
  { -- | A token that you provide to identify the request to create a FAQ.
    -- Multiple calls to the @CreateFaqRequest@ API with the same client token
    -- will create only one FAQ.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the FAQ.
    description :: Prelude.Maybe Prelude.Text,
    -- | The format of the FAQ input file. You can choose between a basic CSV
    -- format, a CSV format that includes customs attributes in a header, and a
    -- JSON format that includes custom attributes.
    --
    -- The format must match the format of the file stored in the S3 bucket
    -- identified in the @S3Path@ parameter.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-creating-faq.html Adding questions and answers>.
    fileFormat :: Prelude.Maybe FaqFileFormat,
    -- | The code for a language. This allows you to support a language for the
    -- FAQ document. English is supported by default. For more information on
    -- supported languages, including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that identify the FAQ. You can use the tags to
    -- identify and organize your resources and to control access to resources.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the index for the FAQ.
    indexId :: Prelude.Text,
    -- | A name for the FAQ.
    name :: Prelude.Text,
    -- | The path to the FAQ file in S3.
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
-- 'clientToken', 'createFaq_clientToken' - A token that you provide to identify the request to create a FAQ.
-- Multiple calls to the @CreateFaqRequest@ API with the same client token
-- will create only one FAQ.
--
-- 'description', 'createFaq_description' - A description for the FAQ.
--
-- 'fileFormat', 'createFaq_fileFormat' - The format of the FAQ input file. You can choose between a basic CSV
-- format, a CSV format that includes customs attributes in a header, and a
-- JSON format that includes custom attributes.
--
-- The format must match the format of the file stored in the S3 bucket
-- identified in the @S3Path@ parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-creating-faq.html Adding questions and answers>.
--
-- 'languageCode', 'createFaq_languageCode' - The code for a language. This allows you to support a language for the
-- FAQ document. English is supported by default. For more information on
-- supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'tags', 'createFaq_tags' - A list of key-value pairs that identify the FAQ. You can use the tags to
-- identify and organize your resources and to control access to resources.
--
-- 'indexId', 'createFaq_indexId' - The identifier of the index for the FAQ.
--
-- 'name', 'createFaq_name' - A name for the FAQ.
--
-- 's3Path', 'createFaq_s3Path' - The path to the FAQ file in S3.
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
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      tags = Prelude.Nothing,
      indexId = pIndexId_,
      name = pName_,
      s3Path = pS3Path_,
      roleArn = pRoleArn_
    }

-- | A token that you provide to identify the request to create a FAQ.
-- Multiple calls to the @CreateFaqRequest@ API with the same client token
-- will create only one FAQ.
createFaq_clientToken :: Lens.Lens' CreateFaq (Prelude.Maybe Prelude.Text)
createFaq_clientToken = Lens.lens (\CreateFaq' {clientToken} -> clientToken) (\s@CreateFaq' {} a -> s {clientToken = a} :: CreateFaq)

-- | A description for the FAQ.
createFaq_description :: Lens.Lens' CreateFaq (Prelude.Maybe Prelude.Text)
createFaq_description = Lens.lens (\CreateFaq' {description} -> description) (\s@CreateFaq' {} a -> s {description = a} :: CreateFaq)

-- | The format of the FAQ input file. You can choose between a basic CSV
-- format, a CSV format that includes customs attributes in a header, and a
-- JSON format that includes custom attributes.
--
-- The format must match the format of the file stored in the S3 bucket
-- identified in the @S3Path@ parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-creating-faq.html Adding questions and answers>.
createFaq_fileFormat :: Lens.Lens' CreateFaq (Prelude.Maybe FaqFileFormat)
createFaq_fileFormat = Lens.lens (\CreateFaq' {fileFormat} -> fileFormat) (\s@CreateFaq' {} a -> s {fileFormat = a} :: CreateFaq)

-- | The code for a language. This allows you to support a language for the
-- FAQ document. English is supported by default. For more information on
-- supported languages, including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
createFaq_languageCode :: Lens.Lens' CreateFaq (Prelude.Maybe Prelude.Text)
createFaq_languageCode = Lens.lens (\CreateFaq' {languageCode} -> languageCode) (\s@CreateFaq' {} a -> s {languageCode = a} :: CreateFaq)

-- | A list of key-value pairs that identify the FAQ. You can use the tags to
-- identify and organize your resources and to control access to resources.
createFaq_tags :: Lens.Lens' CreateFaq (Prelude.Maybe [Tag])
createFaq_tags = Lens.lens (\CreateFaq' {tags} -> tags) (\s@CreateFaq' {} a -> s {tags = a} :: CreateFaq) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the index for the FAQ.
createFaq_indexId :: Lens.Lens' CreateFaq Prelude.Text
createFaq_indexId = Lens.lens (\CreateFaq' {indexId} -> indexId) (\s@CreateFaq' {} a -> s {indexId = a} :: CreateFaq)

-- | A name for the FAQ.
createFaq_name :: Lens.Lens' CreateFaq Prelude.Text
createFaq_name = Lens.lens (\CreateFaq' {name} -> name) (\s@CreateFaq' {} a -> s {name = a} :: CreateFaq)

-- | The path to the FAQ file in S3.
createFaq_s3Path :: Lens.Lens' CreateFaq S3Path
createFaq_s3Path = Lens.lens (\CreateFaq' {s3Path} -> s3Path) (\s@CreateFaq' {} a -> s {s3Path = a} :: CreateFaq)

-- | The Amazon Resource Name (ARN) of a role with permission to access the
-- S3 bucket that contains the FAQs. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM Roles for Amazon Kendra>.
createFaq_roleArn :: Lens.Lens' CreateFaq Prelude.Text
createFaq_roleArn = Lens.lens (\CreateFaq' {roleArn} -> roleArn) (\s@CreateFaq' {} a -> s {roleArn = a} :: CreateFaq)

instance Core.AWSRequest CreateFaq where
  type AWSResponse CreateFaq = CreateFaqResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFaqResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFaq where
  hashWithSalt _salt CreateFaq' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fileFormat
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` s3Path
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateFaq where
  rnf CreateFaq' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf fileFormat `Prelude.seq`
          Prelude.rnf languageCode `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf indexId `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf s3Path `Prelude.seq`
                    Prelude.rnf roleArn

instance Data.ToHeaders CreateFaq where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.CreateFaq" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFaq where
  toJSON CreateFaq' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("FileFormat" Data..=) Prelude.<$> fileFormat,
            ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("S3Path" Data..= s3Path),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateFaq where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFaq where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFaqResponse' smart constructor.
data CreateFaqResponse = CreateFaqResponse'
  { -- | The identifier of the FAQ.
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
-- 'id', 'createFaqResponse_id' - The identifier of the FAQ.
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

-- | The identifier of the FAQ.
createFaqResponse_id :: Lens.Lens' CreateFaqResponse (Prelude.Maybe Prelude.Text)
createFaqResponse_id = Lens.lens (\CreateFaqResponse' {id} -> id) (\s@CreateFaqResponse' {} a -> s {id = a} :: CreateFaqResponse)

-- | The response's http status code.
createFaqResponse_httpStatus :: Lens.Lens' CreateFaqResponse Prelude.Int
createFaqResponse_httpStatus = Lens.lens (\CreateFaqResponse' {httpStatus} -> httpStatus) (\s@CreateFaqResponse' {} a -> s {httpStatus = a} :: CreateFaqResponse)

instance Prelude.NFData CreateFaqResponse where
  rnf CreateFaqResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus

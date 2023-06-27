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
-- Module      : Amazonka.Kendra.CreateThesaurus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a thesaurus for an index. The thesaurus contains a list of
-- synonyms in Solr format.
--
-- For an example of adding a thesaurus file to an index, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/index-synonyms-adding-thesaurus-file.html Adding custom synonyms to an index>.
module Amazonka.Kendra.CreateThesaurus
  ( -- * Creating a Request
    CreateThesaurus (..),
    newCreateThesaurus,

    -- * Request Lenses
    createThesaurus_clientToken,
    createThesaurus_description,
    createThesaurus_tags,
    createThesaurus_indexId,
    createThesaurus_name,
    createThesaurus_roleArn,
    createThesaurus_sourceS3Path,

    -- * Destructuring the Response
    CreateThesaurusResponse (..),
    newCreateThesaurusResponse,

    -- * Response Lenses
    createThesaurusResponse_id,
    createThesaurusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateThesaurus' smart constructor.
data CreateThesaurus = CreateThesaurus'
  { -- | A token that you provide to identify the request to create a thesaurus.
    -- Multiple calls to the @CreateThesaurus@ API with the same client token
    -- will create only one thesaurus.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the thesaurus.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that identify or categorize the thesaurus. You
    -- can also use tags to help control access to the thesaurus. Tag keys and
    -- values can consist of Unicode letters, digits, white space, and any of
    -- the following symbols: _ . : \/ = + - \@.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the index for the thesaurus.
    indexId :: Prelude.Text,
    -- | A name for the thesaurus.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role with permission to access
    -- your S3 bucket that contains the thesaurus file. For more information,
    -- see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM access roles for Amazon Kendra>.
    roleArn :: Prelude.Text,
    -- | The path to the thesaurus file in S3.
    sourceS3Path :: S3Path
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThesaurus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createThesaurus_clientToken' - A token that you provide to identify the request to create a thesaurus.
-- Multiple calls to the @CreateThesaurus@ API with the same client token
-- will create only one thesaurus.
--
-- 'description', 'createThesaurus_description' - A description for the thesaurus.
--
-- 'tags', 'createThesaurus_tags' - A list of key-value pairs that identify or categorize the thesaurus. You
-- can also use tags to help control access to the thesaurus. Tag keys and
-- values can consist of Unicode letters, digits, white space, and any of
-- the following symbols: _ . : \/ = + - \@.
--
-- 'indexId', 'createThesaurus_indexId' - The identifier of the index for the thesaurus.
--
-- 'name', 'createThesaurus_name' - A name for the thesaurus.
--
-- 'roleArn', 'createThesaurus_roleArn' - The Amazon Resource Name (ARN) of an IAM role with permission to access
-- your S3 bucket that contains the thesaurus file. For more information,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM access roles for Amazon Kendra>.
--
-- 'sourceS3Path', 'createThesaurus_sourceS3Path' - The path to the thesaurus file in S3.
newCreateThesaurus ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'sourceS3Path'
  S3Path ->
  CreateThesaurus
newCreateThesaurus
  pIndexId_
  pName_
  pRoleArn_
  pSourceS3Path_ =
    CreateThesaurus'
      { clientToken = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        indexId = pIndexId_,
        name = pName_,
        roleArn = pRoleArn_,
        sourceS3Path = pSourceS3Path_
      }

-- | A token that you provide to identify the request to create a thesaurus.
-- Multiple calls to the @CreateThesaurus@ API with the same client token
-- will create only one thesaurus.
createThesaurus_clientToken :: Lens.Lens' CreateThesaurus (Prelude.Maybe Prelude.Text)
createThesaurus_clientToken = Lens.lens (\CreateThesaurus' {clientToken} -> clientToken) (\s@CreateThesaurus' {} a -> s {clientToken = a} :: CreateThesaurus)

-- | A description for the thesaurus.
createThesaurus_description :: Lens.Lens' CreateThesaurus (Prelude.Maybe Prelude.Text)
createThesaurus_description = Lens.lens (\CreateThesaurus' {description} -> description) (\s@CreateThesaurus' {} a -> s {description = a} :: CreateThesaurus)

-- | A list of key-value pairs that identify or categorize the thesaurus. You
-- can also use tags to help control access to the thesaurus. Tag keys and
-- values can consist of Unicode letters, digits, white space, and any of
-- the following symbols: _ . : \/ = + - \@.
createThesaurus_tags :: Lens.Lens' CreateThesaurus (Prelude.Maybe [Tag])
createThesaurus_tags = Lens.lens (\CreateThesaurus' {tags} -> tags) (\s@CreateThesaurus' {} a -> s {tags = a} :: CreateThesaurus) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the index for the thesaurus.
createThesaurus_indexId :: Lens.Lens' CreateThesaurus Prelude.Text
createThesaurus_indexId = Lens.lens (\CreateThesaurus' {indexId} -> indexId) (\s@CreateThesaurus' {} a -> s {indexId = a} :: CreateThesaurus)

-- | A name for the thesaurus.
createThesaurus_name :: Lens.Lens' CreateThesaurus Prelude.Text
createThesaurus_name = Lens.lens (\CreateThesaurus' {name} -> name) (\s@CreateThesaurus' {} a -> s {name = a} :: CreateThesaurus)

-- | The Amazon Resource Name (ARN) of an IAM role with permission to access
-- your S3 bucket that contains the thesaurus file. For more information,
-- see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM access roles for Amazon Kendra>.
createThesaurus_roleArn :: Lens.Lens' CreateThesaurus Prelude.Text
createThesaurus_roleArn = Lens.lens (\CreateThesaurus' {roleArn} -> roleArn) (\s@CreateThesaurus' {} a -> s {roleArn = a} :: CreateThesaurus)

-- | The path to the thesaurus file in S3.
createThesaurus_sourceS3Path :: Lens.Lens' CreateThesaurus S3Path
createThesaurus_sourceS3Path = Lens.lens (\CreateThesaurus' {sourceS3Path} -> sourceS3Path) (\s@CreateThesaurus' {} a -> s {sourceS3Path = a} :: CreateThesaurus)

instance Core.AWSRequest CreateThesaurus where
  type
    AWSResponse CreateThesaurus =
      CreateThesaurusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThesaurusResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateThesaurus where
  hashWithSalt _salt CreateThesaurus' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` indexId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` sourceS3Path

instance Prelude.NFData CreateThesaurus where
  rnf CreateThesaurus' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sourceS3Path

instance Data.ToHeaders CreateThesaurus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.CreateThesaurus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateThesaurus where
  toJSON CreateThesaurus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("SourceS3Path" Data..= sourceS3Path)
          ]
      )

instance Data.ToPath CreateThesaurus where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateThesaurus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateThesaurusResponse' smart constructor.
data CreateThesaurusResponse = CreateThesaurusResponse'
  { -- | The identifier of the thesaurus.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateThesaurusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createThesaurusResponse_id' - The identifier of the thesaurus.
--
-- 'httpStatus', 'createThesaurusResponse_httpStatus' - The response's http status code.
newCreateThesaurusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateThesaurusResponse
newCreateThesaurusResponse pHttpStatus_ =
  CreateThesaurusResponse'
    { id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the thesaurus.
createThesaurusResponse_id :: Lens.Lens' CreateThesaurusResponse (Prelude.Maybe Prelude.Text)
createThesaurusResponse_id = Lens.lens (\CreateThesaurusResponse' {id} -> id) (\s@CreateThesaurusResponse' {} a -> s {id = a} :: CreateThesaurusResponse)

-- | The response's http status code.
createThesaurusResponse_httpStatus :: Lens.Lens' CreateThesaurusResponse Prelude.Int
createThesaurusResponse_httpStatus = Lens.lens (\CreateThesaurusResponse' {httpStatus} -> httpStatus) (\s@CreateThesaurusResponse' {} a -> s {httpStatus = a} :: CreateThesaurusResponse)

instance Prelude.NFData CreateThesaurusResponse where
  rnf CreateThesaurusResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus

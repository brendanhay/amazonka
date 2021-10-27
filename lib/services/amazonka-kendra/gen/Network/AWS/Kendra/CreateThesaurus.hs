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
-- Module      : Network.AWS.Kendra.CreateThesaurus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a thesaurus for an index. The thesaurus contains a list of
-- synonyms in Solr format.
module Network.AWS.Kendra.CreateThesaurus
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

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateThesaurus' smart constructor.
data CreateThesaurus = CreateThesaurus'
  { -- | A token that you provide to identify the request to create a thesaurus.
    -- Multiple calls to the @CreateThesaurus@ operation with the same client
    -- token will create only one thesaurus.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description for the new thesaurus.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that identify the thesaurus. You can use the
    -- tags to identify and organize your resources and to control access to
    -- resources.
    tags :: Prelude.Maybe [Tag],
    -- | The unique identifier of the index for the new thesaurus.
    indexId :: Prelude.Text,
    -- | The name for the new thesaurus.
    name :: Prelude.Text,
    -- | An AWS Identity and Access Management (IAM) role that gives Amazon
    -- Kendra permissions to access thesaurus file specified in @SourceS3Path@.
    roleArn :: Prelude.Text,
    -- | The thesaurus file Amazon S3 source path.
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
-- Multiple calls to the @CreateThesaurus@ operation with the same client
-- token will create only one thesaurus.
--
-- 'description', 'createThesaurus_description' - The description for the new thesaurus.
--
-- 'tags', 'createThesaurus_tags' - A list of key-value pairs that identify the thesaurus. You can use the
-- tags to identify and organize your resources and to control access to
-- resources.
--
-- 'indexId', 'createThesaurus_indexId' - The unique identifier of the index for the new thesaurus.
--
-- 'name', 'createThesaurus_name' - The name for the new thesaurus.
--
-- 'roleArn', 'createThesaurus_roleArn' - An AWS Identity and Access Management (IAM) role that gives Amazon
-- Kendra permissions to access thesaurus file specified in @SourceS3Path@.
--
-- 'sourceS3Path', 'createThesaurus_sourceS3Path' - The thesaurus file Amazon S3 source path.
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
-- Multiple calls to the @CreateThesaurus@ operation with the same client
-- token will create only one thesaurus.
createThesaurus_clientToken :: Lens.Lens' CreateThesaurus (Prelude.Maybe Prelude.Text)
createThesaurus_clientToken = Lens.lens (\CreateThesaurus' {clientToken} -> clientToken) (\s@CreateThesaurus' {} a -> s {clientToken = a} :: CreateThesaurus)

-- | The description for the new thesaurus.
createThesaurus_description :: Lens.Lens' CreateThesaurus (Prelude.Maybe Prelude.Text)
createThesaurus_description = Lens.lens (\CreateThesaurus' {description} -> description) (\s@CreateThesaurus' {} a -> s {description = a} :: CreateThesaurus)

-- | A list of key-value pairs that identify the thesaurus. You can use the
-- tags to identify and organize your resources and to control access to
-- resources.
createThesaurus_tags :: Lens.Lens' CreateThesaurus (Prelude.Maybe [Tag])
createThesaurus_tags = Lens.lens (\CreateThesaurus' {tags} -> tags) (\s@CreateThesaurus' {} a -> s {tags = a} :: CreateThesaurus) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the index for the new thesaurus.
createThesaurus_indexId :: Lens.Lens' CreateThesaurus Prelude.Text
createThesaurus_indexId = Lens.lens (\CreateThesaurus' {indexId} -> indexId) (\s@CreateThesaurus' {} a -> s {indexId = a} :: CreateThesaurus)

-- | The name for the new thesaurus.
createThesaurus_name :: Lens.Lens' CreateThesaurus Prelude.Text
createThesaurus_name = Lens.lens (\CreateThesaurus' {name} -> name) (\s@CreateThesaurus' {} a -> s {name = a} :: CreateThesaurus)

-- | An AWS Identity and Access Management (IAM) role that gives Amazon
-- Kendra permissions to access thesaurus file specified in @SourceS3Path@.
createThesaurus_roleArn :: Lens.Lens' CreateThesaurus Prelude.Text
createThesaurus_roleArn = Lens.lens (\CreateThesaurus' {roleArn} -> roleArn) (\s@CreateThesaurus' {} a -> s {roleArn = a} :: CreateThesaurus)

-- | The thesaurus file Amazon S3 source path.
createThesaurus_sourceS3Path :: Lens.Lens' CreateThesaurus S3Path
createThesaurus_sourceS3Path = Lens.lens (\CreateThesaurus' {sourceS3Path} -> sourceS3Path) (\s@CreateThesaurus' {} a -> s {sourceS3Path = a} :: CreateThesaurus)

instance Core.AWSRequest CreateThesaurus where
  type
    AWSResponse CreateThesaurus =
      CreateThesaurusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateThesaurusResponse'
            Prelude.<$> (x Core..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateThesaurus

instance Prelude.NFData CreateThesaurus

instance Core.ToHeaders CreateThesaurus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.CreateThesaurus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateThesaurus where
  toJSON CreateThesaurus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just ("SourceS3Path" Core..= sourceS3Path)
          ]
      )

instance Core.ToPath CreateThesaurus where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateThesaurus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateThesaurusResponse' smart constructor.
data CreateThesaurusResponse = CreateThesaurusResponse'
  { -- | The unique identifier of the thesaurus.
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
-- 'id', 'createThesaurusResponse_id' - The unique identifier of the thesaurus.
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

-- | The unique identifier of the thesaurus.
createThesaurusResponse_id :: Lens.Lens' CreateThesaurusResponse (Prelude.Maybe Prelude.Text)
createThesaurusResponse_id = Lens.lens (\CreateThesaurusResponse' {id} -> id) (\s@CreateThesaurusResponse' {} a -> s {id = a} :: CreateThesaurusResponse)

-- | The response's http status code.
createThesaurusResponse_httpStatus :: Lens.Lens' CreateThesaurusResponse Prelude.Int
createThesaurusResponse_httpStatus = Lens.lens (\CreateThesaurusResponse' {httpStatus} -> httpStatus) (\s@CreateThesaurusResponse' {} a -> s {httpStatus = a} :: CreateThesaurusResponse)

instance Prelude.NFData CreateThesaurusResponse

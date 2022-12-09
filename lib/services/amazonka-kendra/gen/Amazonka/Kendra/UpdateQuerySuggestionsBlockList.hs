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
-- Module      : Amazonka.Kendra.UpdateQuerySuggestionsBlockList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a block list used for query suggestions for an index.
--
-- Updates to a block list might not take effect right away. Amazon Kendra
-- needs to refresh the entire suggestions list to apply any updates to the
-- block list. Other changes not related to the block list apply
-- immediately.
--
-- If a block list is updating, then you need to wait for the first update
-- to finish before submitting another update.
--
-- Amazon Kendra supports partial updates, so you only need to provide the
-- fields you want to update.
--
-- @UpdateQuerySuggestionsBlockList@ is currently not supported in the
-- Amazon Web Services GovCloud (US-West) region.
module Amazonka.Kendra.UpdateQuerySuggestionsBlockList
  ( -- * Creating a Request
    UpdateQuerySuggestionsBlockList (..),
    newUpdateQuerySuggestionsBlockList,

    -- * Request Lenses
    updateQuerySuggestionsBlockList_description,
    updateQuerySuggestionsBlockList_name,
    updateQuerySuggestionsBlockList_roleArn,
    updateQuerySuggestionsBlockList_sourceS3Path,
    updateQuerySuggestionsBlockList_indexId,
    updateQuerySuggestionsBlockList_id,

    -- * Destructuring the Response
    UpdateQuerySuggestionsBlockListResponse (..),
    newUpdateQuerySuggestionsBlockListResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateQuerySuggestionsBlockList' smart constructor.
data UpdateQuerySuggestionsBlockList = UpdateQuerySuggestionsBlockList'
  { -- | A new description for the block list.
    description :: Prelude.Maybe Prelude.Text,
    -- | A new name for the block list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The IAM (Identity and Access Management) role used to access the block
    -- list text file in S3.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The S3 path where your block list text file sits in S3.
    --
    -- If you update your block list and provide the same path to the block
    -- list text file in S3, then Amazon Kendra reloads the file to refresh the
    -- block list. Amazon Kendra does not automatically refresh your block
    -- list. You need to call the @UpdateQuerySuggestionsBlockList@ API to
    -- refresh you block list.
    --
    -- If you update your block list, then Amazon Kendra asynchronously
    -- refreshes all query suggestions with the latest content in the S3 file.
    -- This means changes might not take effect immediately.
    sourceS3Path :: Prelude.Maybe S3Path,
    -- | The identifier of the index for the block list.
    indexId :: Prelude.Text,
    -- | The identifier of the block list you want to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQuerySuggestionsBlockList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateQuerySuggestionsBlockList_description' - A new description for the block list.
--
-- 'name', 'updateQuerySuggestionsBlockList_name' - A new name for the block list.
--
-- 'roleArn', 'updateQuerySuggestionsBlockList_roleArn' - The IAM (Identity and Access Management) role used to access the block
-- list text file in S3.
--
-- 'sourceS3Path', 'updateQuerySuggestionsBlockList_sourceS3Path' - The S3 path where your block list text file sits in S3.
--
-- If you update your block list and provide the same path to the block
-- list text file in S3, then Amazon Kendra reloads the file to refresh the
-- block list. Amazon Kendra does not automatically refresh your block
-- list. You need to call the @UpdateQuerySuggestionsBlockList@ API to
-- refresh you block list.
--
-- If you update your block list, then Amazon Kendra asynchronously
-- refreshes all query suggestions with the latest content in the S3 file.
-- This means changes might not take effect immediately.
--
-- 'indexId', 'updateQuerySuggestionsBlockList_indexId' - The identifier of the index for the block list.
--
-- 'id', 'updateQuerySuggestionsBlockList_id' - The identifier of the block list you want to update.
newUpdateQuerySuggestionsBlockList ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  UpdateQuerySuggestionsBlockList
newUpdateQuerySuggestionsBlockList pIndexId_ pId_ =
  UpdateQuerySuggestionsBlockList'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      sourceS3Path = Prelude.Nothing,
      indexId = pIndexId_,
      id = pId_
    }

-- | A new description for the block list.
updateQuerySuggestionsBlockList_description :: Lens.Lens' UpdateQuerySuggestionsBlockList (Prelude.Maybe Prelude.Text)
updateQuerySuggestionsBlockList_description = Lens.lens (\UpdateQuerySuggestionsBlockList' {description} -> description) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {description = a} :: UpdateQuerySuggestionsBlockList)

-- | A new name for the block list.
updateQuerySuggestionsBlockList_name :: Lens.Lens' UpdateQuerySuggestionsBlockList (Prelude.Maybe Prelude.Text)
updateQuerySuggestionsBlockList_name = Lens.lens (\UpdateQuerySuggestionsBlockList' {name} -> name) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {name = a} :: UpdateQuerySuggestionsBlockList)

-- | The IAM (Identity and Access Management) role used to access the block
-- list text file in S3.
updateQuerySuggestionsBlockList_roleArn :: Lens.Lens' UpdateQuerySuggestionsBlockList (Prelude.Maybe Prelude.Text)
updateQuerySuggestionsBlockList_roleArn = Lens.lens (\UpdateQuerySuggestionsBlockList' {roleArn} -> roleArn) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {roleArn = a} :: UpdateQuerySuggestionsBlockList)

-- | The S3 path where your block list text file sits in S3.
--
-- If you update your block list and provide the same path to the block
-- list text file in S3, then Amazon Kendra reloads the file to refresh the
-- block list. Amazon Kendra does not automatically refresh your block
-- list. You need to call the @UpdateQuerySuggestionsBlockList@ API to
-- refresh you block list.
--
-- If you update your block list, then Amazon Kendra asynchronously
-- refreshes all query suggestions with the latest content in the S3 file.
-- This means changes might not take effect immediately.
updateQuerySuggestionsBlockList_sourceS3Path :: Lens.Lens' UpdateQuerySuggestionsBlockList (Prelude.Maybe S3Path)
updateQuerySuggestionsBlockList_sourceS3Path = Lens.lens (\UpdateQuerySuggestionsBlockList' {sourceS3Path} -> sourceS3Path) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {sourceS3Path = a} :: UpdateQuerySuggestionsBlockList)

-- | The identifier of the index for the block list.
updateQuerySuggestionsBlockList_indexId :: Lens.Lens' UpdateQuerySuggestionsBlockList Prelude.Text
updateQuerySuggestionsBlockList_indexId = Lens.lens (\UpdateQuerySuggestionsBlockList' {indexId} -> indexId) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {indexId = a} :: UpdateQuerySuggestionsBlockList)

-- | The identifier of the block list you want to update.
updateQuerySuggestionsBlockList_id :: Lens.Lens' UpdateQuerySuggestionsBlockList Prelude.Text
updateQuerySuggestionsBlockList_id = Lens.lens (\UpdateQuerySuggestionsBlockList' {id} -> id) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {id = a} :: UpdateQuerySuggestionsBlockList)

instance
  Core.AWSRequest
    UpdateQuerySuggestionsBlockList
  where
  type
    AWSResponse UpdateQuerySuggestionsBlockList =
      UpdateQuerySuggestionsBlockListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateQuerySuggestionsBlockListResponse'

instance
  Prelude.Hashable
    UpdateQuerySuggestionsBlockList
  where
  hashWithSalt
    _salt
    UpdateQuerySuggestionsBlockList' {..} =
      _salt `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` sourceS3Path
        `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateQuerySuggestionsBlockList
  where
  rnf UpdateQuerySuggestionsBlockList' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sourceS3Path
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    UpdateQuerySuggestionsBlockList
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.UpdateQuerySuggestionsBlockList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateQuerySuggestionsBlockList where
  toJSON UpdateQuerySuggestionsBlockList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("SourceS3Path" Data..=) Prelude.<$> sourceS3Path,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath UpdateQuerySuggestionsBlockList where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateQuerySuggestionsBlockList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateQuerySuggestionsBlockListResponse' smart constructor.
data UpdateQuerySuggestionsBlockListResponse = UpdateQuerySuggestionsBlockListResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateQuerySuggestionsBlockListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateQuerySuggestionsBlockListResponse ::
  UpdateQuerySuggestionsBlockListResponse
newUpdateQuerySuggestionsBlockListResponse =
  UpdateQuerySuggestionsBlockListResponse'

instance
  Prelude.NFData
    UpdateQuerySuggestionsBlockListResponse
  where
  rnf _ = ()

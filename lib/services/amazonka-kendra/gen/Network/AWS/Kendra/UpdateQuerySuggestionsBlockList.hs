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
-- Module      : Network.AWS.Kendra.UpdateQuerySuggestionsBlockList
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Kendra.UpdateQuerySuggestionsBlockList
  ( -- * Creating a Request
    UpdateQuerySuggestionsBlockList (..),
    newUpdateQuerySuggestionsBlockList,

    -- * Request Lenses
    updateQuerySuggestionsBlockList_sourceS3Path,
    updateQuerySuggestionsBlockList_name,
    updateQuerySuggestionsBlockList_description,
    updateQuerySuggestionsBlockList_roleArn,
    updateQuerySuggestionsBlockList_indexId,
    updateQuerySuggestionsBlockList_id,

    -- * Destructuring the Response
    UpdateQuerySuggestionsBlockListResponse (..),
    newUpdateQuerySuggestionsBlockListResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateQuerySuggestionsBlockList' smart constructor.
data UpdateQuerySuggestionsBlockList = UpdateQuerySuggestionsBlockList'
  { -- | The S3 path where your block list text file sits in S3.
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
    -- | The name of a block list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The description for a block list.
    description :: Prelude.Maybe Prelude.Text,
    -- | The IAM (Identity and Access Management) role used to access the block
    -- list text file in S3.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index for a block list.
    indexId :: Prelude.Text,
    -- | The unique identifier of a block list.
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
-- 'name', 'updateQuerySuggestionsBlockList_name' - The name of a block list.
--
-- 'description', 'updateQuerySuggestionsBlockList_description' - The description for a block list.
--
-- 'roleArn', 'updateQuerySuggestionsBlockList_roleArn' - The IAM (Identity and Access Management) role used to access the block
-- list text file in S3.
--
-- 'indexId', 'updateQuerySuggestionsBlockList_indexId' - The identifier of the index for a block list.
--
-- 'id', 'updateQuerySuggestionsBlockList_id' - The unique identifier of a block list.
newUpdateQuerySuggestionsBlockList ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  UpdateQuerySuggestionsBlockList
newUpdateQuerySuggestionsBlockList pIndexId_ pId_ =
  UpdateQuerySuggestionsBlockList'
    { sourceS3Path =
        Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      indexId = pIndexId_,
      id = pId_
    }

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

-- | The name of a block list.
updateQuerySuggestionsBlockList_name :: Lens.Lens' UpdateQuerySuggestionsBlockList (Prelude.Maybe Prelude.Text)
updateQuerySuggestionsBlockList_name = Lens.lens (\UpdateQuerySuggestionsBlockList' {name} -> name) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {name = a} :: UpdateQuerySuggestionsBlockList)

-- | The description for a block list.
updateQuerySuggestionsBlockList_description :: Lens.Lens' UpdateQuerySuggestionsBlockList (Prelude.Maybe Prelude.Text)
updateQuerySuggestionsBlockList_description = Lens.lens (\UpdateQuerySuggestionsBlockList' {description} -> description) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {description = a} :: UpdateQuerySuggestionsBlockList)

-- | The IAM (Identity and Access Management) role used to access the block
-- list text file in S3.
updateQuerySuggestionsBlockList_roleArn :: Lens.Lens' UpdateQuerySuggestionsBlockList (Prelude.Maybe Prelude.Text)
updateQuerySuggestionsBlockList_roleArn = Lens.lens (\UpdateQuerySuggestionsBlockList' {roleArn} -> roleArn) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {roleArn = a} :: UpdateQuerySuggestionsBlockList)

-- | The identifier of the index for a block list.
updateQuerySuggestionsBlockList_indexId :: Lens.Lens' UpdateQuerySuggestionsBlockList Prelude.Text
updateQuerySuggestionsBlockList_indexId = Lens.lens (\UpdateQuerySuggestionsBlockList' {indexId} -> indexId) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {indexId = a} :: UpdateQuerySuggestionsBlockList)

-- | The unique identifier of a block list.
updateQuerySuggestionsBlockList_id :: Lens.Lens' UpdateQuerySuggestionsBlockList Prelude.Text
updateQuerySuggestionsBlockList_id = Lens.lens (\UpdateQuerySuggestionsBlockList' {id} -> id) (\s@UpdateQuerySuggestionsBlockList' {} a -> s {id = a} :: UpdateQuerySuggestionsBlockList)

instance
  Core.AWSRequest
    UpdateQuerySuggestionsBlockList
  where
  type
    AWSResponse UpdateQuerySuggestionsBlockList =
      UpdateQuerySuggestionsBlockListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      UpdateQuerySuggestionsBlockListResponse'

instance
  Prelude.Hashable
    UpdateQuerySuggestionsBlockList

instance
  Prelude.NFData
    UpdateQuerySuggestionsBlockList

instance
  Core.ToHeaders
    UpdateQuerySuggestionsBlockList
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.UpdateQuerySuggestionsBlockList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateQuerySuggestionsBlockList where
  toJSON UpdateQuerySuggestionsBlockList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceS3Path" Core..=) Prelude.<$> sourceS3Path,
            ("Name" Core..=) Prelude.<$> name,
            ("Description" Core..=) Prelude.<$> description,
            ("RoleArn" Core..=) Prelude.<$> roleArn,
            Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath UpdateQuerySuggestionsBlockList where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateQuerySuggestionsBlockList where
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

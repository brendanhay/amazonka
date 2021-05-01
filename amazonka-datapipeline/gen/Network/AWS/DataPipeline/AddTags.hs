{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DataPipeline.AddTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or modifies tags for the specified pipeline.
module Network.AWS.DataPipeline.AddTags
  ( -- * Creating a Request
    AddTags (..),
    newAddTags,

    -- * Request Lenses
    addTags_pipelineId,
    addTags_tags,

    -- * Destructuring the Response
    AddTagsResponse (..),
    newAddTagsResponse,

    -- * Response Lenses
    addTagsResponse_httpStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for AddTags.
--
-- /See:/ 'newAddTags' smart constructor.
data AddTags = AddTags'
  { -- | The ID of the pipeline.
    pipelineId :: Prelude.Text,
    -- | The tags to add, as key\/value pairs.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'addTags_pipelineId' - The ID of the pipeline.
--
-- 'tags', 'addTags_tags' - The tags to add, as key\/value pairs.
newAddTags ::
  -- | 'pipelineId'
  Prelude.Text ->
  AddTags
newAddTags pPipelineId_ =
  AddTags'
    { pipelineId = pPipelineId_,
      tags = Prelude.mempty
    }

-- | The ID of the pipeline.
addTags_pipelineId :: Lens.Lens' AddTags Prelude.Text
addTags_pipelineId = Lens.lens (\AddTags' {pipelineId} -> pipelineId) (\s@AddTags' {} a -> s {pipelineId = a} :: AddTags)

-- | The tags to add, as key\/value pairs.
addTags_tags :: Lens.Lens' AddTags [Tag]
addTags_tags = Lens.lens (\AddTags' {tags} -> tags) (\s@AddTags' {} a -> s {tags = a} :: AddTags) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddTags where
  type Rs AddTags = AddTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddTags

instance Prelude.NFData AddTags

instance Prelude.ToHeaders AddTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("DataPipeline.AddTags" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddTags where
  toJSON AddTags' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("pipelineId" Prelude..= pipelineId),
            Prelude.Just ("tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath AddTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddTags where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of AddTags.
--
-- /See:/ 'newAddTagsResponse' smart constructor.
data AddTagsResponse = AddTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addTagsResponse_httpStatus' - The response's http status code.
newAddTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddTagsResponse
newAddTagsResponse pHttpStatus_ =
  AddTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
addTagsResponse_httpStatus :: Lens.Lens' AddTagsResponse Prelude.Int
addTagsResponse_httpStatus = Lens.lens (\AddTagsResponse' {httpStatus} -> httpStatus) (\s@AddTagsResponse' {} a -> s {httpStatus = a} :: AddTagsResponse)

instance Prelude.NFData AddTagsResponse

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
-- Module      : Amazonka.DataPipeline.RemoveTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes existing tags from the specified pipeline.
module Amazonka.DataPipeline.RemoveTags
  ( -- * Creating a Request
    RemoveTags (..),
    newRemoveTags,

    -- * Request Lenses
    removeTags_pipelineId,
    removeTags_tagKeys,

    -- * Destructuring the Response
    RemoveTagsResponse (..),
    newRemoveTagsResponse,

    -- * Response Lenses
    removeTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for RemoveTags.
--
-- /See:/ 'newRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | The ID of the pipeline.
    pipelineId :: Prelude.Text,
    -- | The keys of the tags to remove.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'removeTags_pipelineId' - The ID of the pipeline.
--
-- 'tagKeys', 'removeTags_tagKeys' - The keys of the tags to remove.
newRemoveTags ::
  -- | 'pipelineId'
  Prelude.Text ->
  RemoveTags
newRemoveTags pPipelineId_ =
  RemoveTags'
    { pipelineId = pPipelineId_,
      tagKeys = Prelude.mempty
    }

-- | The ID of the pipeline.
removeTags_pipelineId :: Lens.Lens' RemoveTags Prelude.Text
removeTags_pipelineId = Lens.lens (\RemoveTags' {pipelineId} -> pipelineId) (\s@RemoveTags' {} a -> s {pipelineId = a} :: RemoveTags)

-- | The keys of the tags to remove.
removeTags_tagKeys :: Lens.Lens' RemoveTags [Prelude.Text]
removeTags_tagKeys = Lens.lens (\RemoveTags' {tagKeys} -> tagKeys) (\s@RemoveTags' {} a -> s {tagKeys = a} :: RemoveTags) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveTags where
  type AWSResponse RemoveTags = RemoveTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveTags where
  hashWithSalt _salt RemoveTags' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineId
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData RemoveTags where
  rnf RemoveTags' {..} =
    Prelude.rnf pipelineId
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders RemoveTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("DataPipeline.RemoveTags" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("pipelineId" Data..= pipelineId),
            Prelude.Just ("tagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath RemoveTags where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveTags where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of RemoveTags.
--
-- /See:/ 'newRemoveTagsResponse' smart constructor.
data RemoveTagsResponse = RemoveTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeTagsResponse_httpStatus' - The response's http status code.
newRemoveTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveTagsResponse
newRemoveTagsResponse pHttpStatus_ =
  RemoveTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeTagsResponse_httpStatus :: Lens.Lens' RemoveTagsResponse Prelude.Int
removeTagsResponse_httpStatus = Lens.lens (\RemoveTagsResponse' {httpStatus} -> httpStatus) (\s@RemoveTagsResponse' {} a -> s {httpStatus = a} :: RemoveTagsResponse)

instance Prelude.NFData RemoveTagsResponse where
  rnf RemoveTagsResponse' {..} = Prelude.rnf httpStatus

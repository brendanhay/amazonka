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
-- Module      : Amazonka.CloudTrail.RemoveTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from a trail or event data store.
module Amazonka.CloudTrail.RemoveTags
  ( -- * Creating a Request
    RemoveTags (..),
    newRemoveTags,

    -- * Request Lenses
    removeTags_resourceId,
    removeTags_tagsList,

    -- * Destructuring the Response
    RemoveTagsResponse (..),
    newRemoveTagsResponse,

    -- * Response Lenses
    removeTagsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Specifies the tags to remove from a trail or event data store.
--
-- /See:/ 'newRemoveTags' smart constructor.
data RemoveTags = RemoveTags'
  { -- | Specifies the ARN of the trail or event data store from which tags
    -- should be removed.
    --
    -- Example trail ARN format:
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
    --
    -- Example event data store ARN format:
    -- @arn:aws:cloudtrail:us-east-2:12345678910:eventdatastore\/EXAMPLE-f852-4e8f-8bd1-bcf6cEXAMPLE@
    resourceId :: Prelude.Text,
    -- | Specifies a list of tags to be removed.
    tagsList :: [Tag]
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
-- 'resourceId', 'removeTags_resourceId' - Specifies the ARN of the trail or event data store from which tags
-- should be removed.
--
-- Example trail ARN format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- Example event data store ARN format:
-- @arn:aws:cloudtrail:us-east-2:12345678910:eventdatastore\/EXAMPLE-f852-4e8f-8bd1-bcf6cEXAMPLE@
--
-- 'tagsList', 'removeTags_tagsList' - Specifies a list of tags to be removed.
newRemoveTags ::
  -- | 'resourceId'
  Prelude.Text ->
  RemoveTags
newRemoveTags pResourceId_ =
  RemoveTags'
    { resourceId = pResourceId_,
      tagsList = Prelude.mempty
    }

-- | Specifies the ARN of the trail or event data store from which tags
-- should be removed.
--
-- Example trail ARN format:
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@
--
-- Example event data store ARN format:
-- @arn:aws:cloudtrail:us-east-2:12345678910:eventdatastore\/EXAMPLE-f852-4e8f-8bd1-bcf6cEXAMPLE@
removeTags_resourceId :: Lens.Lens' RemoveTags Prelude.Text
removeTags_resourceId = Lens.lens (\RemoveTags' {resourceId} -> resourceId) (\s@RemoveTags' {} a -> s {resourceId = a} :: RemoveTags)

-- | Specifies a list of tags to be removed.
removeTags_tagsList :: Lens.Lens' RemoveTags [Tag]
removeTags_tagsList = Lens.lens (\RemoveTags' {tagsList} -> tagsList) (\s@RemoveTags' {} a -> s {tagsList = a} :: RemoveTags) Prelude.. Lens.coerced

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
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tagsList

instance Prelude.NFData RemoveTags where
  rnf RemoveTags' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tagsList

instance Core.ToHeaders RemoveTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.RemoveTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveTags where
  toJSON RemoveTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Core..= resourceId),
            Prelude.Just ("TagsList" Core..= tagsList)
          ]
      )

instance Core.ToPath RemoveTags where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveTags where
  toQuery = Prelude.const Prelude.mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
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

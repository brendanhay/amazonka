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
-- Module      : Network.AWS.SageMaker.DeleteTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags from an Amazon SageMaker resource.
--
-- To list a resource\'s tags, use the @ListTags@ API.
--
-- When you call this API to delete tags from a hyperparameter tuning job,
-- the deleted tags are not removed from training jobs that the
-- hyperparameter tuning job launched before you called this API.
--
-- When you call this API to delete tags from a SageMaker Studio Domain or
-- User Profile, the deleted tags are not removed from Apps that the
-- SageMaker Studio Domain or User Profile launched before you called this
-- API.
module Network.AWS.SageMaker.DeleteTags
  ( -- * Creating a Request
    DeleteTags (..),
    newDeleteTags,

    -- * Request Lenses
    deleteTags_resourceArn,
    deleteTags_tagKeys,

    -- * Destructuring the Response
    DeleteTagsResponse (..),
    newDeleteTagsResponse,

    -- * Response Lenses
    deleteTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteTags' smart constructor.
data DeleteTags = DeleteTags'
  { -- | The Amazon Resource Name (ARN) of the resource whose tags you want to
    -- delete.
    resourceArn :: Prelude.Text,
    -- | An array or one or more tag keys to delete.
    tagKeys :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'deleteTags_resourceArn' - The Amazon Resource Name (ARN) of the resource whose tags you want to
-- delete.
--
-- 'tagKeys', 'deleteTags_tagKeys' - An array or one or more tag keys to delete.
newDeleteTags ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'tagKeys'
  Prelude.NonEmpty Prelude.Text ->
  DeleteTags
newDeleteTags pResourceArn_ pTagKeys_ =
  DeleteTags'
    { resourceArn = pResourceArn_,
      tagKeys = Lens.coerced Lens.# pTagKeys_
    }

-- | The Amazon Resource Name (ARN) of the resource whose tags you want to
-- delete.
deleteTags_resourceArn :: Lens.Lens' DeleteTags Prelude.Text
deleteTags_resourceArn = Lens.lens (\DeleteTags' {resourceArn} -> resourceArn) (\s@DeleteTags' {} a -> s {resourceArn = a} :: DeleteTags)

-- | An array or one or more tag keys to delete.
deleteTags_tagKeys :: Lens.Lens' DeleteTags (Prelude.NonEmpty Prelude.Text)
deleteTags_tagKeys = Lens.lens (\DeleteTags' {tagKeys} -> tagKeys) (\s@DeleteTags' {} a -> s {tagKeys = a} :: DeleteTags) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteTags where
  type AWSResponse DeleteTags = DeleteTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTags

instance Prelude.NFData DeleteTags

instance Core.ToHeaders DeleteTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteTags" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteTags where
  toJSON DeleteTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Core..= resourceArn),
            Prelude.Just ("TagKeys" Core..= tagKeys)
          ]
      )

instance Core.ToPath DeleteTags where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTagsResponse_httpStatus' - The response's http status code.
newDeleteTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTagsResponse
newDeleteTagsResponse pHttpStatus_ =
  DeleteTagsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTagsResponse_httpStatus :: Lens.Lens' DeleteTagsResponse Prelude.Int
deleteTagsResponse_httpStatus = Lens.lens (\DeleteTagsResponse' {httpStatus} -> httpStatus) (\s@DeleteTagsResponse' {} a -> s {httpStatus = a} :: DeleteTagsResponse)

instance Prelude.NFData DeleteTagsResponse

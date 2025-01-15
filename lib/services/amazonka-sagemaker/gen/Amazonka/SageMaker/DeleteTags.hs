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
-- Module      : Amazonka.SageMaker.DeleteTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags from an SageMaker resource.
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
module Amazonka.SageMaker.DeleteTags
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTagsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTags where
  hashWithSalt _salt DeleteTags' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData DeleteTags where
  rnf DeleteTags' {..} =
    Prelude.rnf resourceArn `Prelude.seq`
      Prelude.rnf tagKeys

instance Data.ToHeaders DeleteTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteTags" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTags where
  toJSON DeleteTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just ("TagKeys" Data..= tagKeys)
          ]
      )

instance Data.ToPath DeleteTags where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTags where
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

instance Prelude.NFData DeleteTagsResponse where
  rnf DeleteTagsResponse' {..} = Prelude.rnf httpStatus

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
-- Module      : Network.AWS.CodeStar.TagProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a project.
module Network.AWS.CodeStar.TagProject
  ( -- * Creating a Request
    TagProject (..),
    newTagProject,

    -- * Request Lenses
    tagProject_id,
    tagProject_tags,

    -- * Destructuring the Response
    TagProjectResponse (..),
    newTagProjectResponse,

    -- * Response Lenses
    tagProjectResponse_tags,
    tagProjectResponse_httpStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTagProject' smart constructor.
data TagProject = TagProject'
  { -- | The ID of the project you want to add a tag to.
    id :: Prelude.Text,
    -- | The tags you want to add to the project.
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'tagProject_id' - The ID of the project you want to add a tag to.
--
-- 'tags', 'tagProject_tags' - The tags you want to add to the project.
newTagProject ::
  -- | 'id'
  Prelude.Text ->
  TagProject
newTagProject pId_ =
  TagProject' {id = pId_, tags = Prelude.mempty}

-- | The ID of the project you want to add a tag to.
tagProject_id :: Lens.Lens' TagProject Prelude.Text
tagProject_id = Lens.lens (\TagProject' {id} -> id) (\s@TagProject' {} a -> s {id = a} :: TagProject)

-- | The tags you want to add to the project.
tagProject_tags :: Lens.Lens' TagProject (Prelude.HashMap Prelude.Text Prelude.Text)
tagProject_tags = Lens.lens (\TagProject' {tags} -> tags) (\s@TagProject' {} a -> s {tags = a} :: TagProject) Prelude.. Lens._Coerce

instance Core.AWSRequest TagProject where
  type AWSResponse TagProject = TagProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          TagProjectResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TagProject

instance Prelude.NFData TagProject

instance Core.ToHeaders TagProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.TagProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TagProject where
  toJSON TagProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Core..= id),
            Prelude.Just ("tags" Core..= tags)
          ]
      )

instance Core.ToPath TagProject where
  toPath = Prelude.const "/"

instance Core.ToQuery TagProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTagProjectResponse' smart constructor.
data TagProjectResponse = TagProjectResponse'
  { -- | The tags for the project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'tagProjectResponse_tags' - The tags for the project.
--
-- 'httpStatus', 'tagProjectResponse_httpStatus' - The response's http status code.
newTagProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TagProjectResponse
newTagProjectResponse pHttpStatus_ =
  TagProjectResponse'
    { tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags for the project.
tagProjectResponse_tags :: Lens.Lens' TagProjectResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
tagProjectResponse_tags = Lens.lens (\TagProjectResponse' {tags} -> tags) (\s@TagProjectResponse' {} a -> s {tags = a} :: TagProjectResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
tagProjectResponse_httpStatus :: Lens.Lens' TagProjectResponse Prelude.Int
tagProjectResponse_httpStatus = Lens.lens (\TagProjectResponse' {httpStatus} -> httpStatus) (\s@TagProjectResponse' {} a -> s {httpStatus = a} :: TagProjectResponse)

instance Prelude.NFData TagProjectResponse

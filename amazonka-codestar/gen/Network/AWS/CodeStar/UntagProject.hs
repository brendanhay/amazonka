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
-- Module      : Network.AWS.CodeStar.UntagProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a project.
module Network.AWS.CodeStar.UntagProject
  ( -- * Creating a Request
    UntagProject (..),
    newUntagProject,

    -- * Request Lenses
    untagProject_id,
    untagProject_tags,

    -- * Destructuring the Response
    UntagProjectResponse (..),
    newUntagProjectResponse,

    -- * Response Lenses
    untagProjectResponse_httpStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagProject' smart constructor.
data UntagProject = UntagProject'
  { -- | The ID of the project to remove tags from.
    id :: Prelude.Text,
    -- | The tags to remove from the project.
    tags :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'untagProject_id' - The ID of the project to remove tags from.
--
-- 'tags', 'untagProject_tags' - The tags to remove from the project.
newUntagProject ::
  -- | 'id'
  Prelude.Text ->
  UntagProject
newUntagProject pId_ =
  UntagProject' {id = pId_, tags = Prelude.mempty}

-- | The ID of the project to remove tags from.
untagProject_id :: Lens.Lens' UntagProject Prelude.Text
untagProject_id = Lens.lens (\UntagProject' {id} -> id) (\s@UntagProject' {} a -> s {id = a} :: UntagProject)

-- | The tags to remove from the project.
untagProject_tags :: Lens.Lens' UntagProject [Prelude.Text]
untagProject_tags = Lens.lens (\UntagProject' {tags} -> tags) (\s@UntagProject' {} a -> s {tags = a} :: UntagProject) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagProject where
  type Rs UntagProject = UntagProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UntagProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UntagProject

instance Prelude.NFData UntagProject

instance Prelude.ToHeaders UntagProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeStar_20170419.UntagProject" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UntagProject where
  toJSON UntagProject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Prelude..= id),
            Prelude.Just ("tags" Prelude..= tags)
          ]
      )

instance Prelude.ToPath UntagProject where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUntagProjectResponse' smart constructor.
data UntagProjectResponse = UntagProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UntagProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'untagProjectResponse_httpStatus' - The response's http status code.
newUntagProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UntagProjectResponse
newUntagProjectResponse pHttpStatus_ =
  UntagProjectResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
untagProjectResponse_httpStatus :: Lens.Lens' UntagProjectResponse Prelude.Int
untagProjectResponse_httpStatus = Lens.lens (\UntagProjectResponse' {httpStatus} -> httpStatus) (\s@UntagProjectResponse' {} a -> s {httpStatus = a} :: UntagProjectResponse)

instance Prelude.NFData UntagProjectResponse

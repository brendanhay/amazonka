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
-- Module      : Amazonka.MediaLive.CreateTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create tags for a resource
module Amazonka.MediaLive.CreateTags
  ( -- * Creating a Request
    CreateTags (..),
    newCreateTags,

    -- * Request Lenses
    createTags_tags,
    createTags_resourceArn,

    -- * Destructuring the Response
    CreateTagsResponse (..),
    newCreateTagsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for CreateTagsRequest
--
-- /See:/ 'newCreateTags' smart constructor.
data CreateTags = CreateTags'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTags_tags' - Undocumented member.
--
-- 'resourceArn', 'createTags_resourceArn' - Undocumented member.
newCreateTags ::
  -- | 'resourceArn'
  Prelude.Text ->
  CreateTags
newCreateTags pResourceArn_ =
  CreateTags'
    { tags = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | Undocumented member.
createTags_tags :: Lens.Lens' CreateTags (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createTags_tags = Lens.lens (\CreateTags' {tags} -> tags) (\s@CreateTags' {} a -> s {tags = a} :: CreateTags) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createTags_resourceArn :: Lens.Lens' CreateTags Prelude.Text
createTags_resourceArn = Lens.lens (\CreateTags' {resourceArn} -> resourceArn) (\s@CreateTags' {} a -> s {resourceArn = a} :: CreateTags)

instance Core.AWSRequest CreateTags where
  type AWSResponse CreateTags = CreateTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull CreateTagsResponse'

instance Prelude.Hashable CreateTags where
  hashWithSalt _salt CreateTags' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData CreateTags where
  rnf CreateTags' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders CreateTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTags where
  toJSON CreateTags' {..} =
    Data.object
      ( Prelude.catMaybes
          [("tags" Data..=) Prelude.<$> tags]
      )

instance Data.ToPath CreateTags where
  toPath CreateTags' {..} =
    Prelude.mconcat
      ["/prod/tags/", Data.toBS resourceArn]

instance Data.ToQuery CreateTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateTagsResponse ::
  CreateTagsResponse
newCreateTagsResponse = CreateTagsResponse'

instance Prelude.NFData CreateTagsResponse where
  rnf _ = ()

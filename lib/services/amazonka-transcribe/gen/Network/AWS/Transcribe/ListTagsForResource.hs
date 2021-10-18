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
-- Module      : Network.AWS.Transcribe.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags associated with a given transcription job, vocabulary, or
-- resource.
module Network.AWS.Transcribe.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_resourceArn,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | Lists all tags associated with a given Amazon Resource Name (ARN).
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'listTagsForResource_resourceArn' - Lists all tags associated with a given Amazon Resource Name (ARN).
newListTagsForResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pResourceArn_ =
  ListTagsForResource' {resourceArn = pResourceArn_}

-- | Lists all tags associated with a given Amazon Resource Name (ARN).
listTagsForResource_resourceArn :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_resourceArn = Lens.lens (\ListTagsForResource' {resourceArn} -> resourceArn) (\s@ListTagsForResource' {} a -> s {resourceArn = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> (x Core..?> "ResourceArn")
            Prelude.<*> (x Core..?> "Tags")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResource

instance Prelude.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListTagsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Core..= resourceArn)]
      )

instance Core.ToPath ListTagsForResource where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTagsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | Lists all tags associated with the given Amazon Resource Name (ARN).
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Lists all tags associated with the given transcription job, vocabulary,
    -- or resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'listTagsForResourceResponse_resourceArn' - Lists all tags associated with the given Amazon Resource Name (ARN).
--
-- 'tags', 'listTagsForResourceResponse_tags' - Lists all tags associated with the given transcription job, vocabulary,
-- or resource.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { resourceArn =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists all tags associated with the given Amazon Resource Name (ARN).
listTagsForResourceResponse_resourceArn :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe Prelude.Text)
listTagsForResourceResponse_resourceArn = Lens.lens (\ListTagsForResourceResponse' {resourceArn} -> resourceArn) (\s@ListTagsForResourceResponse' {} a -> s {resourceArn = a} :: ListTagsForResourceResponse)

-- | Lists all tags associated with the given transcription job, vocabulary,
-- or resource.
listTagsForResourceResponse_tags :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe (Prelude.NonEmpty Tag))
listTagsForResourceResponse_tags = Lens.lens (\ListTagsForResourceResponse' {tags} -> tags) (\s@ListTagsForResourceResponse' {} a -> s {tags = a} :: ListTagsForResourceResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse

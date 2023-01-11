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
-- Module      : Amazonka.ConnectCases.GetLayout
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details for the requested layout.
module Amazonka.ConnectCases.GetLayout
  ( -- * Creating a Request
    GetLayout (..),
    newGetLayout,

    -- * Request Lenses
    getLayout_domainId,
    getLayout_layoutId,

    -- * Destructuring the Response
    GetLayoutResponse (..),
    newGetLayoutResponse,

    -- * Response Lenses
    getLayoutResponse_tags,
    getLayoutResponse_httpStatus,
    getLayoutResponse_content,
    getLayoutResponse_layoutArn,
    getLayoutResponse_layoutId,
    getLayoutResponse_name,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLayout' smart constructor.
data GetLayout = GetLayout'
  { -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The unique identifier of the layout.
    layoutId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLayout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'getLayout_domainId' - The unique identifier of the Cases domain.
--
-- 'layoutId', 'getLayout_layoutId' - The unique identifier of the layout.
newGetLayout ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'layoutId'
  Prelude.Text ->
  GetLayout
newGetLayout pDomainId_ pLayoutId_ =
  GetLayout'
    { domainId = pDomainId_,
      layoutId = pLayoutId_
    }

-- | The unique identifier of the Cases domain.
getLayout_domainId :: Lens.Lens' GetLayout Prelude.Text
getLayout_domainId = Lens.lens (\GetLayout' {domainId} -> domainId) (\s@GetLayout' {} a -> s {domainId = a} :: GetLayout)

-- | The unique identifier of the layout.
getLayout_layoutId :: Lens.Lens' GetLayout Prelude.Text
getLayout_layoutId = Lens.lens (\GetLayout' {layoutId} -> layoutId) (\s@GetLayout' {} a -> s {layoutId = a} :: GetLayout)

instance Core.AWSRequest GetLayout where
  type AWSResponse GetLayout = GetLayoutResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLayoutResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "content")
            Prelude.<*> (x Data..:> "layoutArn")
            Prelude.<*> (x Data..:> "layoutId")
            Prelude.<*> (x Data..:> "name")
      )

instance Prelude.Hashable GetLayout where
  hashWithSalt _salt GetLayout' {..} =
    _salt `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` layoutId

instance Prelude.NFData GetLayout where
  rnf GetLayout' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf layoutId

instance Data.ToHeaders GetLayout where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLayout where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetLayout where
  toPath GetLayout' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/layouts/",
        Data.toBS layoutId
      ]

instance Data.ToQuery GetLayout where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLayoutResponse' smart constructor.
data GetLayoutResponse = GetLayoutResponse'
  { -- | A map of of key-value pairs that represent tags on a resource. Tags are
    -- used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about which fields will be present in the layout, the order
    -- of the fields, and read-only attribute of the field.
    content :: LayoutContent,
    -- | The Amazon Resource Name (ARN) of the newly created layout.
    layoutArn :: Prelude.Text,
    -- | The unique identifier of the layout.
    layoutId :: Prelude.Text,
    -- | The name of the layout. It must be unique.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLayoutResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getLayoutResponse_tags' - A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'getLayoutResponse_httpStatus' - The response's http status code.
--
-- 'content', 'getLayoutResponse_content' - Information about which fields will be present in the layout, the order
-- of the fields, and read-only attribute of the field.
--
-- 'layoutArn', 'getLayoutResponse_layoutArn' - The Amazon Resource Name (ARN) of the newly created layout.
--
-- 'layoutId', 'getLayoutResponse_layoutId' - The unique identifier of the layout.
--
-- 'name', 'getLayoutResponse_name' - The name of the layout. It must be unique.
newGetLayoutResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'content'
  LayoutContent ->
  -- | 'layoutArn'
  Prelude.Text ->
  -- | 'layoutId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GetLayoutResponse
newGetLayoutResponse
  pHttpStatus_
  pContent_
  pLayoutArn_
  pLayoutId_
  pName_ =
    GetLayoutResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        content = pContent_,
        layoutArn = pLayoutArn_,
        layoutId = pLayoutId_,
        name = pName_
      }

-- | A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
getLayoutResponse_tags :: Lens.Lens' GetLayoutResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getLayoutResponse_tags = Lens.lens (\GetLayoutResponse' {tags} -> tags) (\s@GetLayoutResponse' {} a -> s {tags = a} :: GetLayoutResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLayoutResponse_httpStatus :: Lens.Lens' GetLayoutResponse Prelude.Int
getLayoutResponse_httpStatus = Lens.lens (\GetLayoutResponse' {httpStatus} -> httpStatus) (\s@GetLayoutResponse' {} a -> s {httpStatus = a} :: GetLayoutResponse)

-- | Information about which fields will be present in the layout, the order
-- of the fields, and read-only attribute of the field.
getLayoutResponse_content :: Lens.Lens' GetLayoutResponse LayoutContent
getLayoutResponse_content = Lens.lens (\GetLayoutResponse' {content} -> content) (\s@GetLayoutResponse' {} a -> s {content = a} :: GetLayoutResponse)

-- | The Amazon Resource Name (ARN) of the newly created layout.
getLayoutResponse_layoutArn :: Lens.Lens' GetLayoutResponse Prelude.Text
getLayoutResponse_layoutArn = Lens.lens (\GetLayoutResponse' {layoutArn} -> layoutArn) (\s@GetLayoutResponse' {} a -> s {layoutArn = a} :: GetLayoutResponse)

-- | The unique identifier of the layout.
getLayoutResponse_layoutId :: Lens.Lens' GetLayoutResponse Prelude.Text
getLayoutResponse_layoutId = Lens.lens (\GetLayoutResponse' {layoutId} -> layoutId) (\s@GetLayoutResponse' {} a -> s {layoutId = a} :: GetLayoutResponse)

-- | The name of the layout. It must be unique.
getLayoutResponse_name :: Lens.Lens' GetLayoutResponse Prelude.Text
getLayoutResponse_name = Lens.lens (\GetLayoutResponse' {name} -> name) (\s@GetLayoutResponse' {} a -> s {name = a} :: GetLayoutResponse)

instance Prelude.NFData GetLayoutResponse where
  rnf GetLayoutResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf layoutArn
      `Prelude.seq` Prelude.rnf layoutId
      `Prelude.seq` Prelude.rnf name

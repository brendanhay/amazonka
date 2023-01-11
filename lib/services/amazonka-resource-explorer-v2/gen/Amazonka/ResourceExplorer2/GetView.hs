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
-- Module      : Amazonka.ResourceExplorer2.GetView
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of the specified view.
module Amazonka.ResourceExplorer2.GetView
  ( -- * Creating a Request
    GetView (..),
    newGetView,

    -- * Request Lenses
    getView_viewArn,

    -- * Destructuring the Response
    GetViewResponse (..),
    newGetViewResponse,

    -- * Response Lenses
    getViewResponse_tags,
    getViewResponse_view,
    getViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetView' smart constructor.
data GetView = GetView'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view that you want information about.
    viewArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewArn', 'getView_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that you want information about.
newGetView ::
  -- | 'viewArn'
  Prelude.Text ->
  GetView
newGetView pViewArn_ = GetView' {viewArn = pViewArn_}

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that you want information about.
getView_viewArn :: Lens.Lens' GetView Prelude.Text
getView_viewArn = Lens.lens (\GetView' {viewArn} -> viewArn) (\s@GetView' {} a -> s {viewArn = a} :: GetView)

instance Core.AWSRequest GetView where
  type AWSResponse GetView = GetViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetViewResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "View")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetView where
  hashWithSalt _salt GetView' {..} =
    _salt `Prelude.hashWithSalt` viewArn

instance Prelude.NFData GetView where
  rnf GetView' {..} = Prelude.rnf viewArn

instance Data.ToHeaders GetView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetView where
  toJSON GetView' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ViewArn" Data..= viewArn)]
      )

instance Data.ToPath GetView where
  toPath = Prelude.const "/GetView"

instance Data.ToQuery GetView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetViewResponse' smart constructor.
data GetViewResponse = GetViewResponse'
  { -- | Tag key and value pairs that are attached to the view.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A structure that contains the details for the requested view.
    view :: Prelude.Maybe View,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getViewResponse_tags' - Tag key and value pairs that are attached to the view.
--
-- 'view', 'getViewResponse_view' - A structure that contains the details for the requested view.
--
-- 'httpStatus', 'getViewResponse_httpStatus' - The response's http status code.
newGetViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetViewResponse
newGetViewResponse pHttpStatus_ =
  GetViewResponse'
    { tags = Prelude.Nothing,
      view = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Tag key and value pairs that are attached to the view.
getViewResponse_tags :: Lens.Lens' GetViewResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getViewResponse_tags = Lens.lens (\GetViewResponse' {tags} -> tags) (\s@GetViewResponse' {} a -> s {tags = a} :: GetViewResponse) Prelude.. Lens.mapping Lens.coerced

-- | A structure that contains the details for the requested view.
getViewResponse_view :: Lens.Lens' GetViewResponse (Prelude.Maybe View)
getViewResponse_view = Lens.lens (\GetViewResponse' {view} -> view) (\s@GetViewResponse' {} a -> s {view = a} :: GetViewResponse)

-- | The response's http status code.
getViewResponse_httpStatus :: Lens.Lens' GetViewResponse Prelude.Int
getViewResponse_httpStatus = Lens.lens (\GetViewResponse' {httpStatus} -> httpStatus) (\s@GetViewResponse' {} a -> s {httpStatus = a} :: GetViewResponse)

instance Prelude.NFData GetViewResponse where
  rnf GetViewResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf view
      `Prelude.seq` Prelude.rnf httpStatus

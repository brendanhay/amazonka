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
-- Module      : Amazonka.ResourceExplorer2.CreateView
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a view that users can query by using the Search operation.
-- Results from queries that you make using this view include only
-- resources that match the view\'s @Filters@. For more information about
-- Amazon Web Services Resource Explorer views, see
-- <https://docs.aws.amazon.com/arexug/mainline/manage-views.html Managing views>
-- in the /Amazon Web Services Resource Explorer User Guide/.
--
-- Only the principals with an IAM identity-based policy that grants
-- @Allow@ to the @Search@ action on a @Resource@ with the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of this view can Search using views you create with this operation.
module Amazonka.ResourceExplorer2.CreateView
  ( -- * Creating a Request
    CreateView (..),
    newCreateView,

    -- * Request Lenses
    createView_tags,
    createView_clientToken,
    createView_filters,
    createView_includedProperties,
    createView_viewName,

    -- * Destructuring the Response
    CreateViewResponse (..),
    newCreateViewResponse,

    -- * Response Lenses
    createViewResponse_view,
    createViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateView' smart constructor.
data CreateView = CreateView'
  { -- | Tag key and value pairs that are attached to the view.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | This value helps ensure idempotency. Resource Explorer uses this value
    -- to prevent the accidental creation of duplicate versions. We recommend
    -- that you generate a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type value>
    -- to ensure the uniqueness of your views.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that specify which resources are included in the
    -- results of queries made using this view. When you use this view in a
    -- Search operation, the filter string is combined with the search\'s
    -- @QueryString@ parameter using a logical @AND@ operator.
    --
    -- For information about the supported syntax, see
    -- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html Search query reference for Resource Explorer>
    -- in the /Amazon Web Services Resource Explorer User Guide/.
    --
    -- This query string in the context of this operation supports only
    -- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html#query-syntax-filters filter prefixes>
    -- with optional
    -- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html#query-syntax-operators operators>.
    -- It doesn\'t support free-form text. For example, the string
    -- @region:us* service:ec2 -tag:stage=prod@ includes all Amazon EC2
    -- resources in any Amazon Web Services Region that begins with the letters
    -- @us@ and is /not/ tagged with a key @Stage@ that has the value @prod@.
    filters :: Prelude.Maybe (Core.Sensitive SearchFilter),
    -- | Specifies optional fields that you want included in search results from
    -- this view. It is a list of objects that each describe a field to
    -- include.
    --
    -- The default is an empty list, with no optional fields included in the
    -- results.
    includedProperties :: Prelude.Maybe [IncludedProperty],
    -- | The name of the new view. This name appears in the list of views in
    -- Resource Explorer.
    --
    -- The name must be no more than 64 characters long, and can include
    -- letters, digits, and the dash (-) character. The name must be unique
    -- within its Amazon Web Services Region.
    viewName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createView_tags' - Tag key and value pairs that are attached to the view.
--
-- 'clientToken', 'createView_clientToken' - This value helps ensure idempotency. Resource Explorer uses this value
-- to prevent the accidental creation of duplicate versions. We recommend
-- that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type value>
-- to ensure the uniqueness of your views.
--
-- 'filters', 'createView_filters' - An array of strings that specify which resources are included in the
-- results of queries made using this view. When you use this view in a
-- Search operation, the filter string is combined with the search\'s
-- @QueryString@ parameter using a logical @AND@ operator.
--
-- For information about the supported syntax, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html Search query reference for Resource Explorer>
-- in the /Amazon Web Services Resource Explorer User Guide/.
--
-- This query string in the context of this operation supports only
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html#query-syntax-filters filter prefixes>
-- with optional
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html#query-syntax-operators operators>.
-- It doesn\'t support free-form text. For example, the string
-- @region:us* service:ec2 -tag:stage=prod@ includes all Amazon EC2
-- resources in any Amazon Web Services Region that begins with the letters
-- @us@ and is /not/ tagged with a key @Stage@ that has the value @prod@.
--
-- 'includedProperties', 'createView_includedProperties' - Specifies optional fields that you want included in search results from
-- this view. It is a list of objects that each describe a field to
-- include.
--
-- The default is an empty list, with no optional fields included in the
-- results.
--
-- 'viewName', 'createView_viewName' - The name of the new view. This name appears in the list of views in
-- Resource Explorer.
--
-- The name must be no more than 64 characters long, and can include
-- letters, digits, and the dash (-) character. The name must be unique
-- within its Amazon Web Services Region.
newCreateView ::
  -- | 'viewName'
  Prelude.Text ->
  CreateView
newCreateView pViewName_ =
  CreateView'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      includedProperties = Prelude.Nothing,
      viewName = pViewName_
    }

-- | Tag key and value pairs that are attached to the view.
createView_tags :: Lens.Lens' CreateView (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createView_tags = Lens.lens (\CreateView' {tags} -> tags) (\s@CreateView' {} a -> s {tags = a} :: CreateView) Prelude.. Lens.mapping Lens.coerced

-- | This value helps ensure idempotency. Resource Explorer uses this value
-- to prevent the accidental creation of duplicate versions. We recommend
-- that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type value>
-- to ensure the uniqueness of your views.
createView_clientToken :: Lens.Lens' CreateView (Prelude.Maybe Prelude.Text)
createView_clientToken = Lens.lens (\CreateView' {clientToken} -> clientToken) (\s@CreateView' {} a -> s {clientToken = a} :: CreateView)

-- | An array of strings that specify which resources are included in the
-- results of queries made using this view. When you use this view in a
-- Search operation, the filter string is combined with the search\'s
-- @QueryString@ parameter using a logical @AND@ operator.
--
-- For information about the supported syntax, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html Search query reference for Resource Explorer>
-- in the /Amazon Web Services Resource Explorer User Guide/.
--
-- This query string in the context of this operation supports only
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html#query-syntax-filters filter prefixes>
-- with optional
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/using-search-query-syntax.html#query-syntax-operators operators>.
-- It doesn\'t support free-form text. For example, the string
-- @region:us* service:ec2 -tag:stage=prod@ includes all Amazon EC2
-- resources in any Amazon Web Services Region that begins with the letters
-- @us@ and is /not/ tagged with a key @Stage@ that has the value @prod@.
createView_filters :: Lens.Lens' CreateView (Prelude.Maybe SearchFilter)
createView_filters = Lens.lens (\CreateView' {filters} -> filters) (\s@CreateView' {} a -> s {filters = a} :: CreateView) Prelude.. Lens.mapping Core._Sensitive

-- | Specifies optional fields that you want included in search results from
-- this view. It is a list of objects that each describe a field to
-- include.
--
-- The default is an empty list, with no optional fields included in the
-- results.
createView_includedProperties :: Lens.Lens' CreateView (Prelude.Maybe [IncludedProperty])
createView_includedProperties = Lens.lens (\CreateView' {includedProperties} -> includedProperties) (\s@CreateView' {} a -> s {includedProperties = a} :: CreateView) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new view. This name appears in the list of views in
-- Resource Explorer.
--
-- The name must be no more than 64 characters long, and can include
-- letters, digits, and the dash (-) character. The name must be unique
-- within its Amazon Web Services Region.
createView_viewName :: Lens.Lens' CreateView Prelude.Text
createView_viewName = Lens.lens (\CreateView' {viewName} -> viewName) (\s@CreateView' {} a -> s {viewName = a} :: CreateView)

instance Core.AWSRequest CreateView where
  type AWSResponse CreateView = CreateViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateViewResponse'
            Prelude.<$> (x Core..?> "View")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateView where
  hashWithSalt _salt CreateView' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` includedProperties
      `Prelude.hashWithSalt` viewName

instance Prelude.NFData CreateView where
  rnf CreateView' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includedProperties
      `Prelude.seq` Prelude.rnf viewName

instance Core.ToHeaders CreateView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateView where
  toJSON CreateView' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("IncludedProperties" Core..=)
              Prelude.<$> includedProperties,
            Prelude.Just ("ViewName" Core..= viewName)
          ]
      )

instance Core.ToPath CreateView where
  toPath = Prelude.const "/CreateView"

instance Core.ToQuery CreateView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateViewResponse' smart constructor.
data CreateViewResponse = CreateViewResponse'
  { -- | A structure that contains the details about the new view.
    view :: Prelude.Maybe View,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'view', 'createViewResponse_view' - A structure that contains the details about the new view.
--
-- 'httpStatus', 'createViewResponse_httpStatus' - The response's http status code.
newCreateViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateViewResponse
newCreateViewResponse pHttpStatus_ =
  CreateViewResponse'
    { view = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains the details about the new view.
createViewResponse_view :: Lens.Lens' CreateViewResponse (Prelude.Maybe View)
createViewResponse_view = Lens.lens (\CreateViewResponse' {view} -> view) (\s@CreateViewResponse' {} a -> s {view = a} :: CreateViewResponse)

-- | The response's http status code.
createViewResponse_httpStatus :: Lens.Lens' CreateViewResponse Prelude.Int
createViewResponse_httpStatus = Lens.lens (\CreateViewResponse' {httpStatus} -> httpStatus) (\s@CreateViewResponse' {} a -> s {httpStatus = a} :: CreateViewResponse)

instance Prelude.NFData CreateViewResponse where
  rnf CreateViewResponse' {..} =
    Prelude.rnf view
      `Prelude.seq` Prelude.rnf httpStatus

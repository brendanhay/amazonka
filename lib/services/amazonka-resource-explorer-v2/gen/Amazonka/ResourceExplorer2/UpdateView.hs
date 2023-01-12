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
-- Module      : Amazonka.ResourceExplorer2.UpdateView
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies some of the details of a view. You can change the filter string
-- and the list of included properties. You can\'t change the name of the
-- view.
module Amazonka.ResourceExplorer2.UpdateView
  ( -- * Creating a Request
    UpdateView (..),
    newUpdateView,

    -- * Request Lenses
    updateView_filters,
    updateView_includedProperties,
    updateView_viewArn,

    -- * Destructuring the Response
    UpdateViewResponse (..),
    newUpdateViewResponse,

    -- * Response Lenses
    updateViewResponse_view,
    updateViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateView' smart constructor.
data UpdateView = UpdateView'
  { -- | An array of strings that specify which resources are included in the
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
    filters :: Prelude.Maybe (Data.Sensitive SearchFilter),
    -- | Specifies optional fields that you want included in search results from
    -- this view. It is a list of objects that each describe a field to
    -- include.
    --
    -- The default is an empty list, with no optional fields included in the
    -- results.
    includedProperties :: Prelude.Maybe [IncludedProperty],
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view that you want to modify.
    viewArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'updateView_filters' - An array of strings that specify which resources are included in the
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
-- 'includedProperties', 'updateView_includedProperties' - Specifies optional fields that you want included in search results from
-- this view. It is a list of objects that each describe a field to
-- include.
--
-- The default is an empty list, with no optional fields included in the
-- results.
--
-- 'viewArn', 'updateView_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that you want to modify.
newUpdateView ::
  -- | 'viewArn'
  Prelude.Text ->
  UpdateView
newUpdateView pViewArn_ =
  UpdateView'
    { filters = Prelude.Nothing,
      includedProperties = Prelude.Nothing,
      viewArn = pViewArn_
    }

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
updateView_filters :: Lens.Lens' UpdateView (Prelude.Maybe SearchFilter)
updateView_filters = Lens.lens (\UpdateView' {filters} -> filters) (\s@UpdateView' {} a -> s {filters = a} :: UpdateView) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies optional fields that you want included in search results from
-- this view. It is a list of objects that each describe a field to
-- include.
--
-- The default is an empty list, with no optional fields included in the
-- results.
updateView_includedProperties :: Lens.Lens' UpdateView (Prelude.Maybe [IncludedProperty])
updateView_includedProperties = Lens.lens (\UpdateView' {includedProperties} -> includedProperties) (\s@UpdateView' {} a -> s {includedProperties = a} :: UpdateView) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view that you want to modify.
updateView_viewArn :: Lens.Lens' UpdateView Prelude.Text
updateView_viewArn = Lens.lens (\UpdateView' {viewArn} -> viewArn) (\s@UpdateView' {} a -> s {viewArn = a} :: UpdateView)

instance Core.AWSRequest UpdateView where
  type AWSResponse UpdateView = UpdateViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateViewResponse'
            Prelude.<$> (x Data..?> "View")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateView where
  hashWithSalt _salt UpdateView' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` includedProperties
      `Prelude.hashWithSalt` viewArn

instance Prelude.NFData UpdateView where
  rnf UpdateView' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf includedProperties
      `Prelude.seq` Prelude.rnf viewArn

instance Data.ToHeaders UpdateView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateView where
  toJSON UpdateView' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("IncludedProperties" Data..=)
              Prelude.<$> includedProperties,
            Prelude.Just ("ViewArn" Data..= viewArn)
          ]
      )

instance Data.ToPath UpdateView where
  toPath = Prelude.const "/UpdateView"

instance Data.ToQuery UpdateView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateViewResponse' smart constructor.
data UpdateViewResponse = UpdateViewResponse'
  { -- | Details about the view that you changed with this operation.
    view :: Prelude.Maybe View,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'view', 'updateViewResponse_view' - Details about the view that you changed with this operation.
--
-- 'httpStatus', 'updateViewResponse_httpStatus' - The response's http status code.
newUpdateViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateViewResponse
newUpdateViewResponse pHttpStatus_ =
  UpdateViewResponse'
    { view = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the view that you changed with this operation.
updateViewResponse_view :: Lens.Lens' UpdateViewResponse (Prelude.Maybe View)
updateViewResponse_view = Lens.lens (\UpdateViewResponse' {view} -> view) (\s@UpdateViewResponse' {} a -> s {view = a} :: UpdateViewResponse)

-- | The response's http status code.
updateViewResponse_httpStatus :: Lens.Lens' UpdateViewResponse Prelude.Int
updateViewResponse_httpStatus = Lens.lens (\UpdateViewResponse' {httpStatus} -> httpStatus) (\s@UpdateViewResponse' {} a -> s {httpStatus = a} :: UpdateViewResponse)

instance Prelude.NFData UpdateViewResponse where
  rnf UpdateViewResponse' {..} =
    Prelude.rnf view
      `Prelude.seq` Prelude.rnf httpStatus

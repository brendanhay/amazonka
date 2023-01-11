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
-- Module      : Amazonka.ResourceExplorer2.BatchGetView
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about a list of views.
module Amazonka.ResourceExplorer2.BatchGetView
  ( -- * Creating a Request
    BatchGetView (..),
    newBatchGetView,

    -- * Request Lenses
    batchGetView_viewArns,

    -- * Destructuring the Response
    BatchGetViewResponse (..),
    newBatchGetViewResponse,

    -- * Response Lenses
    batchGetViewResponse_errors,
    batchGetViewResponse_views,
    batchGetViewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetView' smart constructor.
data BatchGetView = BatchGetView'
  { -- | A list of
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource names (ARNs)>
    -- that identify the views you want details for.
    viewArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'viewArns', 'batchGetView_viewArns' - A list of
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource names (ARNs)>
-- that identify the views you want details for.
newBatchGetView ::
  BatchGetView
newBatchGetView =
  BatchGetView' {viewArns = Prelude.Nothing}

-- | A list of
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource names (ARNs)>
-- that identify the views you want details for.
batchGetView_viewArns :: Lens.Lens' BatchGetView (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetView_viewArns = Lens.lens (\BatchGetView' {viewArns} -> viewArns) (\s@BatchGetView' {} a -> s {viewArns = a} :: BatchGetView) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest BatchGetView where
  type AWSResponse BatchGetView = BatchGetViewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetViewResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Views" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetView where
  hashWithSalt _salt BatchGetView' {..} =
    _salt `Prelude.hashWithSalt` viewArns

instance Prelude.NFData BatchGetView where
  rnf BatchGetView' {..} = Prelude.rnf viewArns

instance Data.ToHeaders BatchGetView where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetView where
  toJSON BatchGetView' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ViewArns" Data..=) Prelude.<$> viewArns]
      )

instance Data.ToPath BatchGetView where
  toPath = Prelude.const "/BatchGetView"

instance Data.ToQuery BatchGetView where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetViewResponse' smart constructor.
data BatchGetViewResponse = BatchGetViewResponse'
  { -- | If any of the specified ARNs result in an error, then this structure
    -- describes the error.
    errors :: Prelude.Maybe [BatchGetViewError],
    -- | A structure with a list of objects with details for each of the
    -- specified views.
    views :: Prelude.Maybe [View],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchGetViewResponse_errors' - If any of the specified ARNs result in an error, then this structure
-- describes the error.
--
-- 'views', 'batchGetViewResponse_views' - A structure with a list of objects with details for each of the
-- specified views.
--
-- 'httpStatus', 'batchGetViewResponse_httpStatus' - The response's http status code.
newBatchGetViewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetViewResponse
newBatchGetViewResponse pHttpStatus_ =
  BatchGetViewResponse'
    { errors = Prelude.Nothing,
      views = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If any of the specified ARNs result in an error, then this structure
-- describes the error.
batchGetViewResponse_errors :: Lens.Lens' BatchGetViewResponse (Prelude.Maybe [BatchGetViewError])
batchGetViewResponse_errors = Lens.lens (\BatchGetViewResponse' {errors} -> errors) (\s@BatchGetViewResponse' {} a -> s {errors = a} :: BatchGetViewResponse) Prelude.. Lens.mapping Lens.coerced

-- | A structure with a list of objects with details for each of the
-- specified views.
batchGetViewResponse_views :: Lens.Lens' BatchGetViewResponse (Prelude.Maybe [View])
batchGetViewResponse_views = Lens.lens (\BatchGetViewResponse' {views} -> views) (\s@BatchGetViewResponse' {} a -> s {views = a} :: BatchGetViewResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetViewResponse_httpStatus :: Lens.Lens' BatchGetViewResponse Prelude.Int
batchGetViewResponse_httpStatus = Lens.lens (\BatchGetViewResponse' {httpStatus} -> httpStatus) (\s@BatchGetViewResponse' {} a -> s {httpStatus = a} :: BatchGetViewResponse)

instance Prelude.NFData BatchGetViewResponse where
  rnf BatchGetViewResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf views
      `Prelude.seq` Prelude.rnf httpStatus

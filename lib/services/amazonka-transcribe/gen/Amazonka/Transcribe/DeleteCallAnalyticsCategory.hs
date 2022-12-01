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
-- Module      : Amazonka.Transcribe.DeleteCallAnalyticsCategory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Call Analytics category. To use this operation, specify the
-- name of the category you want to delete using @CategoryName@. Category
-- names are case sensitive.
module Amazonka.Transcribe.DeleteCallAnalyticsCategory
  ( -- * Creating a Request
    DeleteCallAnalyticsCategory (..),
    newDeleteCallAnalyticsCategory,

    -- * Request Lenses
    deleteCallAnalyticsCategory_categoryName,

    -- * Destructuring the Response
    DeleteCallAnalyticsCategoryResponse (..),
    newDeleteCallAnalyticsCategoryResponse,

    -- * Response Lenses
    deleteCallAnalyticsCategoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newDeleteCallAnalyticsCategory' smart constructor.
data DeleteCallAnalyticsCategory = DeleteCallAnalyticsCategory'
  { -- | The name of the Call Analytics category you want to delete. Category
    -- names are case sensitive.
    categoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCallAnalyticsCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryName', 'deleteCallAnalyticsCategory_categoryName' - The name of the Call Analytics category you want to delete. Category
-- names are case sensitive.
newDeleteCallAnalyticsCategory ::
  -- | 'categoryName'
  Prelude.Text ->
  DeleteCallAnalyticsCategory
newDeleteCallAnalyticsCategory pCategoryName_ =
  DeleteCallAnalyticsCategory'
    { categoryName =
        pCategoryName_
    }

-- | The name of the Call Analytics category you want to delete. Category
-- names are case sensitive.
deleteCallAnalyticsCategory_categoryName :: Lens.Lens' DeleteCallAnalyticsCategory Prelude.Text
deleteCallAnalyticsCategory_categoryName = Lens.lens (\DeleteCallAnalyticsCategory' {categoryName} -> categoryName) (\s@DeleteCallAnalyticsCategory' {} a -> s {categoryName = a} :: DeleteCallAnalyticsCategory)

instance Core.AWSRequest DeleteCallAnalyticsCategory where
  type
    AWSResponse DeleteCallAnalyticsCategory =
      DeleteCallAnalyticsCategoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCallAnalyticsCategoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCallAnalyticsCategory where
  hashWithSalt _salt DeleteCallAnalyticsCategory' {..} =
    _salt `Prelude.hashWithSalt` categoryName

instance Prelude.NFData DeleteCallAnalyticsCategory where
  rnf DeleteCallAnalyticsCategory' {..} =
    Prelude.rnf categoryName

instance Core.ToHeaders DeleteCallAnalyticsCategory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DeleteCallAnalyticsCategory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteCallAnalyticsCategory where
  toJSON DeleteCallAnalyticsCategory' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("CategoryName" Core..= categoryName)]
      )

instance Core.ToPath DeleteCallAnalyticsCategory where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCallAnalyticsCategory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCallAnalyticsCategoryResponse' smart constructor.
data DeleteCallAnalyticsCategoryResponse = DeleteCallAnalyticsCategoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCallAnalyticsCategoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCallAnalyticsCategoryResponse_httpStatus' - The response's http status code.
newDeleteCallAnalyticsCategoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCallAnalyticsCategoryResponse
newDeleteCallAnalyticsCategoryResponse pHttpStatus_ =
  DeleteCallAnalyticsCategoryResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCallAnalyticsCategoryResponse_httpStatus :: Lens.Lens' DeleteCallAnalyticsCategoryResponse Prelude.Int
deleteCallAnalyticsCategoryResponse_httpStatus = Lens.lens (\DeleteCallAnalyticsCategoryResponse' {httpStatus} -> httpStatus) (\s@DeleteCallAnalyticsCategoryResponse' {} a -> s {httpStatus = a} :: DeleteCallAnalyticsCategoryResponse)

instance
  Prelude.NFData
    DeleteCallAnalyticsCategoryResponse
  where
  rnf DeleteCallAnalyticsCategoryResponse' {..} =
    Prelude.rnf httpStatus

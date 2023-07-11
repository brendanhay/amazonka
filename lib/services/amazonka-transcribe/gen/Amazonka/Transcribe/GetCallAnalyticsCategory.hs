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
-- Module      : Amazonka.Transcribe.GetCallAnalyticsCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the specified Call Analytics category.
--
-- To get a list of your Call Analytics categories, use the operation.
module Amazonka.Transcribe.GetCallAnalyticsCategory
  ( -- * Creating a Request
    GetCallAnalyticsCategory (..),
    newGetCallAnalyticsCategory,

    -- * Request Lenses
    getCallAnalyticsCategory_categoryName,

    -- * Destructuring the Response
    GetCallAnalyticsCategoryResponse (..),
    newGetCallAnalyticsCategoryResponse,

    -- * Response Lenses
    getCallAnalyticsCategoryResponse_categoryProperties,
    getCallAnalyticsCategoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newGetCallAnalyticsCategory' smart constructor.
data GetCallAnalyticsCategory = GetCallAnalyticsCategory'
  { -- | The name of the Call Analytics category you want information about.
    -- Category names are case sensitive.
    categoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCallAnalyticsCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryName', 'getCallAnalyticsCategory_categoryName' - The name of the Call Analytics category you want information about.
-- Category names are case sensitive.
newGetCallAnalyticsCategory ::
  -- | 'categoryName'
  Prelude.Text ->
  GetCallAnalyticsCategory
newGetCallAnalyticsCategory pCategoryName_ =
  GetCallAnalyticsCategory'
    { categoryName =
        pCategoryName_
    }

-- | The name of the Call Analytics category you want information about.
-- Category names are case sensitive.
getCallAnalyticsCategory_categoryName :: Lens.Lens' GetCallAnalyticsCategory Prelude.Text
getCallAnalyticsCategory_categoryName = Lens.lens (\GetCallAnalyticsCategory' {categoryName} -> categoryName) (\s@GetCallAnalyticsCategory' {} a -> s {categoryName = a} :: GetCallAnalyticsCategory)

instance Core.AWSRequest GetCallAnalyticsCategory where
  type
    AWSResponse GetCallAnalyticsCategory =
      GetCallAnalyticsCategoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCallAnalyticsCategoryResponse'
            Prelude.<$> (x Data..?> "CategoryProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCallAnalyticsCategory where
  hashWithSalt _salt GetCallAnalyticsCategory' {..} =
    _salt `Prelude.hashWithSalt` categoryName

instance Prelude.NFData GetCallAnalyticsCategory where
  rnf GetCallAnalyticsCategory' {..} =
    Prelude.rnf categoryName

instance Data.ToHeaders GetCallAnalyticsCategory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.GetCallAnalyticsCategory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCallAnalyticsCategory where
  toJSON GetCallAnalyticsCategory' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("CategoryName" Data..= categoryName)]
      )

instance Data.ToPath GetCallAnalyticsCategory where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCallAnalyticsCategory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCallAnalyticsCategoryResponse' smart constructor.
data GetCallAnalyticsCategoryResponse = GetCallAnalyticsCategoryResponse'
  { -- | Provides you with the properties of the Call Analytics category you
    -- specified in your @GetCallAnalyticsCategory@ request.
    categoryProperties :: Prelude.Maybe CategoryProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCallAnalyticsCategoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryProperties', 'getCallAnalyticsCategoryResponse_categoryProperties' - Provides you with the properties of the Call Analytics category you
-- specified in your @GetCallAnalyticsCategory@ request.
--
-- 'httpStatus', 'getCallAnalyticsCategoryResponse_httpStatus' - The response's http status code.
newGetCallAnalyticsCategoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCallAnalyticsCategoryResponse
newGetCallAnalyticsCategoryResponse pHttpStatus_ =
  GetCallAnalyticsCategoryResponse'
    { categoryProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides you with the properties of the Call Analytics category you
-- specified in your @GetCallAnalyticsCategory@ request.
getCallAnalyticsCategoryResponse_categoryProperties :: Lens.Lens' GetCallAnalyticsCategoryResponse (Prelude.Maybe CategoryProperties)
getCallAnalyticsCategoryResponse_categoryProperties = Lens.lens (\GetCallAnalyticsCategoryResponse' {categoryProperties} -> categoryProperties) (\s@GetCallAnalyticsCategoryResponse' {} a -> s {categoryProperties = a} :: GetCallAnalyticsCategoryResponse)

-- | The response's http status code.
getCallAnalyticsCategoryResponse_httpStatus :: Lens.Lens' GetCallAnalyticsCategoryResponse Prelude.Int
getCallAnalyticsCategoryResponse_httpStatus = Lens.lens (\GetCallAnalyticsCategoryResponse' {httpStatus} -> httpStatus) (\s@GetCallAnalyticsCategoryResponse' {} a -> s {httpStatus = a} :: GetCallAnalyticsCategoryResponse)

instance
  Prelude.NFData
    GetCallAnalyticsCategoryResponse
  where
  rnf GetCallAnalyticsCategoryResponse' {..} =
    Prelude.rnf categoryProperties
      `Prelude.seq` Prelude.rnf httpStatus

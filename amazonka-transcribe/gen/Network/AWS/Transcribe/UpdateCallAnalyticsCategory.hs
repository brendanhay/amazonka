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
-- Module      : Network.AWS.Transcribe.UpdateCallAnalyticsCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the call analytics category with new values. The
-- @UpdateCallAnalyticsCategory@ operation overwrites all of the existing
-- information with the values that you provide in the request.
module Network.AWS.Transcribe.UpdateCallAnalyticsCategory
  ( -- * Creating a Request
    UpdateCallAnalyticsCategory (..),
    newUpdateCallAnalyticsCategory,

    -- * Request Lenses
    updateCallAnalyticsCategory_categoryName,
    updateCallAnalyticsCategory_rules,

    -- * Destructuring the Response
    UpdateCallAnalyticsCategoryResponse (..),
    newUpdateCallAnalyticsCategoryResponse,

    -- * Response Lenses
    updateCallAnalyticsCategoryResponse_categoryProperties,
    updateCallAnalyticsCategoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newUpdateCallAnalyticsCategory' smart constructor.
data UpdateCallAnalyticsCategory = UpdateCallAnalyticsCategory'
  { -- | The name of the analytics category to update. The name is case
    -- sensitive. If you try to update a call analytics category with the same
    -- name as a previous category you will receive a @ConflictException@
    -- error.
    categoryName :: Prelude.Text,
    -- | The rules used for the updated analytics category. The rules that you
    -- provide in this field replace the ones that are currently being used.
    rules :: Prelude.NonEmpty Rule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCallAnalyticsCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryName', 'updateCallAnalyticsCategory_categoryName' - The name of the analytics category to update. The name is case
-- sensitive. If you try to update a call analytics category with the same
-- name as a previous category you will receive a @ConflictException@
-- error.
--
-- 'rules', 'updateCallAnalyticsCategory_rules' - The rules used for the updated analytics category. The rules that you
-- provide in this field replace the ones that are currently being used.
newUpdateCallAnalyticsCategory ::
  -- | 'categoryName'
  Prelude.Text ->
  -- | 'rules'
  Prelude.NonEmpty Rule ->
  UpdateCallAnalyticsCategory
newUpdateCallAnalyticsCategory pCategoryName_ pRules_ =
  UpdateCallAnalyticsCategory'
    { categoryName =
        pCategoryName_,
      rules = Lens._Coerce Lens.# pRules_
    }

-- | The name of the analytics category to update. The name is case
-- sensitive. If you try to update a call analytics category with the same
-- name as a previous category you will receive a @ConflictException@
-- error.
updateCallAnalyticsCategory_categoryName :: Lens.Lens' UpdateCallAnalyticsCategory Prelude.Text
updateCallAnalyticsCategory_categoryName = Lens.lens (\UpdateCallAnalyticsCategory' {categoryName} -> categoryName) (\s@UpdateCallAnalyticsCategory' {} a -> s {categoryName = a} :: UpdateCallAnalyticsCategory)

-- | The rules used for the updated analytics category. The rules that you
-- provide in this field replace the ones that are currently being used.
updateCallAnalyticsCategory_rules :: Lens.Lens' UpdateCallAnalyticsCategory (Prelude.NonEmpty Rule)
updateCallAnalyticsCategory_rules = Lens.lens (\UpdateCallAnalyticsCategory' {rules} -> rules) (\s@UpdateCallAnalyticsCategory' {} a -> s {rules = a} :: UpdateCallAnalyticsCategory) Prelude.. Lens._Coerce

instance Core.AWSRequest UpdateCallAnalyticsCategory where
  type
    AWSResponse UpdateCallAnalyticsCategory =
      UpdateCallAnalyticsCategoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCallAnalyticsCategoryResponse'
            Prelude.<$> (x Core..?> "CategoryProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCallAnalyticsCategory

instance Prelude.NFData UpdateCallAnalyticsCategory

instance Core.ToHeaders UpdateCallAnalyticsCategory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.UpdateCallAnalyticsCategory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateCallAnalyticsCategory where
  toJSON UpdateCallAnalyticsCategory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CategoryName" Core..= categoryName),
            Prelude.Just ("Rules" Core..= rules)
          ]
      )

instance Core.ToPath UpdateCallAnalyticsCategory where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateCallAnalyticsCategory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCallAnalyticsCategoryResponse' smart constructor.
data UpdateCallAnalyticsCategoryResponse = UpdateCallAnalyticsCategoryResponse'
  { -- | The attributes describing the analytics category. You can see
    -- information such as the rules that you\'ve used to update the category
    -- and when the category was originally created.
    categoryProperties :: Prelude.Maybe CategoryProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCallAnalyticsCategoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryProperties', 'updateCallAnalyticsCategoryResponse_categoryProperties' - The attributes describing the analytics category. You can see
-- information such as the rules that you\'ve used to update the category
-- and when the category was originally created.
--
-- 'httpStatus', 'updateCallAnalyticsCategoryResponse_httpStatus' - The response's http status code.
newUpdateCallAnalyticsCategoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCallAnalyticsCategoryResponse
newUpdateCallAnalyticsCategoryResponse pHttpStatus_ =
  UpdateCallAnalyticsCategoryResponse'
    { categoryProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attributes describing the analytics category. You can see
-- information such as the rules that you\'ve used to update the category
-- and when the category was originally created.
updateCallAnalyticsCategoryResponse_categoryProperties :: Lens.Lens' UpdateCallAnalyticsCategoryResponse (Prelude.Maybe CategoryProperties)
updateCallAnalyticsCategoryResponse_categoryProperties = Lens.lens (\UpdateCallAnalyticsCategoryResponse' {categoryProperties} -> categoryProperties) (\s@UpdateCallAnalyticsCategoryResponse' {} a -> s {categoryProperties = a} :: UpdateCallAnalyticsCategoryResponse)

-- | The response's http status code.
updateCallAnalyticsCategoryResponse_httpStatus :: Lens.Lens' UpdateCallAnalyticsCategoryResponse Prelude.Int
updateCallAnalyticsCategoryResponse_httpStatus = Lens.lens (\UpdateCallAnalyticsCategoryResponse' {httpStatus} -> httpStatus) (\s@UpdateCallAnalyticsCategoryResponse' {} a -> s {httpStatus = a} :: UpdateCallAnalyticsCategoryResponse)

instance
  Prelude.NFData
    UpdateCallAnalyticsCategoryResponse

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
-- Module      : Amazonka.Transcribe.UpdateCallAnalyticsCategory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Call Analytics category with new rules. Note that
-- the @UpdateCallAnalyticsCategory@ operation overwrites all existing
-- rules contained in the specified category. You cannot append additional
-- rules onto an existing category.
--
-- To create a new category, see .
module Amazonka.Transcribe.UpdateCallAnalyticsCategory
  ( -- * Creating a Request
    UpdateCallAnalyticsCategory (..),
    newUpdateCallAnalyticsCategory,

    -- * Request Lenses
    updateCallAnalyticsCategory_inputType,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newUpdateCallAnalyticsCategory' smart constructor.
data UpdateCallAnalyticsCategory = UpdateCallAnalyticsCategory'
  { -- | Choose whether you want to update a streaming or a batch Call Analytics
    -- category. The input type you specify must match the input type specified
    -- when the category was created. For example, if you created a category
    -- with the @POST_CALL@ input type, you must use @POST_CALL@ as the input
    -- type when updating this category.
    inputType :: Prelude.Maybe InputType,
    -- | The name of the Call Analytics category you want to update. Category
    -- names are case sensitive.
    categoryName :: Prelude.Text,
    -- | The rules used for the updated Call Analytics category. The rules you
    -- provide in this field replace the ones that are currently being used in
    -- the specified category.
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
-- 'inputType', 'updateCallAnalyticsCategory_inputType' - Choose whether you want to update a streaming or a batch Call Analytics
-- category. The input type you specify must match the input type specified
-- when the category was created. For example, if you created a category
-- with the @POST_CALL@ input type, you must use @POST_CALL@ as the input
-- type when updating this category.
--
-- 'categoryName', 'updateCallAnalyticsCategory_categoryName' - The name of the Call Analytics category you want to update. Category
-- names are case sensitive.
--
-- 'rules', 'updateCallAnalyticsCategory_rules' - The rules used for the updated Call Analytics category. The rules you
-- provide in this field replace the ones that are currently being used in
-- the specified category.
newUpdateCallAnalyticsCategory ::
  -- | 'categoryName'
  Prelude.Text ->
  -- | 'rules'
  Prelude.NonEmpty Rule ->
  UpdateCallAnalyticsCategory
newUpdateCallAnalyticsCategory pCategoryName_ pRules_ =
  UpdateCallAnalyticsCategory'
    { inputType =
        Prelude.Nothing,
      categoryName = pCategoryName_,
      rules = Lens.coerced Lens.# pRules_
    }

-- | Choose whether you want to update a streaming or a batch Call Analytics
-- category. The input type you specify must match the input type specified
-- when the category was created. For example, if you created a category
-- with the @POST_CALL@ input type, you must use @POST_CALL@ as the input
-- type when updating this category.
updateCallAnalyticsCategory_inputType :: Lens.Lens' UpdateCallAnalyticsCategory (Prelude.Maybe InputType)
updateCallAnalyticsCategory_inputType = Lens.lens (\UpdateCallAnalyticsCategory' {inputType} -> inputType) (\s@UpdateCallAnalyticsCategory' {} a -> s {inputType = a} :: UpdateCallAnalyticsCategory)

-- | The name of the Call Analytics category you want to update. Category
-- names are case sensitive.
updateCallAnalyticsCategory_categoryName :: Lens.Lens' UpdateCallAnalyticsCategory Prelude.Text
updateCallAnalyticsCategory_categoryName = Lens.lens (\UpdateCallAnalyticsCategory' {categoryName} -> categoryName) (\s@UpdateCallAnalyticsCategory' {} a -> s {categoryName = a} :: UpdateCallAnalyticsCategory)

-- | The rules used for the updated Call Analytics category. The rules you
-- provide in this field replace the ones that are currently being used in
-- the specified category.
updateCallAnalyticsCategory_rules :: Lens.Lens' UpdateCallAnalyticsCategory (Prelude.NonEmpty Rule)
updateCallAnalyticsCategory_rules = Lens.lens (\UpdateCallAnalyticsCategory' {rules} -> rules) (\s@UpdateCallAnalyticsCategory' {} a -> s {rules = a} :: UpdateCallAnalyticsCategory) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateCallAnalyticsCategory where
  type
    AWSResponse UpdateCallAnalyticsCategory =
      UpdateCallAnalyticsCategoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCallAnalyticsCategoryResponse'
            Prelude.<$> (x Data..?> "CategoryProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCallAnalyticsCategory where
  hashWithSalt _salt UpdateCallAnalyticsCategory' {..} =
    _salt
      `Prelude.hashWithSalt` inputType
      `Prelude.hashWithSalt` categoryName
      `Prelude.hashWithSalt` rules

instance Prelude.NFData UpdateCallAnalyticsCategory where
  rnf UpdateCallAnalyticsCategory' {..} =
    Prelude.rnf inputType
      `Prelude.seq` Prelude.rnf categoryName
      `Prelude.seq` Prelude.rnf rules

instance Data.ToHeaders UpdateCallAnalyticsCategory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.UpdateCallAnalyticsCategory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCallAnalyticsCategory where
  toJSON UpdateCallAnalyticsCategory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InputType" Data..=) Prelude.<$> inputType,
            Prelude.Just ("CategoryName" Data..= categoryName),
            Prelude.Just ("Rules" Data..= rules)
          ]
      )

instance Data.ToPath UpdateCallAnalyticsCategory where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCallAnalyticsCategory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCallAnalyticsCategoryResponse' smart constructor.
data UpdateCallAnalyticsCategoryResponse = UpdateCallAnalyticsCategoryResponse'
  { -- | Provides you with the properties of the Call Analytics category you
    -- specified in your @UpdateCallAnalyticsCategory@ request.
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
-- 'categoryProperties', 'updateCallAnalyticsCategoryResponse_categoryProperties' - Provides you with the properties of the Call Analytics category you
-- specified in your @UpdateCallAnalyticsCategory@ request.
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

-- | Provides you with the properties of the Call Analytics category you
-- specified in your @UpdateCallAnalyticsCategory@ request.
updateCallAnalyticsCategoryResponse_categoryProperties :: Lens.Lens' UpdateCallAnalyticsCategoryResponse (Prelude.Maybe CategoryProperties)
updateCallAnalyticsCategoryResponse_categoryProperties = Lens.lens (\UpdateCallAnalyticsCategoryResponse' {categoryProperties} -> categoryProperties) (\s@UpdateCallAnalyticsCategoryResponse' {} a -> s {categoryProperties = a} :: UpdateCallAnalyticsCategoryResponse)

-- | The response's http status code.
updateCallAnalyticsCategoryResponse_httpStatus :: Lens.Lens' UpdateCallAnalyticsCategoryResponse Prelude.Int
updateCallAnalyticsCategoryResponse_httpStatus = Lens.lens (\UpdateCallAnalyticsCategoryResponse' {httpStatus} -> httpStatus) (\s@UpdateCallAnalyticsCategoryResponse' {} a -> s {httpStatus = a} :: UpdateCallAnalyticsCategoryResponse)

instance
  Prelude.NFData
    UpdateCallAnalyticsCategoryResponse
  where
  rnf UpdateCallAnalyticsCategoryResponse' {..} =
    Prelude.rnf categoryProperties
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Network.AWS.Transcribe.CreateCallAnalyticsCategory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an analytics category. Amazon Transcribe applies the conditions
-- specified by your analytics categories to your call analytics jobs. For
-- each analytics category, you specify one or more rules. For example, you
-- can specify a rule that the customer sentiment was neutral or negative
-- within that category. If you start a call analytics job, Amazon
-- Transcribe applies the category to the analytics job that you\'ve
-- specified.
module Network.AWS.Transcribe.CreateCallAnalyticsCategory
  ( -- * Creating a Request
    CreateCallAnalyticsCategory (..),
    newCreateCallAnalyticsCategory,

    -- * Request Lenses
    createCallAnalyticsCategory_categoryName,
    createCallAnalyticsCategory_rules,

    -- * Destructuring the Response
    CreateCallAnalyticsCategoryResponse (..),
    newCreateCallAnalyticsCategoryResponse,

    -- * Response Lenses
    createCallAnalyticsCategoryResponse_categoryProperties,
    createCallAnalyticsCategoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newCreateCallAnalyticsCategory' smart constructor.
data CreateCallAnalyticsCategory = CreateCallAnalyticsCategory'
  { -- | The name that you choose for your category when you create it.
    categoryName :: Prelude.Text,
    -- | To create a category, you must specify between 1 and 20 rules. For each
    -- rule, you specify a filter to be applied to the attributes of the call.
    -- For example, you can specify a sentiment filter to detect if the
    -- customer\'s sentiment was negative or neutral.
    rules :: Prelude.NonEmpty Rule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCallAnalyticsCategory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryName', 'createCallAnalyticsCategory_categoryName' - The name that you choose for your category when you create it.
--
-- 'rules', 'createCallAnalyticsCategory_rules' - To create a category, you must specify between 1 and 20 rules. For each
-- rule, you specify a filter to be applied to the attributes of the call.
-- For example, you can specify a sentiment filter to detect if the
-- customer\'s sentiment was negative or neutral.
newCreateCallAnalyticsCategory ::
  -- | 'categoryName'
  Prelude.Text ->
  -- | 'rules'
  Prelude.NonEmpty Rule ->
  CreateCallAnalyticsCategory
newCreateCallAnalyticsCategory pCategoryName_ pRules_ =
  CreateCallAnalyticsCategory'
    { categoryName =
        pCategoryName_,
      rules = Lens.coerced Lens.# pRules_
    }

-- | The name that you choose for your category when you create it.
createCallAnalyticsCategory_categoryName :: Lens.Lens' CreateCallAnalyticsCategory Prelude.Text
createCallAnalyticsCategory_categoryName = Lens.lens (\CreateCallAnalyticsCategory' {categoryName} -> categoryName) (\s@CreateCallAnalyticsCategory' {} a -> s {categoryName = a} :: CreateCallAnalyticsCategory)

-- | To create a category, you must specify between 1 and 20 rules. For each
-- rule, you specify a filter to be applied to the attributes of the call.
-- For example, you can specify a sentiment filter to detect if the
-- customer\'s sentiment was negative or neutral.
createCallAnalyticsCategory_rules :: Lens.Lens' CreateCallAnalyticsCategory (Prelude.NonEmpty Rule)
createCallAnalyticsCategory_rules = Lens.lens (\CreateCallAnalyticsCategory' {rules} -> rules) (\s@CreateCallAnalyticsCategory' {} a -> s {rules = a} :: CreateCallAnalyticsCategory) Prelude.. Lens.coerced

instance Core.AWSRequest CreateCallAnalyticsCategory where
  type
    AWSResponse CreateCallAnalyticsCategory =
      CreateCallAnalyticsCategoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCallAnalyticsCategoryResponse'
            Prelude.<$> (x Core..?> "CategoryProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCallAnalyticsCategory

instance Prelude.NFData CreateCallAnalyticsCategory

instance Core.ToHeaders CreateCallAnalyticsCategory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.CreateCallAnalyticsCategory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCallAnalyticsCategory where
  toJSON CreateCallAnalyticsCategory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("CategoryName" Core..= categoryName),
            Prelude.Just ("Rules" Core..= rules)
          ]
      )

instance Core.ToPath CreateCallAnalyticsCategory where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCallAnalyticsCategory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCallAnalyticsCategoryResponse' smart constructor.
data CreateCallAnalyticsCategoryResponse = CreateCallAnalyticsCategoryResponse'
  { -- | The rules and associated metadata used to create a category.
    categoryProperties :: Prelude.Maybe CategoryProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCallAnalyticsCategoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryProperties', 'createCallAnalyticsCategoryResponse_categoryProperties' - The rules and associated metadata used to create a category.
--
-- 'httpStatus', 'createCallAnalyticsCategoryResponse_httpStatus' - The response's http status code.
newCreateCallAnalyticsCategoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCallAnalyticsCategoryResponse
newCreateCallAnalyticsCategoryResponse pHttpStatus_ =
  CreateCallAnalyticsCategoryResponse'
    { categoryProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The rules and associated metadata used to create a category.
createCallAnalyticsCategoryResponse_categoryProperties :: Lens.Lens' CreateCallAnalyticsCategoryResponse (Prelude.Maybe CategoryProperties)
createCallAnalyticsCategoryResponse_categoryProperties = Lens.lens (\CreateCallAnalyticsCategoryResponse' {categoryProperties} -> categoryProperties) (\s@CreateCallAnalyticsCategoryResponse' {} a -> s {categoryProperties = a} :: CreateCallAnalyticsCategoryResponse)

-- | The response's http status code.
createCallAnalyticsCategoryResponse_httpStatus :: Lens.Lens' CreateCallAnalyticsCategoryResponse Prelude.Int
createCallAnalyticsCategoryResponse_httpStatus = Lens.lens (\CreateCallAnalyticsCategoryResponse' {httpStatus} -> httpStatus) (\s@CreateCallAnalyticsCategoryResponse' {} a -> s {httpStatus = a} :: CreateCallAnalyticsCategoryResponse)

instance
  Prelude.NFData
    CreateCallAnalyticsCategoryResponse

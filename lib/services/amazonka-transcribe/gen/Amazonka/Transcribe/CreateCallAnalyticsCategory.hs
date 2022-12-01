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
-- Module      : Amazonka.Transcribe.CreateCallAnalyticsCategory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Call Analytics category.
--
-- All categories are automatically applied to your Call Analytics jobs.
-- Note that in order to apply your categories to your jobs, you must
-- create them before submitting your job request, as categories cannot be
-- applied retroactively.
--
-- Call Analytics categories are composed of rules. For each category, you
-- must create between 1 and 20 rules. Rules can include these parameters:
-- , , , and .
--
-- To update an existing category, see .
--
-- To learn more about:
--
-- -   Call Analytics categories, see
--     <https://docs.aws.amazon.com/transcribe/latest/dg/call-analytics-create-categories.html Creating categories>
--
-- -   Using rules, see
--     <https://docs.aws.amazon.com/transcribe/latest/dg/call-analytics-create-categories.html#call-analytics-create-categories-rules Rule criteria>
--     and refer to the data type
--
-- -   Call Analytics, see
--     <https://docs.aws.amazon.com/transcribe/latest/dg/call-analytics.html Analyzing call center audio with Call Analytics>
module Amazonka.Transcribe.CreateCallAnalyticsCategory
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newCreateCallAnalyticsCategory' smart constructor.
data CreateCallAnalyticsCategory = CreateCallAnalyticsCategory'
  { -- | A unique name, chosen by you, for your Call Analytics category. It\'s
    -- helpful to use a detailed naming system that will make sense to you in
    -- the future. For example, it\'s better to use
    -- @sentiment-positive-last30seconds@ for a category over a generic name
    -- like @test-category@.
    --
    -- Category names are case sensitive.
    categoryName :: Prelude.Text,
    -- | Rules define a Call Analytics category. When creating a new Call
    -- Analytics category, you must create between 1 and 20 rules for that
    -- category. For each rule, you specify a filter you want applied to the
    -- attributes of a call. For example, you can choose a sentiment filter
    -- that detects if a customer\'s sentiment was positive during the last 30
    -- seconds of the call.
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
-- 'categoryName', 'createCallAnalyticsCategory_categoryName' - A unique name, chosen by you, for your Call Analytics category. It\'s
-- helpful to use a detailed naming system that will make sense to you in
-- the future. For example, it\'s better to use
-- @sentiment-positive-last30seconds@ for a category over a generic name
-- like @test-category@.
--
-- Category names are case sensitive.
--
-- 'rules', 'createCallAnalyticsCategory_rules' - Rules define a Call Analytics category. When creating a new Call
-- Analytics category, you must create between 1 and 20 rules for that
-- category. For each rule, you specify a filter you want applied to the
-- attributes of a call. For example, you can choose a sentiment filter
-- that detects if a customer\'s sentiment was positive during the last 30
-- seconds of the call.
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

-- | A unique name, chosen by you, for your Call Analytics category. It\'s
-- helpful to use a detailed naming system that will make sense to you in
-- the future. For example, it\'s better to use
-- @sentiment-positive-last30seconds@ for a category over a generic name
-- like @test-category@.
--
-- Category names are case sensitive.
createCallAnalyticsCategory_categoryName :: Lens.Lens' CreateCallAnalyticsCategory Prelude.Text
createCallAnalyticsCategory_categoryName = Lens.lens (\CreateCallAnalyticsCategory' {categoryName} -> categoryName) (\s@CreateCallAnalyticsCategory' {} a -> s {categoryName = a} :: CreateCallAnalyticsCategory)

-- | Rules define a Call Analytics category. When creating a new Call
-- Analytics category, you must create between 1 and 20 rules for that
-- category. For each rule, you specify a filter you want applied to the
-- attributes of a call. For example, you can choose a sentiment filter
-- that detects if a customer\'s sentiment was positive during the last 30
-- seconds of the call.
createCallAnalyticsCategory_rules :: Lens.Lens' CreateCallAnalyticsCategory (Prelude.NonEmpty Rule)
createCallAnalyticsCategory_rules = Lens.lens (\CreateCallAnalyticsCategory' {rules} -> rules) (\s@CreateCallAnalyticsCategory' {} a -> s {rules = a} :: CreateCallAnalyticsCategory) Prelude.. Lens.coerced

instance Core.AWSRequest CreateCallAnalyticsCategory where
  type
    AWSResponse CreateCallAnalyticsCategory =
      CreateCallAnalyticsCategoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCallAnalyticsCategoryResponse'
            Prelude.<$> (x Core..?> "CategoryProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCallAnalyticsCategory where
  hashWithSalt _salt CreateCallAnalyticsCategory' {..} =
    _salt `Prelude.hashWithSalt` categoryName
      `Prelude.hashWithSalt` rules

instance Prelude.NFData CreateCallAnalyticsCategory where
  rnf CreateCallAnalyticsCategory' {..} =
    Prelude.rnf categoryName
      `Prelude.seq` Prelude.rnf rules

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
  { -- | Provides you with the properties of your new category, including its
    -- associated rules.
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
-- 'categoryProperties', 'createCallAnalyticsCategoryResponse_categoryProperties' - Provides you with the properties of your new category, including its
-- associated rules.
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

-- | Provides you with the properties of your new category, including its
-- associated rules.
createCallAnalyticsCategoryResponse_categoryProperties :: Lens.Lens' CreateCallAnalyticsCategoryResponse (Prelude.Maybe CategoryProperties)
createCallAnalyticsCategoryResponse_categoryProperties = Lens.lens (\CreateCallAnalyticsCategoryResponse' {categoryProperties} -> categoryProperties) (\s@CreateCallAnalyticsCategoryResponse' {} a -> s {categoryProperties = a} :: CreateCallAnalyticsCategoryResponse)

-- | The response's http status code.
createCallAnalyticsCategoryResponse_httpStatus :: Lens.Lens' CreateCallAnalyticsCategoryResponse Prelude.Int
createCallAnalyticsCategoryResponse_httpStatus = Lens.lens (\CreateCallAnalyticsCategoryResponse' {httpStatus} -> httpStatus) (\s@CreateCallAnalyticsCategoryResponse' {} a -> s {httpStatus = a} :: CreateCallAnalyticsCategoryResponse)

instance
  Prelude.NFData
    CreateCallAnalyticsCategoryResponse
  where
  rnf CreateCallAnalyticsCategoryResponse' {..} =
    Prelude.rnf categoryProperties
      `Prelude.seq` Prelude.rnf httpStatus

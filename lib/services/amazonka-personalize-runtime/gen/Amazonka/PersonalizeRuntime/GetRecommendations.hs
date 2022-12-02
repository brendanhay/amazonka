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
-- Module      : Amazonka.PersonalizeRuntime.GetRecommendations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of recommended items. For campaigns, the campaign\'s
-- Amazon Resource Name (ARN) is required and the required user and item
-- input depends on the recipe type used to create the solution backing the
-- campaign as follows:
--
-- -   USER_PERSONALIZATION - @userId@ required, @itemId@ not used
--
-- -   RELATED_ITEMS - @itemId@ required, @userId@ not used
--
-- Campaigns that are backed by a solution created using a recipe of type
-- PERSONALIZED_RANKING use the API.
--
-- For recommenders, the recommender\'s ARN is required and the required
-- item and user input depends on the use case (domain-based recipe)
-- backing the recommender. For information on use case requirements see
-- <https://docs.aws.amazon.com/personalize/latest/dg/domain-use-cases.html Choosing recommender use cases>.
module Amazonka.PersonalizeRuntime.GetRecommendations
  ( -- * Creating a Request
    GetRecommendations (..),
    newGetRecommendations,

    -- * Request Lenses
    getRecommendations_filterArn,
    getRecommendations_recommenderArn,
    getRecommendations_numResults,
    getRecommendations_context,
    getRecommendations_userId,
    getRecommendations_itemId,
    getRecommendations_filterValues,
    getRecommendations_promotions,
    getRecommendations_campaignArn,

    -- * Destructuring the Response
    GetRecommendationsResponse (..),
    newGetRecommendationsResponse,

    -- * Response Lenses
    getRecommendationsResponse_recommendationId,
    getRecommendationsResponse_itemList,
    getRecommendationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PersonalizeRuntime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRecommendations' smart constructor.
data GetRecommendations = GetRecommendations'
  { -- | The ARN of the filter to apply to the returned recommendations. For more
    -- information, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
    --
    -- When using this parameter, be sure the filter resource is @ACTIVE@.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the recommender to use to get
    -- recommendations. Provide a recommender ARN if you created a Domain
    -- dataset group with a recommender for a domain use case.
    recommenderArn :: Prelude.Maybe Prelude.Text,
    -- | The number of results to return. The default is 25. The maximum is 500.
    numResults :: Prelude.Maybe Prelude.Natural,
    -- | The contextual metadata to use when getting recommendations. Contextual
    -- metadata includes any interaction information that might be relevant
    -- when getting a user\'s recommendations, such as the user\'s current
    -- location or device type.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The user ID to provide recommendations for.
    --
    -- Required for @USER_PERSONALIZATION@ recipe type.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The item ID to provide recommendations for.
    --
    -- Required for @RELATED_ITEMS@ recipe type.
    itemId :: Prelude.Maybe Prelude.Text,
    -- | The values to use when filtering recommendations. For each placeholder
    -- parameter in your filter expression, provide the parameter name (in
    -- matching case) as a key and the filter value(s) as the corresponding
    -- value. Separate multiple values for one parameter with a comma.
    --
    -- For filter expressions that use an @INCLUDE@ element to include items,
    -- you must provide values for all parameters that are defined in the
    -- expression. For filters with expressions that use an @EXCLUDE@ element
    -- to exclude items, you can omit the @filter-values@.In this case, Amazon
    -- Personalize doesn\'t use that portion of the expression to filter
    -- recommendations.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering recommendations and user segments>.
    filterValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The promotions to apply to the recommendation request. A promotion
    -- defines additional business rules that apply to a configurable subset of
    -- recommended items.
    promotions :: Prelude.Maybe [Promotion],
    -- | The Amazon Resource Name (ARN) of the campaign to use for getting
    -- recommendations.
    campaignArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterArn', 'getRecommendations_filterArn' - The ARN of the filter to apply to the returned recommendations. For more
-- information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
--
-- When using this parameter, be sure the filter resource is @ACTIVE@.
--
-- 'recommenderArn', 'getRecommendations_recommenderArn' - The Amazon Resource Name (ARN) of the recommender to use to get
-- recommendations. Provide a recommender ARN if you created a Domain
-- dataset group with a recommender for a domain use case.
--
-- 'numResults', 'getRecommendations_numResults' - The number of results to return. The default is 25. The maximum is 500.
--
-- 'context', 'getRecommendations_context' - The contextual metadata to use when getting recommendations. Contextual
-- metadata includes any interaction information that might be relevant
-- when getting a user\'s recommendations, such as the user\'s current
-- location or device type.
--
-- 'userId', 'getRecommendations_userId' - The user ID to provide recommendations for.
--
-- Required for @USER_PERSONALIZATION@ recipe type.
--
-- 'itemId', 'getRecommendations_itemId' - The item ID to provide recommendations for.
--
-- Required for @RELATED_ITEMS@ recipe type.
--
-- 'filterValues', 'getRecommendations_filterValues' - The values to use when filtering recommendations. For each placeholder
-- parameter in your filter expression, provide the parameter name (in
-- matching case) as a key and the filter value(s) as the corresponding
-- value. Separate multiple values for one parameter with a comma.
--
-- For filter expressions that use an @INCLUDE@ element to include items,
-- you must provide values for all parameters that are defined in the
-- expression. For filters with expressions that use an @EXCLUDE@ element
-- to exclude items, you can omit the @filter-values@.In this case, Amazon
-- Personalize doesn\'t use that portion of the expression to filter
-- recommendations.
--
-- For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering recommendations and user segments>.
--
-- 'promotions', 'getRecommendations_promotions' - The promotions to apply to the recommendation request. A promotion
-- defines additional business rules that apply to a configurable subset of
-- recommended items.
--
-- 'campaignArn', 'getRecommendations_campaignArn' - The Amazon Resource Name (ARN) of the campaign to use for getting
-- recommendations.
newGetRecommendations ::
  GetRecommendations
newGetRecommendations =
  GetRecommendations'
    { filterArn = Prelude.Nothing,
      recommenderArn = Prelude.Nothing,
      numResults = Prelude.Nothing,
      context = Prelude.Nothing,
      userId = Prelude.Nothing,
      itemId = Prelude.Nothing,
      filterValues = Prelude.Nothing,
      promotions = Prelude.Nothing,
      campaignArn = Prelude.Nothing
    }

-- | The ARN of the filter to apply to the returned recommendations. For more
-- information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
--
-- When using this parameter, be sure the filter resource is @ACTIVE@.
getRecommendations_filterArn :: Lens.Lens' GetRecommendations (Prelude.Maybe Prelude.Text)
getRecommendations_filterArn = Lens.lens (\GetRecommendations' {filterArn} -> filterArn) (\s@GetRecommendations' {} a -> s {filterArn = a} :: GetRecommendations)

-- | The Amazon Resource Name (ARN) of the recommender to use to get
-- recommendations. Provide a recommender ARN if you created a Domain
-- dataset group with a recommender for a domain use case.
getRecommendations_recommenderArn :: Lens.Lens' GetRecommendations (Prelude.Maybe Prelude.Text)
getRecommendations_recommenderArn = Lens.lens (\GetRecommendations' {recommenderArn} -> recommenderArn) (\s@GetRecommendations' {} a -> s {recommenderArn = a} :: GetRecommendations)

-- | The number of results to return. The default is 25. The maximum is 500.
getRecommendations_numResults :: Lens.Lens' GetRecommendations (Prelude.Maybe Prelude.Natural)
getRecommendations_numResults = Lens.lens (\GetRecommendations' {numResults} -> numResults) (\s@GetRecommendations' {} a -> s {numResults = a} :: GetRecommendations)

-- | The contextual metadata to use when getting recommendations. Contextual
-- metadata includes any interaction information that might be relevant
-- when getting a user\'s recommendations, such as the user\'s current
-- location or device type.
getRecommendations_context :: Lens.Lens' GetRecommendations (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRecommendations_context = Lens.lens (\GetRecommendations' {context} -> context) (\s@GetRecommendations' {} a -> s {context = a} :: GetRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The user ID to provide recommendations for.
--
-- Required for @USER_PERSONALIZATION@ recipe type.
getRecommendations_userId :: Lens.Lens' GetRecommendations (Prelude.Maybe Prelude.Text)
getRecommendations_userId = Lens.lens (\GetRecommendations' {userId} -> userId) (\s@GetRecommendations' {} a -> s {userId = a} :: GetRecommendations)

-- | The item ID to provide recommendations for.
--
-- Required for @RELATED_ITEMS@ recipe type.
getRecommendations_itemId :: Lens.Lens' GetRecommendations (Prelude.Maybe Prelude.Text)
getRecommendations_itemId = Lens.lens (\GetRecommendations' {itemId} -> itemId) (\s@GetRecommendations' {} a -> s {itemId = a} :: GetRecommendations)

-- | The values to use when filtering recommendations. For each placeholder
-- parameter in your filter expression, provide the parameter name (in
-- matching case) as a key and the filter value(s) as the corresponding
-- value. Separate multiple values for one parameter with a comma.
--
-- For filter expressions that use an @INCLUDE@ element to include items,
-- you must provide values for all parameters that are defined in the
-- expression. For filters with expressions that use an @EXCLUDE@ element
-- to exclude items, you can omit the @filter-values@.In this case, Amazon
-- Personalize doesn\'t use that portion of the expression to filter
-- recommendations.
--
-- For more information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering recommendations and user segments>.
getRecommendations_filterValues :: Lens.Lens' GetRecommendations (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRecommendations_filterValues = Lens.lens (\GetRecommendations' {filterValues} -> filterValues) (\s@GetRecommendations' {} a -> s {filterValues = a} :: GetRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The promotions to apply to the recommendation request. A promotion
-- defines additional business rules that apply to a configurable subset of
-- recommended items.
getRecommendations_promotions :: Lens.Lens' GetRecommendations (Prelude.Maybe [Promotion])
getRecommendations_promotions = Lens.lens (\GetRecommendations' {promotions} -> promotions) (\s@GetRecommendations' {} a -> s {promotions = a} :: GetRecommendations) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the campaign to use for getting
-- recommendations.
getRecommendations_campaignArn :: Lens.Lens' GetRecommendations (Prelude.Maybe Prelude.Text)
getRecommendations_campaignArn = Lens.lens (\GetRecommendations' {campaignArn} -> campaignArn) (\s@GetRecommendations' {} a -> s {campaignArn = a} :: GetRecommendations)

instance Core.AWSRequest GetRecommendations where
  type
    AWSResponse GetRecommendations =
      GetRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommendationsResponse'
            Prelude.<$> (x Data..?> "recommendationId")
            Prelude.<*> (x Data..?> "itemList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRecommendations where
  hashWithSalt _salt GetRecommendations' {..} =
    _salt `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` recommenderArn
      `Prelude.hashWithSalt` numResults
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` itemId
      `Prelude.hashWithSalt` filterValues
      `Prelude.hashWithSalt` promotions
      `Prelude.hashWithSalt` campaignArn

instance Prelude.NFData GetRecommendations where
  rnf GetRecommendations' {..} =
    Prelude.rnf filterArn
      `Prelude.seq` Prelude.rnf recommenderArn
      `Prelude.seq` Prelude.rnf numResults
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf itemId
      `Prelude.seq` Prelude.rnf filterValues
      `Prelude.seq` Prelude.rnf promotions
      `Prelude.seq` Prelude.rnf campaignArn

instance Data.ToHeaders GetRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRecommendations where
  toJSON GetRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterArn" Data..=) Prelude.<$> filterArn,
            ("recommenderArn" Data..=)
              Prelude.<$> recommenderArn,
            ("numResults" Data..=) Prelude.<$> numResults,
            ("context" Data..=) Prelude.<$> context,
            ("userId" Data..=) Prelude.<$> userId,
            ("itemId" Data..=) Prelude.<$> itemId,
            ("filterValues" Data..=) Prelude.<$> filterValues,
            ("promotions" Data..=) Prelude.<$> promotions,
            ("campaignArn" Data..=) Prelude.<$> campaignArn
          ]
      )

instance Data.ToPath GetRecommendations where
  toPath = Prelude.const "/recommendations"

instance Data.ToQuery GetRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRecommendationsResponse' smart constructor.
data GetRecommendationsResponse = GetRecommendationsResponse'
  { -- | The ID of the recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | A list of recommendations sorted in descending order by prediction
    -- score. There can be a maximum of 500 items in the list.
    itemList :: Prelude.Maybe [PredictedItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recommendationId', 'getRecommendationsResponse_recommendationId' - The ID of the recommendation.
--
-- 'itemList', 'getRecommendationsResponse_itemList' - A list of recommendations sorted in descending order by prediction
-- score. There can be a maximum of 500 items in the list.
--
-- 'httpStatus', 'getRecommendationsResponse_httpStatus' - The response's http status code.
newGetRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecommendationsResponse
newGetRecommendationsResponse pHttpStatus_ =
  GetRecommendationsResponse'
    { recommendationId =
        Prelude.Nothing,
      itemList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the recommendation.
getRecommendationsResponse_recommendationId :: Lens.Lens' GetRecommendationsResponse (Prelude.Maybe Prelude.Text)
getRecommendationsResponse_recommendationId = Lens.lens (\GetRecommendationsResponse' {recommendationId} -> recommendationId) (\s@GetRecommendationsResponse' {} a -> s {recommendationId = a} :: GetRecommendationsResponse)

-- | A list of recommendations sorted in descending order by prediction
-- score. There can be a maximum of 500 items in the list.
getRecommendationsResponse_itemList :: Lens.Lens' GetRecommendationsResponse (Prelude.Maybe [PredictedItem])
getRecommendationsResponse_itemList = Lens.lens (\GetRecommendationsResponse' {itemList} -> itemList) (\s@GetRecommendationsResponse' {} a -> s {itemList = a} :: GetRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRecommendationsResponse_httpStatus :: Lens.Lens' GetRecommendationsResponse Prelude.Int
getRecommendationsResponse_httpStatus = Lens.lens (\GetRecommendationsResponse' {httpStatus} -> httpStatus) (\s@GetRecommendationsResponse' {} a -> s {httpStatus = a} :: GetRecommendationsResponse)

instance Prelude.NFData GetRecommendationsResponse where
  rnf GetRecommendationsResponse' {..} =
    Prelude.rnf recommendationId
      `Prelude.seq` Prelude.rnf itemList
      `Prelude.seq` Prelude.rnf httpStatus

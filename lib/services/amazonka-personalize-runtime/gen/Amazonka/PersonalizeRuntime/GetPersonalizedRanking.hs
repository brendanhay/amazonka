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
-- Module      : Amazonka.PersonalizeRuntime.GetPersonalizedRanking
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Re-ranks a list of recommended items for the given user. The first item
-- in the list is deemed the most likely item to be of interest to the
-- user.
--
-- The solution backing the campaign must have been created using a recipe
-- of type PERSONALIZED_RANKING.
module Amazonka.PersonalizeRuntime.GetPersonalizedRanking
  ( -- * Creating a Request
    GetPersonalizedRanking (..),
    newGetPersonalizedRanking,

    -- * Request Lenses
    getPersonalizedRanking_context,
    getPersonalizedRanking_filterArn,
    getPersonalizedRanking_filterValues,
    getPersonalizedRanking_campaignArn,
    getPersonalizedRanking_inputList,
    getPersonalizedRanking_userId,

    -- * Destructuring the Response
    GetPersonalizedRankingResponse (..),
    newGetPersonalizedRankingResponse,

    -- * Response Lenses
    getPersonalizedRankingResponse_personalizedRanking,
    getPersonalizedRankingResponse_recommendationId,
    getPersonalizedRankingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PersonalizeRuntime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPersonalizedRanking' smart constructor.
data GetPersonalizedRanking = GetPersonalizedRanking'
  { -- | The contextual metadata to use when getting recommendations. Contextual
    -- metadata includes any interaction information that might be relevant
    -- when getting a user\'s recommendations, such as the user\'s current
    -- location or device type.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The Amazon Resource Name (ARN) of a filter you created to include items
    -- or exclude items from recommendations for a given user. For more
    -- information, see
    -- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
    filterArn :: Prelude.Maybe Prelude.Text,
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
    -- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
    filterValues :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The Amazon Resource Name (ARN) of the campaign to use for generating the
    -- personalized ranking.
    campaignArn :: Prelude.Text,
    -- | A list of items (by @itemId@) to rank. If an item was not included in
    -- the training dataset, the item is appended to the end of the reranked
    -- list. The maximum is 500.
    inputList :: [Prelude.Text],
    -- | The user for which you want the campaign to provide a personalized
    -- ranking.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPersonalizedRanking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'context', 'getPersonalizedRanking_context' - The contextual metadata to use when getting recommendations. Contextual
-- metadata includes any interaction information that might be relevant
-- when getting a user\'s recommendations, such as the user\'s current
-- location or device type.
--
-- 'filterArn', 'getPersonalizedRanking_filterArn' - The Amazon Resource Name (ARN) of a filter you created to include items
-- or exclude items from recommendations for a given user. For more
-- information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
--
-- 'filterValues', 'getPersonalizedRanking_filterValues' - The values to use when filtering recommendations. For each placeholder
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
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
--
-- 'campaignArn', 'getPersonalizedRanking_campaignArn' - The Amazon Resource Name (ARN) of the campaign to use for generating the
-- personalized ranking.
--
-- 'inputList', 'getPersonalizedRanking_inputList' - A list of items (by @itemId@) to rank. If an item was not included in
-- the training dataset, the item is appended to the end of the reranked
-- list. The maximum is 500.
--
-- 'userId', 'getPersonalizedRanking_userId' - The user for which you want the campaign to provide a personalized
-- ranking.
newGetPersonalizedRanking ::
  -- | 'campaignArn'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  GetPersonalizedRanking
newGetPersonalizedRanking pCampaignArn_ pUserId_ =
  GetPersonalizedRanking'
    { context = Prelude.Nothing,
      filterArn = Prelude.Nothing,
      filterValues = Prelude.Nothing,
      campaignArn = pCampaignArn_,
      inputList = Prelude.mempty,
      userId = pUserId_
    }

-- | The contextual metadata to use when getting recommendations. Contextual
-- metadata includes any interaction information that might be relevant
-- when getting a user\'s recommendations, such as the user\'s current
-- location or device type.
getPersonalizedRanking_context :: Lens.Lens' GetPersonalizedRanking (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getPersonalizedRanking_context = Lens.lens (\GetPersonalizedRanking' {context} -> context) (\s@GetPersonalizedRanking' {} a -> s {context = a} :: GetPersonalizedRanking) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of a filter you created to include items
-- or exclude items from recommendations for a given user. For more
-- information, see
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
getPersonalizedRanking_filterArn :: Lens.Lens' GetPersonalizedRanking (Prelude.Maybe Prelude.Text)
getPersonalizedRanking_filterArn = Lens.lens (\GetPersonalizedRanking' {filterArn} -> filterArn) (\s@GetPersonalizedRanking' {} a -> s {filterArn = a} :: GetPersonalizedRanking)

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
-- <https://docs.aws.amazon.com/personalize/latest/dg/filter.html Filtering Recommendations>.
getPersonalizedRanking_filterValues :: Lens.Lens' GetPersonalizedRanking (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getPersonalizedRanking_filterValues = Lens.lens (\GetPersonalizedRanking' {filterValues} -> filterValues) (\s@GetPersonalizedRanking' {} a -> s {filterValues = a} :: GetPersonalizedRanking) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the campaign to use for generating the
-- personalized ranking.
getPersonalizedRanking_campaignArn :: Lens.Lens' GetPersonalizedRanking Prelude.Text
getPersonalizedRanking_campaignArn = Lens.lens (\GetPersonalizedRanking' {campaignArn} -> campaignArn) (\s@GetPersonalizedRanking' {} a -> s {campaignArn = a} :: GetPersonalizedRanking)

-- | A list of items (by @itemId@) to rank. If an item was not included in
-- the training dataset, the item is appended to the end of the reranked
-- list. The maximum is 500.
getPersonalizedRanking_inputList :: Lens.Lens' GetPersonalizedRanking [Prelude.Text]
getPersonalizedRanking_inputList = Lens.lens (\GetPersonalizedRanking' {inputList} -> inputList) (\s@GetPersonalizedRanking' {} a -> s {inputList = a} :: GetPersonalizedRanking) Prelude.. Lens.coerced

-- | The user for which you want the campaign to provide a personalized
-- ranking.
getPersonalizedRanking_userId :: Lens.Lens' GetPersonalizedRanking Prelude.Text
getPersonalizedRanking_userId = Lens.lens (\GetPersonalizedRanking' {userId} -> userId) (\s@GetPersonalizedRanking' {} a -> s {userId = a} :: GetPersonalizedRanking)

instance Core.AWSRequest GetPersonalizedRanking where
  type
    AWSResponse GetPersonalizedRanking =
      GetPersonalizedRankingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPersonalizedRankingResponse'
            Prelude.<$> ( x
                            Data..?> "personalizedRanking"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "recommendationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPersonalizedRanking where
  hashWithSalt _salt GetPersonalizedRanking' {..} =
    _salt
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` filterValues
      `Prelude.hashWithSalt` campaignArn
      `Prelude.hashWithSalt` inputList
      `Prelude.hashWithSalt` userId

instance Prelude.NFData GetPersonalizedRanking where
  rnf GetPersonalizedRanking' {..} =
    Prelude.rnf context `Prelude.seq`
      Prelude.rnf filterArn `Prelude.seq`
        Prelude.rnf filterValues `Prelude.seq`
          Prelude.rnf campaignArn `Prelude.seq`
            Prelude.rnf inputList `Prelude.seq`
              Prelude.rnf userId

instance Data.ToHeaders GetPersonalizedRanking where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPersonalizedRanking where
  toJSON GetPersonalizedRanking' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("context" Data..=) Prelude.<$> context,
            ("filterArn" Data..=) Prelude.<$> filterArn,
            ("filterValues" Data..=) Prelude.<$> filterValues,
            Prelude.Just ("campaignArn" Data..= campaignArn),
            Prelude.Just ("inputList" Data..= inputList),
            Prelude.Just ("userId" Data..= userId)
          ]
      )

instance Data.ToPath GetPersonalizedRanking where
  toPath = Prelude.const "/personalize-ranking"

instance Data.ToQuery GetPersonalizedRanking where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPersonalizedRankingResponse' smart constructor.
data GetPersonalizedRankingResponse = GetPersonalizedRankingResponse'
  { -- | A list of items in order of most likely interest to the user. The
    -- maximum is 500.
    personalizedRanking :: Prelude.Maybe [PredictedItem],
    -- | The ID of the recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPersonalizedRankingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'personalizedRanking', 'getPersonalizedRankingResponse_personalizedRanking' - A list of items in order of most likely interest to the user. The
-- maximum is 500.
--
-- 'recommendationId', 'getPersonalizedRankingResponse_recommendationId' - The ID of the recommendation.
--
-- 'httpStatus', 'getPersonalizedRankingResponse_httpStatus' - The response's http status code.
newGetPersonalizedRankingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPersonalizedRankingResponse
newGetPersonalizedRankingResponse pHttpStatus_ =
  GetPersonalizedRankingResponse'
    { personalizedRanking =
        Prelude.Nothing,
      recommendationId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of items in order of most likely interest to the user. The
-- maximum is 500.
getPersonalizedRankingResponse_personalizedRanking :: Lens.Lens' GetPersonalizedRankingResponse (Prelude.Maybe [PredictedItem])
getPersonalizedRankingResponse_personalizedRanking = Lens.lens (\GetPersonalizedRankingResponse' {personalizedRanking} -> personalizedRanking) (\s@GetPersonalizedRankingResponse' {} a -> s {personalizedRanking = a} :: GetPersonalizedRankingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the recommendation.
getPersonalizedRankingResponse_recommendationId :: Lens.Lens' GetPersonalizedRankingResponse (Prelude.Maybe Prelude.Text)
getPersonalizedRankingResponse_recommendationId = Lens.lens (\GetPersonalizedRankingResponse' {recommendationId} -> recommendationId) (\s@GetPersonalizedRankingResponse' {} a -> s {recommendationId = a} :: GetPersonalizedRankingResponse)

-- | The response's http status code.
getPersonalizedRankingResponse_httpStatus :: Lens.Lens' GetPersonalizedRankingResponse Prelude.Int
getPersonalizedRankingResponse_httpStatus = Lens.lens (\GetPersonalizedRankingResponse' {httpStatus} -> httpStatus) (\s@GetPersonalizedRankingResponse' {} a -> s {httpStatus = a} :: GetPersonalizedRankingResponse)

instance
  Prelude.NFData
    GetPersonalizedRankingResponse
  where
  rnf GetPersonalizedRankingResponse' {..} =
    Prelude.rnf personalizedRanking `Prelude.seq`
      Prelude.rnf recommendationId `Prelude.seq`
        Prelude.rnf httpStatus

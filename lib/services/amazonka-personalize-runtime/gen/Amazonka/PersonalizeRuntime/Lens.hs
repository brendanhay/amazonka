{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PersonalizeRuntime.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PersonalizeRuntime.Lens
  ( -- * Operations

    -- ** GetPersonalizedRanking
    getPersonalizedRanking_context,
    getPersonalizedRanking_filterArn,
    getPersonalizedRanking_filterValues,
    getPersonalizedRanking_campaignArn,
    getPersonalizedRanking_inputList,
    getPersonalizedRanking_userId,
    getPersonalizedRankingResponse_personalizedRanking,
    getPersonalizedRankingResponse_recommendationId,
    getPersonalizedRankingResponse_httpStatus,

    -- ** GetRecommendations
    getRecommendations_campaignArn,
    getRecommendations_context,
    getRecommendations_filterArn,
    getRecommendations_filterValues,
    getRecommendations_itemId,
    getRecommendations_numResults,
    getRecommendations_promotions,
    getRecommendations_recommenderArn,
    getRecommendations_userId,
    getRecommendationsResponse_itemList,
    getRecommendationsResponse_recommendationId,
    getRecommendationsResponse_httpStatus,

    -- * Types

    -- ** PredictedItem
    predictedItem_itemId,
    predictedItem_promotionName,
    predictedItem_score,

    -- ** Promotion
    promotion_filterArn,
    promotion_filterValues,
    promotion_name,
    promotion_percentPromotedItems,
  )
where

import Amazonka.PersonalizeRuntime.GetPersonalizedRanking
import Amazonka.PersonalizeRuntime.GetRecommendations
import Amazonka.PersonalizeRuntime.Types.PredictedItem
import Amazonka.PersonalizeRuntime.Types.Promotion

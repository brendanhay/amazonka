{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PersonalizeRuntime.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PersonalizeRuntime.Lens
  ( -- * Operations

    -- ** GetPersonalizedRanking
    getPersonalizedRanking_filterArn,
    getPersonalizedRanking_context,
    getPersonalizedRanking_filterValues,
    getPersonalizedRanking_campaignArn,
    getPersonalizedRanking_inputList,
    getPersonalizedRanking_userId,
    getPersonalizedRankingResponse_personalizedRanking,
    getPersonalizedRankingResponse_recommendationId,
    getPersonalizedRankingResponse_httpStatus,

    -- ** GetRecommendations
    getRecommendations_filterArn,
    getRecommendations_recommenderArn,
    getRecommendations_numResults,
    getRecommendations_context,
    getRecommendations_userId,
    getRecommendations_itemId,
    getRecommendations_filterValues,
    getRecommendations_promotions,
    getRecommendations_campaignArn,
    getRecommendationsResponse_recommendationId,
    getRecommendationsResponse_itemList,
    getRecommendationsResponse_httpStatus,

    -- * Types

    -- ** PredictedItem
    predictedItem_score,
    predictedItem_promotionName,
    predictedItem_itemId,

    -- ** Promotion
    promotion_name,
    promotion_percentPromotedItems,
    promotion_filterArn,
    promotion_filterValues,
  )
where

import Amazonka.PersonalizeRuntime.GetPersonalizedRanking
import Amazonka.PersonalizeRuntime.GetRecommendations
import Amazonka.PersonalizeRuntime.Types.PredictedItem
import Amazonka.PersonalizeRuntime.Types.Promotion

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationPurchaseRecommendationMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about this specific recommendation, such as the timestamp for when AWS made a specific recommendation.
--
--
--
-- /See:/ 'reservationPurchaseRecommendationMetadata' smart constructor.
data ReservationPurchaseRecommendationMetadata = ReservationPurchaseRecommendationMetadata'
  { _rprmRecommendationId ::
      !( Maybe
           Text
       ),
    _rprmGenerationTimestamp ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ReservationPurchaseRecommendationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rprmRecommendationId' - The ID for this specific recommendation.
--
-- * 'rprmGenerationTimestamp' - The timestamp for when AWS made this recommendation.
reservationPurchaseRecommendationMetadata ::
  ReservationPurchaseRecommendationMetadata
reservationPurchaseRecommendationMetadata =
  ReservationPurchaseRecommendationMetadata'
    { _rprmRecommendationId =
        Nothing,
      _rprmGenerationTimestamp = Nothing
    }

-- | The ID for this specific recommendation.
rprmRecommendationId :: Lens' ReservationPurchaseRecommendationMetadata (Maybe Text)
rprmRecommendationId = lens _rprmRecommendationId (\s a -> s {_rprmRecommendationId = a})

-- | The timestamp for when AWS made this recommendation.
rprmGenerationTimestamp :: Lens' ReservationPurchaseRecommendationMetadata (Maybe Text)
rprmGenerationTimestamp = lens _rprmGenerationTimestamp (\s a -> s {_rprmGenerationTimestamp = a})

instance FromJSON ReservationPurchaseRecommendationMetadata where
  parseJSON =
    withObject
      "ReservationPurchaseRecommendationMetadata"
      ( \x ->
          ReservationPurchaseRecommendationMetadata'
            <$> (x .:? "RecommendationId") <*> (x .:? "GenerationTimestamp")
      )

instance Hashable ReservationPurchaseRecommendationMetadata

instance NFData ReservationPurchaseRecommendationMetadata

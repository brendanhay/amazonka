{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationMetadata where

import Network.AWS.CostExplorer.Types.LookbackPeriodInDays
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Metadata for this recommendation set.
--
--
--
-- /See:/ 'rightsizingRecommendationMetadata' smart constructor.
data RightsizingRecommendationMetadata = RightsizingRecommendationMetadata'
  { _rrmRecommendationId ::
      !(Maybe Text),
    _rrmGenerationTimestamp ::
      !(Maybe Text),
    _rrmLookbackPeriodInDays ::
      !( Maybe
           LookbackPeriodInDays
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RightsizingRecommendationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrmRecommendationId' - The ID for this specific recommendation.
--
-- * 'rrmGenerationTimestamp' - The timestamp for when AWS made this recommendation.
--
-- * 'rrmLookbackPeriodInDays' - How many days of previous usage that AWS considers when making this recommendation.
rightsizingRecommendationMetadata ::
  RightsizingRecommendationMetadata
rightsizingRecommendationMetadata =
  RightsizingRecommendationMetadata'
    { _rrmRecommendationId =
        Nothing,
      _rrmGenerationTimestamp = Nothing,
      _rrmLookbackPeriodInDays = Nothing
    }

-- | The ID for this specific recommendation.
rrmRecommendationId :: Lens' RightsizingRecommendationMetadata (Maybe Text)
rrmRecommendationId = lens _rrmRecommendationId (\s a -> s {_rrmRecommendationId = a})

-- | The timestamp for when AWS made this recommendation.
rrmGenerationTimestamp :: Lens' RightsizingRecommendationMetadata (Maybe Text)
rrmGenerationTimestamp = lens _rrmGenerationTimestamp (\s a -> s {_rrmGenerationTimestamp = a})

-- | How many days of previous usage that AWS considers when making this recommendation.
rrmLookbackPeriodInDays :: Lens' RightsizingRecommendationMetadata (Maybe LookbackPeriodInDays)
rrmLookbackPeriodInDays = lens _rrmLookbackPeriodInDays (\s a -> s {_rrmLookbackPeriodInDays = a})

instance FromJSON RightsizingRecommendationMetadata where
  parseJSON =
    withObject
      "RightsizingRecommendationMetadata"
      ( \x ->
          RightsizingRecommendationMetadata'
            <$> (x .:? "RecommendationId")
            <*> (x .:? "GenerationTimestamp")
            <*> (x .:? "LookbackPeriodInDays")
      )

instance Hashable RightsizingRecommendationMetadata

instance NFData RightsizingRecommendationMetadata

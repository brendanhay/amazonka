{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansPurchaseRecommendationMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Metadata about your Savings Plans Purchase Recommendations.
--
--
--
-- /See:/ 'savingsPlansPurchaseRecommendationMetadata' smart constructor.
data SavingsPlansPurchaseRecommendationMetadata = SavingsPlansPurchaseRecommendationMetadata'
  { _spprmRecommendationId ::
      !( Maybe
           Text
       ),
    _spprmGenerationTimestamp ::
      !( Maybe
           Text
       ),
    _spprmAdditionalMetadata ::
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

-- | Creates a value of 'SavingsPlansPurchaseRecommendationMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spprmRecommendationId' - The unique identifier for the recommendation set.
--
-- * 'spprmGenerationTimestamp' - The timestamp showing when the recommendations were generated.
--
-- * 'spprmAdditionalMetadata' - Additional metadata that may be applicable to the recommendation.
savingsPlansPurchaseRecommendationMetadata ::
  SavingsPlansPurchaseRecommendationMetadata
savingsPlansPurchaseRecommendationMetadata =
  SavingsPlansPurchaseRecommendationMetadata'
    { _spprmRecommendationId =
        Nothing,
      _spprmGenerationTimestamp = Nothing,
      _spprmAdditionalMetadata = Nothing
    }

-- | The unique identifier for the recommendation set.
spprmRecommendationId :: Lens' SavingsPlansPurchaseRecommendationMetadata (Maybe Text)
spprmRecommendationId = lens _spprmRecommendationId (\s a -> s {_spprmRecommendationId = a})

-- | The timestamp showing when the recommendations were generated.
spprmGenerationTimestamp :: Lens' SavingsPlansPurchaseRecommendationMetadata (Maybe Text)
spprmGenerationTimestamp = lens _spprmGenerationTimestamp (\s a -> s {_spprmGenerationTimestamp = a})

-- | Additional metadata that may be applicable to the recommendation.
spprmAdditionalMetadata :: Lens' SavingsPlansPurchaseRecommendationMetadata (Maybe Text)
spprmAdditionalMetadata = lens _spprmAdditionalMetadata (\s a -> s {_spprmAdditionalMetadata = a})

instance FromJSON SavingsPlansPurchaseRecommendationMetadata where
  parseJSON =
    withObject
      "SavingsPlansPurchaseRecommendationMetadata"
      ( \x ->
          SavingsPlansPurchaseRecommendationMetadata'
            <$> (x .:? "RecommendationId")
            <*> (x .:? "GenerationTimestamp")
            <*> (x .:? "AdditionalMetadata")
      )

instance Hashable SavingsPlansPurchaseRecommendationMetadata

instance NFData SavingsPlansPurchaseRecommendationMetadata

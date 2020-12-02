{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendationConfiguration where

import Network.AWS.CostExplorer.Types.RecommendationTarget
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither.
--
--
--
-- /See:/ 'rightsizingRecommendationConfiguration' smart constructor.
data RightsizingRecommendationConfiguration = RightsizingRecommendationConfiguration'
  { _rrcRecommendationTarget ::
      !RecommendationTarget,
    _rrcBenefitsConsidered ::
      !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RightsizingRecommendationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrcRecommendationTarget' - The option to see recommendations within the same instance family, or recommendations for instances across other families. The default value is @SAME_INSTANCE_FAMILY@ .
--
-- * 'rrcBenefitsConsidered' - The option to consider RI or Savings Plans discount benefits in your savings calculation. The default value is @TRUE@ .
rightsizingRecommendationConfiguration ::
  -- | 'rrcRecommendationTarget'
  RecommendationTarget ->
  -- | 'rrcBenefitsConsidered'
  Bool ->
  RightsizingRecommendationConfiguration
rightsizingRecommendationConfiguration
  pRecommendationTarget_
  pBenefitsConsidered_ =
    RightsizingRecommendationConfiguration'
      { _rrcRecommendationTarget =
          pRecommendationTarget_,
        _rrcBenefitsConsidered = pBenefitsConsidered_
      }

-- | The option to see recommendations within the same instance family, or recommendations for instances across other families. The default value is @SAME_INSTANCE_FAMILY@ .
rrcRecommendationTarget :: Lens' RightsizingRecommendationConfiguration RecommendationTarget
rrcRecommendationTarget = lens _rrcRecommendationTarget (\s a -> s {_rrcRecommendationTarget = a})

-- | The option to consider RI or Savings Plans discount benefits in your savings calculation. The default value is @TRUE@ .
rrcBenefitsConsidered :: Lens' RightsizingRecommendationConfiguration Bool
rrcBenefitsConsidered = lens _rrcBenefitsConsidered (\s a -> s {_rrcBenefitsConsidered = a})

instance FromJSON RightsizingRecommendationConfiguration where
  parseJSON =
    withObject
      "RightsizingRecommendationConfiguration"
      ( \x ->
          RightsizingRecommendationConfiguration'
            <$> (x .: "RecommendationTarget") <*> (x .: "BenefitsConsidered")
      )

instance Hashable RightsizingRecommendationConfiguration

instance NFData RightsizingRecommendationConfiguration

instance ToJSON RightsizingRecommendationConfiguration where
  toJSON RightsizingRecommendationConfiguration' {..} =
    object
      ( catMaybes
          [ Just ("RecommendationTarget" .= _rrcRecommendationTarget),
            Just ("BenefitsConsidered" .= _rrcBenefitsConsidered)
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RightsizingRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RightsizingRecommendation where

import Network.AWS.CostExplorer.Types.CurrentInstance
import Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
import Network.AWS.CostExplorer.Types.RightsizingType
import Network.AWS.CostExplorer.Types.TerminateRecommendationDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Recommendations to rightsize resources.
--
--
--
-- /See:/ 'rightsizingRecommendation' smart constructor.
data RightsizingRecommendation = RightsizingRecommendation'
  { _rrAccountId ::
      !(Maybe Text),
    _rrModifyRecommendationDetail ::
      !(Maybe ModifyRecommendationDetail),
    _rrCurrentInstance ::
      !(Maybe CurrentInstance),
    _rrRightsizingType ::
      !(Maybe RightsizingType),
    _rrTerminateRecommendationDetail ::
      !(Maybe TerminateRecommendationDetail)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RightsizingRecommendation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrAccountId' - The account that this recommendation is for.
--
-- * 'rrModifyRecommendationDetail' - Details for modification recommendations.
--
-- * 'rrCurrentInstance' - Context regarding the current instance.
--
-- * 'rrRightsizingType' - Recommendation to either terminate or modify the resource.
--
-- * 'rrTerminateRecommendationDetail' - Details for termination recommendations.
rightsizingRecommendation ::
  RightsizingRecommendation
rightsizingRecommendation =
  RightsizingRecommendation'
    { _rrAccountId = Nothing,
      _rrModifyRecommendationDetail = Nothing,
      _rrCurrentInstance = Nothing,
      _rrRightsizingType = Nothing,
      _rrTerminateRecommendationDetail = Nothing
    }

-- | The account that this recommendation is for.
rrAccountId :: Lens' RightsizingRecommendation (Maybe Text)
rrAccountId = lens _rrAccountId (\s a -> s {_rrAccountId = a})

-- | Details for modification recommendations.
rrModifyRecommendationDetail :: Lens' RightsizingRecommendation (Maybe ModifyRecommendationDetail)
rrModifyRecommendationDetail = lens _rrModifyRecommendationDetail (\s a -> s {_rrModifyRecommendationDetail = a})

-- | Context regarding the current instance.
rrCurrentInstance :: Lens' RightsizingRecommendation (Maybe CurrentInstance)
rrCurrentInstance = lens _rrCurrentInstance (\s a -> s {_rrCurrentInstance = a})

-- | Recommendation to either terminate or modify the resource.
rrRightsizingType :: Lens' RightsizingRecommendation (Maybe RightsizingType)
rrRightsizingType = lens _rrRightsizingType (\s a -> s {_rrRightsizingType = a})

-- | Details for termination recommendations.
rrTerminateRecommendationDetail :: Lens' RightsizingRecommendation (Maybe TerminateRecommendationDetail)
rrTerminateRecommendationDetail = lens _rrTerminateRecommendationDetail (\s a -> s {_rrTerminateRecommendationDetail = a})

instance FromJSON RightsizingRecommendation where
  parseJSON =
    withObject
      "RightsizingRecommendation"
      ( \x ->
          RightsizingRecommendation'
            <$> (x .:? "AccountId")
            <*> (x .:? "ModifyRecommendationDetail")
            <*> (x .:? "CurrentInstance")
            <*> (x .:? "RightsizingType")
            <*> (x .:? "TerminateRecommendationDetail")
      )

instance Hashable RightsizingRecommendation

instance NFData RightsizingRecommendation

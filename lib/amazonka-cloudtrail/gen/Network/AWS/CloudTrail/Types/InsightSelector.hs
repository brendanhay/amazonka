{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.InsightSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.InsightSelector where

import Network.AWS.CloudTrail.Types.InsightType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A JSON string that contains a list of insight types that are logged on a trail.
--
--
--
-- /See:/ 'insightSelector' smart constructor.
newtype InsightSelector = InsightSelector'
  { _isInsightType ::
      Maybe InsightType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsightSelector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'isInsightType' - The type of insights to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
insightSelector ::
  InsightSelector
insightSelector = InsightSelector' {_isInsightType = Nothing}

-- | The type of insights to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
isInsightType :: Lens' InsightSelector (Maybe InsightType)
isInsightType = lens _isInsightType (\s a -> s {_isInsightType = a})

instance FromJSON InsightSelector where
  parseJSON =
    withObject
      "InsightSelector"
      (\x -> InsightSelector' <$> (x .:? "InsightType"))

instance Hashable InsightSelector

instance NFData InsightSelector

instance ToJSON InsightSelector where
  toJSON InsightSelector' {..} =
    object (catMaybes [("InsightType" .=) <$> _isInsightType])

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ModifyRecommendationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ModifyRecommendationDetail where

import Network.AWS.CostExplorer.Types.TargetInstance
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on the modification recommendation.
--
--
--
-- /See:/ 'modifyRecommendationDetail' smart constructor.
newtype ModifyRecommendationDetail = ModifyRecommendationDetail'
  { _mrdTargetInstances ::
      Maybe [TargetInstance]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyRecommendationDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrdTargetInstances' - Identifies whether this instance type is the AWS default recommendation.
modifyRecommendationDetail ::
  ModifyRecommendationDetail
modifyRecommendationDetail =
  ModifyRecommendationDetail' {_mrdTargetInstances = Nothing}

-- | Identifies whether this instance type is the AWS default recommendation.
mrdTargetInstances :: Lens' ModifyRecommendationDetail [TargetInstance]
mrdTargetInstances = lens _mrdTargetInstances (\s a -> s {_mrdTargetInstances = a}) . _Default . _Coerce

instance FromJSON ModifyRecommendationDetail where
  parseJSON =
    withObject
      "ModifyRecommendationDetail"
      ( \x ->
          ModifyRecommendationDetail'
            <$> (x .:? "TargetInstances" .!= mempty)
      )

instance Hashable ModifyRecommendationDetail

instance NFData ModifyRecommendationDetail

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.CapacityProviderStrategyItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProviderStrategyItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of a capacity provider strategy.
--
--
--
-- /See:/ 'capacityProviderStrategyItem' smart constructor.
data CapacityProviderStrategyItem = CapacityProviderStrategyItem'
  { _cpsiBase ::
      !(Maybe Nat),
    _cpsiWeight :: !(Maybe Nat),
    _cpsiCapacityProvider :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CapacityProviderStrategyItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpsiBase' - The /base/ value designates how many tasks, at a minimum, to run on the specified capacity provider. Only one capacity provider in a capacity provider strategy can have a /base/ defined.
--
-- * 'cpsiWeight' - The /weight/ value designates the relative percentage of the total number of tasks launched that should use the specified capacity provider. For example, if you have a strategy that contains two capacity providers and both have a weight of @1@ , then when the @base@ is satisfied, the tasks will be split evenly across the two capacity providers. Using that same logic, if you specify a weight of @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/ , then for every one task that is run using /capacityProviderA/ , four tasks would use /capacityProviderB/ .
--
-- * 'cpsiCapacityProvider' - The short name of the capacity provider.
capacityProviderStrategyItem ::
  -- | 'cpsiCapacityProvider'
  Text ->
  CapacityProviderStrategyItem
capacityProviderStrategyItem pCapacityProvider_ =
  CapacityProviderStrategyItem'
    { _cpsiBase = Nothing,
      _cpsiWeight = Nothing,
      _cpsiCapacityProvider = pCapacityProvider_
    }

-- | The /base/ value designates how many tasks, at a minimum, to run on the specified capacity provider. Only one capacity provider in a capacity provider strategy can have a /base/ defined.
cpsiBase :: Lens' CapacityProviderStrategyItem (Maybe Natural)
cpsiBase = lens _cpsiBase (\s a -> s {_cpsiBase = a}) . mapping _Nat

-- | The /weight/ value designates the relative percentage of the total number of tasks launched that should use the specified capacity provider. For example, if you have a strategy that contains two capacity providers and both have a weight of @1@ , then when the @base@ is satisfied, the tasks will be split evenly across the two capacity providers. Using that same logic, if you specify a weight of @1@ for /capacityProviderA/ and a weight of @4@ for /capacityProviderB/ , then for every one task that is run using /capacityProviderA/ , four tasks would use /capacityProviderB/ .
cpsiWeight :: Lens' CapacityProviderStrategyItem (Maybe Natural)
cpsiWeight = lens _cpsiWeight (\s a -> s {_cpsiWeight = a}) . mapping _Nat

-- | The short name of the capacity provider.
cpsiCapacityProvider :: Lens' CapacityProviderStrategyItem Text
cpsiCapacityProvider = lens _cpsiCapacityProvider (\s a -> s {_cpsiCapacityProvider = a})

instance FromJSON CapacityProviderStrategyItem where
  parseJSON =
    withObject
      "CapacityProviderStrategyItem"
      ( \x ->
          CapacityProviderStrategyItem'
            <$> (x .:? "base") <*> (x .:? "weight") <*> (x .: "capacityProvider")
      )

instance Hashable CapacityProviderStrategyItem

instance NFData CapacityProviderStrategyItem

instance ToJSON CapacityProviderStrategyItem where
  toJSON CapacityProviderStrategyItem' {..} =
    object
      ( catMaybes
          [ ("base" .=) <$> _cpsiBase,
            ("weight" .=) <$> _cpsiWeight,
            Just ("capacityProvider" .= _cpsiCapacityProvider)
          ]
      )

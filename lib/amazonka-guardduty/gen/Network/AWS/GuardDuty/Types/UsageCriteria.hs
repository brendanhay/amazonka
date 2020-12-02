{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageCriteria where

import Network.AWS.GuardDuty.Types.DataSource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the criteria used to query usage statistics.
--
--
--
-- /See:/ 'usageCriteria' smart constructor.
data UsageCriteria = UsageCriteria'
  { _ucAccountIds ::
      !(Maybe (List1 Text)),
    _ucResources :: !(Maybe [Text]),
    _ucDataSources :: ![DataSource]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucAccountIds' - The account IDs to aggregate usage statistics from.
--
-- * 'ucResources' - The resources to aggregate usage statistics from. Only accepts exact resource names.
--
-- * 'ucDataSources' - The data sources to aggregate usage statistics from.
usageCriteria ::
  UsageCriteria
usageCriteria =
  UsageCriteria'
    { _ucAccountIds = Nothing,
      _ucResources = Nothing,
      _ucDataSources = mempty
    }

-- | The account IDs to aggregate usage statistics from.
ucAccountIds :: Lens' UsageCriteria (Maybe (NonEmpty Text))
ucAccountIds = lens _ucAccountIds (\s a -> s {_ucAccountIds = a}) . mapping _List1

-- | The resources to aggregate usage statistics from. Only accepts exact resource names.
ucResources :: Lens' UsageCriteria [Text]
ucResources = lens _ucResources (\s a -> s {_ucResources = a}) . _Default . _Coerce

-- | The data sources to aggregate usage statistics from.
ucDataSources :: Lens' UsageCriteria [DataSource]
ucDataSources = lens _ucDataSources (\s a -> s {_ucDataSources = a}) . _Coerce

instance Hashable UsageCriteria

instance NFData UsageCriteria

instance ToJSON UsageCriteria where
  toJSON UsageCriteria' {..} =
    object
      ( catMaybes
          [ ("accountIds" .=) <$> _ucAccountIds,
            ("resources" .=) <$> _ucResources,
            Just ("dataSources" .= _ucDataSources)
          ]
      )

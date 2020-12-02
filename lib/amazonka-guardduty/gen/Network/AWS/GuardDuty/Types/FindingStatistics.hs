{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingStatistics where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about finding statistics.
--
--
--
-- /See:/ 'findingStatistics' smart constructor.
newtype FindingStatistics = FindingStatistics'
  { _fsCountBySeverity ::
      Maybe (Map Text (Int))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FindingStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fsCountBySeverity' - Represents a map of severity to count statistics for a set of findings.
findingStatistics ::
  FindingStatistics
findingStatistics =
  FindingStatistics' {_fsCountBySeverity = Nothing}

-- | Represents a map of severity to count statistics for a set of findings.
fsCountBySeverity :: Lens' FindingStatistics (HashMap Text (Int))
fsCountBySeverity = lens _fsCountBySeverity (\s a -> s {_fsCountBySeverity = a}) . _Default . _Map

instance FromJSON FindingStatistics where
  parseJSON =
    withObject
      "FindingStatistics"
      ( \x ->
          FindingStatistics' <$> (x .:? "countBySeverity" .!= mempty)
      )

instance Hashable FindingStatistics

instance NFData FindingStatistics

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContributorInsightsSummary where

import Network.AWS.DynamoDB.Types.ContributorInsightsStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a Contributor Insights summary entry.
--
--
--
-- /See:/ 'contributorInsightsSummary' smart constructor.
data ContributorInsightsSummary = ContributorInsightsSummary'
  { _cisContributorInsightsStatus ::
      !(Maybe ContributorInsightsStatus),
    _cisTableName :: !(Maybe Text),
    _cisIndexName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContributorInsightsSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisContributorInsightsStatus' - Describes the current status for contributor insights for the given table and index, if applicable.
--
-- * 'cisTableName' - Name of the table associated with the summary.
--
-- * 'cisIndexName' - Name of the index associated with the summary, if any.
contributorInsightsSummary ::
  ContributorInsightsSummary
contributorInsightsSummary =
  ContributorInsightsSummary'
    { _cisContributorInsightsStatus =
        Nothing,
      _cisTableName = Nothing,
      _cisIndexName = Nothing
    }

-- | Describes the current status for contributor insights for the given table and index, if applicable.
cisContributorInsightsStatus :: Lens' ContributorInsightsSummary (Maybe ContributorInsightsStatus)
cisContributorInsightsStatus = lens _cisContributorInsightsStatus (\s a -> s {_cisContributorInsightsStatus = a})

-- | Name of the table associated with the summary.
cisTableName :: Lens' ContributorInsightsSummary (Maybe Text)
cisTableName = lens _cisTableName (\s a -> s {_cisTableName = a})

-- | Name of the index associated with the summary, if any.
cisIndexName :: Lens' ContributorInsightsSummary (Maybe Text)
cisIndexName = lens _cisIndexName (\s a -> s {_cisIndexName = a})

instance FromJSON ContributorInsightsSummary where
  parseJSON =
    withObject
      "ContributorInsightsSummary"
      ( \x ->
          ContributorInsightsSummary'
            <$> (x .:? "ContributorInsightsStatus")
            <*> (x .:? "TableName")
            <*> (x .:? "IndexName")
      )

instance Hashable ContributorInsightsSummary

instance NFData ContributorInsightsSummary

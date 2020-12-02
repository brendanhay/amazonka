{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AccountAggregationSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AccountAggregationSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A collection of accounts and regions.
--
--
--
-- /See:/ 'accountAggregationSource' smart constructor.
data AccountAggregationSource = AccountAggregationSource'
  { _aasAWSRegions ::
      !(Maybe (List1 Text)),
    _aasAllAWSRegions :: !(Maybe Bool),
    _aasAccountIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountAggregationSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aasAWSRegions' - The source regions being aggregated.
--
-- * 'aasAllAWSRegions' - If true, aggregate existing AWS Config regions and future regions.
--
-- * 'aasAccountIds' - The 12-digit account ID of the account being aggregated.
accountAggregationSource ::
  -- | 'aasAccountIds'
  NonEmpty Text ->
  AccountAggregationSource
accountAggregationSource pAccountIds_ =
  AccountAggregationSource'
    { _aasAWSRegions = Nothing,
      _aasAllAWSRegions = Nothing,
      _aasAccountIds = _List1 # pAccountIds_
    }

-- | The source regions being aggregated.
aasAWSRegions :: Lens' AccountAggregationSource (Maybe (NonEmpty Text))
aasAWSRegions = lens _aasAWSRegions (\s a -> s {_aasAWSRegions = a}) . mapping _List1

-- | If true, aggregate existing AWS Config regions and future regions.
aasAllAWSRegions :: Lens' AccountAggregationSource (Maybe Bool)
aasAllAWSRegions = lens _aasAllAWSRegions (\s a -> s {_aasAllAWSRegions = a})

-- | The 12-digit account ID of the account being aggregated.
aasAccountIds :: Lens' AccountAggregationSource (NonEmpty Text)
aasAccountIds = lens _aasAccountIds (\s a -> s {_aasAccountIds = a}) . _List1

instance FromJSON AccountAggregationSource where
  parseJSON =
    withObject
      "AccountAggregationSource"
      ( \x ->
          AccountAggregationSource'
            <$> (x .:? "AwsRegions")
            <*> (x .:? "AllAwsRegions")
            <*> (x .: "AccountIds")
      )

instance Hashable AccountAggregationSource

instance NFData AccountAggregationSource

instance ToJSON AccountAggregationSource where
  toJSON AccountAggregationSource' {..} =
    object
      ( catMaybes
          [ ("AwsRegions" .=) <$> _aasAWSRegions,
            ("AllAwsRegions" .=) <$> _aasAllAWSRegions,
            Just ("AccountIds" .= _aasAccountIds)
          ]
      )

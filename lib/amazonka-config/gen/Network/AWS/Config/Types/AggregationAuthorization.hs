{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregationAuthorization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that represents the authorizations granted to aggregator accounts and regions.
--
--
--
-- /See:/ 'aggregationAuthorization' smart constructor.
data AggregationAuthorization = AggregationAuthorization'
  { _aaCreationTime ::
      !(Maybe POSIX),
    _aaAuthorizedAWSRegion :: !(Maybe Text),
    _aaAggregationAuthorizationARN ::
      !(Maybe Text),
    _aaAuthorizedAccountId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AggregationAuthorization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaCreationTime' - The time stamp when the aggregation authorization was created.
--
-- * 'aaAuthorizedAWSRegion' - The region authorized to collect aggregated data.
--
-- * 'aaAggregationAuthorizationARN' - The Amazon Resource Name (ARN) of the aggregation object.
--
-- * 'aaAuthorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
aggregationAuthorization ::
  AggregationAuthorization
aggregationAuthorization =
  AggregationAuthorization'
    { _aaCreationTime = Nothing,
      _aaAuthorizedAWSRegion = Nothing,
      _aaAggregationAuthorizationARN = Nothing,
      _aaAuthorizedAccountId = Nothing
    }

-- | The time stamp when the aggregation authorization was created.
aaCreationTime :: Lens' AggregationAuthorization (Maybe UTCTime)
aaCreationTime = lens _aaCreationTime (\s a -> s {_aaCreationTime = a}) . mapping _Time

-- | The region authorized to collect aggregated data.
aaAuthorizedAWSRegion :: Lens' AggregationAuthorization (Maybe Text)
aaAuthorizedAWSRegion = lens _aaAuthorizedAWSRegion (\s a -> s {_aaAuthorizedAWSRegion = a})

-- | The Amazon Resource Name (ARN) of the aggregation object.
aaAggregationAuthorizationARN :: Lens' AggregationAuthorization (Maybe Text)
aaAggregationAuthorizationARN = lens _aaAggregationAuthorizationARN (\s a -> s {_aaAggregationAuthorizationARN = a})

-- | The 12-digit account ID of the account authorized to aggregate data.
aaAuthorizedAccountId :: Lens' AggregationAuthorization (Maybe Text)
aaAuthorizedAccountId = lens _aaAuthorizedAccountId (\s a -> s {_aaAuthorizedAccountId = a})

instance FromJSON AggregationAuthorization where
  parseJSON =
    withObject
      "AggregationAuthorization"
      ( \x ->
          AggregationAuthorization'
            <$> (x .:? "CreationTime")
            <*> (x .:? "AuthorizedAwsRegion")
            <*> (x .:? "AggregationAuthorizationArn")
            <*> (x .:? "AuthorizedAccountId")
      )

instance Hashable AggregationAuthorization

instance NFData AggregationAuthorization

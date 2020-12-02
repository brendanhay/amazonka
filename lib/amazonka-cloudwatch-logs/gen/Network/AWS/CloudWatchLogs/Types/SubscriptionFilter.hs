{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.SubscriptionFilter where

import Network.AWS.CloudWatchLogs.Types.Distribution
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a subscription filter.
--
--
--
-- /See:/ 'subscriptionFilter' smart constructor.
data SubscriptionFilter = SubscriptionFilter'
  { _sfCreationTime ::
      !(Maybe Nat),
    _sfFilterName :: !(Maybe Text),
    _sfDistribution :: !(Maybe Distribution),
    _sfDestinationARN :: !(Maybe Text),
    _sfLogGroupName :: !(Maybe Text),
    _sfFilterPattern :: !(Maybe Text),
    _sfRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SubscriptionFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfCreationTime' - The creation time of the subscription filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'sfFilterName' - The name of the subscription filter.
--
-- * 'sfDistribution' - Undocumented member.
--
-- * 'sfDestinationARN' - The Amazon Resource Name (ARN) of the destination.
--
-- * 'sfLogGroupName' - The name of the log group.
--
-- * 'sfFilterPattern' - Undocumented member.
--
-- * 'sfRoleARN' -
subscriptionFilter ::
  SubscriptionFilter
subscriptionFilter =
  SubscriptionFilter'
    { _sfCreationTime = Nothing,
      _sfFilterName = Nothing,
      _sfDistribution = Nothing,
      _sfDestinationARN = Nothing,
      _sfLogGroupName = Nothing,
      _sfFilterPattern = Nothing,
      _sfRoleARN = Nothing
    }

-- | The creation time of the subscription filter, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
sfCreationTime :: Lens' SubscriptionFilter (Maybe Natural)
sfCreationTime = lens _sfCreationTime (\s a -> s {_sfCreationTime = a}) . mapping _Nat

-- | The name of the subscription filter.
sfFilterName :: Lens' SubscriptionFilter (Maybe Text)
sfFilterName = lens _sfFilterName (\s a -> s {_sfFilterName = a})

-- | Undocumented member.
sfDistribution :: Lens' SubscriptionFilter (Maybe Distribution)
sfDistribution = lens _sfDistribution (\s a -> s {_sfDistribution = a})

-- | The Amazon Resource Name (ARN) of the destination.
sfDestinationARN :: Lens' SubscriptionFilter (Maybe Text)
sfDestinationARN = lens _sfDestinationARN (\s a -> s {_sfDestinationARN = a})

-- | The name of the log group.
sfLogGroupName :: Lens' SubscriptionFilter (Maybe Text)
sfLogGroupName = lens _sfLogGroupName (\s a -> s {_sfLogGroupName = a})

-- | Undocumented member.
sfFilterPattern :: Lens' SubscriptionFilter (Maybe Text)
sfFilterPattern = lens _sfFilterPattern (\s a -> s {_sfFilterPattern = a})

-- |
sfRoleARN :: Lens' SubscriptionFilter (Maybe Text)
sfRoleARN = lens _sfRoleARN (\s a -> s {_sfRoleARN = a})

instance FromJSON SubscriptionFilter where
  parseJSON =
    withObject
      "SubscriptionFilter"
      ( \x ->
          SubscriptionFilter'
            <$> (x .:? "creationTime")
            <*> (x .:? "filterName")
            <*> (x .:? "distribution")
            <*> (x .:? "destinationArn")
            <*> (x .:? "logGroupName")
            <*> (x .:? "filterPattern")
            <*> (x .:? "roleArn")
      )

instance Hashable SubscriptionFilter

instance NFData SubscriptionFilter

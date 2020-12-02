{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Subscription where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Shield.Types.AutoRenew
import Network.AWS.Shield.Types.Limit
import Network.AWS.Shield.Types.ProactiveEngagementStatus
import Network.AWS.Shield.Types.SubscriptionLimits

-- | Information about the AWS Shield Advanced subscription for an account.
--
--
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sTimeCommitmentInSeconds ::
      !(Maybe Nat),
    _sStartTime :: !(Maybe POSIX),
    _sLimits :: !(Maybe [Limit]),
    _sAutoRenew :: !(Maybe AutoRenew),
    _sEndTime :: !(Maybe POSIX),
    _sProactiveEngagementStatus :: !(Maybe ProactiveEngagementStatus),
    _sSubscriptionLimits :: !SubscriptionLimits
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTimeCommitmentInSeconds' - The length, in seconds, of the AWS Shield Advanced subscription for the account.
--
-- * 'sStartTime' - The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
--
-- * 'sLimits' - Specifies how many protections of a given type you can create.
--
-- * 'sAutoRenew' - If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
--
-- * 'sEndTime' - The date and time your subscription will end.
--
-- * 'sProactiveEngagementStatus' - If @ENABLED@ , the DDoS Response Team (DRT) will use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support. If @PENDING@ , you have requested proactive engagement and the request is pending. The status changes to @ENABLED@ when your request is fully processed. If @DISABLED@ , the DRT will not proactively notify contacts about escalations or to initiate proactive customer support.
--
-- * 'sSubscriptionLimits' - Limits settings for your subscription.
subscription ::
  -- | 'sSubscriptionLimits'
  SubscriptionLimits ->
  Subscription
subscription pSubscriptionLimits_ =
  Subscription'
    { _sTimeCommitmentInSeconds = Nothing,
      _sStartTime = Nothing,
      _sLimits = Nothing,
      _sAutoRenew = Nothing,
      _sEndTime = Nothing,
      _sProactiveEngagementStatus = Nothing,
      _sSubscriptionLimits = pSubscriptionLimits_
    }

-- | The length, in seconds, of the AWS Shield Advanced subscription for the account.
sTimeCommitmentInSeconds :: Lens' Subscription (Maybe Natural)
sTimeCommitmentInSeconds = lens _sTimeCommitmentInSeconds (\s a -> s {_sTimeCommitmentInSeconds = a}) . mapping _Nat

-- | The start time of the subscription, in Unix time in seconds. For more information see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp> .
sStartTime :: Lens' Subscription (Maybe UTCTime)
sStartTime = lens _sStartTime (\s a -> s {_sStartTime = a}) . mapping _Time

-- | Specifies how many protections of a given type you can create.
sLimits :: Lens' Subscription [Limit]
sLimits = lens _sLimits (\s a -> s {_sLimits = a}) . _Default . _Coerce

-- | If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
sAutoRenew :: Lens' Subscription (Maybe AutoRenew)
sAutoRenew = lens _sAutoRenew (\s a -> s {_sAutoRenew = a})

-- | The date and time your subscription will end.
sEndTime :: Lens' Subscription (Maybe UTCTime)
sEndTime = lens _sEndTime (\s a -> s {_sEndTime = a}) . mapping _Time

-- | If @ENABLED@ , the DDoS Response Team (DRT) will use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support. If @PENDING@ , you have requested proactive engagement and the request is pending. The status changes to @ENABLED@ when your request is fully processed. If @DISABLED@ , the DRT will not proactively notify contacts about escalations or to initiate proactive customer support.
sProactiveEngagementStatus :: Lens' Subscription (Maybe ProactiveEngagementStatus)
sProactiveEngagementStatus = lens _sProactiveEngagementStatus (\s a -> s {_sProactiveEngagementStatus = a})

-- | Limits settings for your subscription.
sSubscriptionLimits :: Lens' Subscription SubscriptionLimits
sSubscriptionLimits = lens _sSubscriptionLimits (\s a -> s {_sSubscriptionLimits = a})

instance FromJSON Subscription where
  parseJSON =
    withObject
      "Subscription"
      ( \x ->
          Subscription'
            <$> (x .:? "TimeCommitmentInSeconds")
            <*> (x .:? "StartTime")
            <*> (x .:? "Limits" .!= mempty)
            <*> (x .:? "AutoRenew")
            <*> (x .:? "EndTime")
            <*> (x .:? "ProactiveEngagementStatus")
            <*> (x .: "SubscriptionLimits")
      )

instance Hashable Subscription

instance NFData Subscription

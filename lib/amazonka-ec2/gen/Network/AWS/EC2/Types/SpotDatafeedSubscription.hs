{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotDatafeedSubscription where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DatafeedSubscriptionState
import Network.AWS.EC2.Types.SpotInstanceStateFault
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the data feed for a Spot Instance.
--
--
--
-- /See:/ 'spotDatafeedSubscription' smart constructor.
data SpotDatafeedSubscription = SpotDatafeedSubscription'
  { _sdsState ::
      !(Maybe DatafeedSubscriptionState),
    _sdsPrefix :: !(Maybe Text),
    _sdsBucket :: !(Maybe Text),
    _sdsOwnerId :: !(Maybe Text),
    _sdsFault ::
      !(Maybe SpotInstanceStateFault)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsState' - The state of the Spot Instance data feed subscription.
--
-- * 'sdsPrefix' - The prefix for the data feed files.
--
-- * 'sdsBucket' - The name of the Amazon S3 bucket where the Spot Instance data feed is located.
--
-- * 'sdsOwnerId' - The AWS account ID of the account.
--
-- * 'sdsFault' - The fault codes for the Spot Instance request, if any.
spotDatafeedSubscription ::
  SpotDatafeedSubscription
spotDatafeedSubscription =
  SpotDatafeedSubscription'
    { _sdsState = Nothing,
      _sdsPrefix = Nothing,
      _sdsBucket = Nothing,
      _sdsOwnerId = Nothing,
      _sdsFault = Nothing
    }

-- | The state of the Spot Instance data feed subscription.
sdsState :: Lens' SpotDatafeedSubscription (Maybe DatafeedSubscriptionState)
sdsState = lens _sdsState (\s a -> s {_sdsState = a})

-- | The prefix for the data feed files.
sdsPrefix :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsPrefix = lens _sdsPrefix (\s a -> s {_sdsPrefix = a})

-- | The name of the Amazon S3 bucket where the Spot Instance data feed is located.
sdsBucket :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsBucket = lens _sdsBucket (\s a -> s {_sdsBucket = a})

-- | The AWS account ID of the account.
sdsOwnerId :: Lens' SpotDatafeedSubscription (Maybe Text)
sdsOwnerId = lens _sdsOwnerId (\s a -> s {_sdsOwnerId = a})

-- | The fault codes for the Spot Instance request, if any.
sdsFault :: Lens' SpotDatafeedSubscription (Maybe SpotInstanceStateFault)
sdsFault = lens _sdsFault (\s a -> s {_sdsFault = a})

instance FromXML SpotDatafeedSubscription where
  parseXML x =
    SpotDatafeedSubscription'
      <$> (x .@? "state")
      <*> (x .@? "prefix")
      <*> (x .@? "bucket")
      <*> (x .@? "ownerId")
      <*> (x .@? "fault")

instance Hashable SpotDatafeedSubscription

instance NFData SpotDatafeedSubscription

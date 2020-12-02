{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetRequestConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetRequestConfig where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ActivityStatus
import Network.AWS.EC2.Types.BatchState
import Network.AWS.EC2.Types.SpotFleetRequestConfigData
import Network.AWS.EC2.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Spot Fleet request.
--
--
--
-- /See:/ 'spotFleetRequestConfig' smart constructor.
data SpotFleetRequestConfig = SpotFleetRequestConfig'
  { _sfrcSpotFleetRequestConfig ::
      !(Maybe SpotFleetRequestConfigData),
    _sfrcSpotFleetRequestId :: !(Maybe Text),
    _sfrcSpotFleetRequestState ::
      !(Maybe BatchState),
    _sfrcCreateTime :: !(Maybe ISO8601),
    _sfrcTags :: !(Maybe [Tag]),
    _sfrcActivityStatus ::
      !(Maybe ActivityStatus)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpotFleetRequestConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfrcSpotFleetRequestConfig' - The configuration of the Spot Fleet request.
--
-- * 'sfrcSpotFleetRequestId' - The ID of the Spot Fleet request.
--
-- * 'sfrcSpotFleetRequestState' - The state of the Spot Fleet request.
--
-- * 'sfrcCreateTime' - The creation date and time of the request.
--
-- * 'sfrcTags' - The tags for a Spot Fleet resource.
--
-- * 'sfrcActivityStatus' - The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
spotFleetRequestConfig ::
  SpotFleetRequestConfig
spotFleetRequestConfig =
  SpotFleetRequestConfig'
    { _sfrcSpotFleetRequestConfig = Nothing,
      _sfrcSpotFleetRequestId = Nothing,
      _sfrcSpotFleetRequestState = Nothing,
      _sfrcCreateTime = Nothing,
      _sfrcTags = Nothing,
      _sfrcActivityStatus = Nothing
    }

-- | The configuration of the Spot Fleet request.
sfrcSpotFleetRequestConfig :: Lens' SpotFleetRequestConfig (Maybe SpotFleetRequestConfigData)
sfrcSpotFleetRequestConfig = lens _sfrcSpotFleetRequestConfig (\s a -> s {_sfrcSpotFleetRequestConfig = a})

-- | The ID of the Spot Fleet request.
sfrcSpotFleetRequestId :: Lens' SpotFleetRequestConfig (Maybe Text)
sfrcSpotFleetRequestId = lens _sfrcSpotFleetRequestId (\s a -> s {_sfrcSpotFleetRequestId = a})

-- | The state of the Spot Fleet request.
sfrcSpotFleetRequestState :: Lens' SpotFleetRequestConfig (Maybe BatchState)
sfrcSpotFleetRequestState = lens _sfrcSpotFleetRequestState (\s a -> s {_sfrcSpotFleetRequestState = a})

-- | The creation date and time of the request.
sfrcCreateTime :: Lens' SpotFleetRequestConfig (Maybe UTCTime)
sfrcCreateTime = lens _sfrcCreateTime (\s a -> s {_sfrcCreateTime = a}) . mapping _Time

-- | The tags for a Spot Fleet resource.
sfrcTags :: Lens' SpotFleetRequestConfig [Tag]
sfrcTags = lens _sfrcTags (\s a -> s {_sfrcTags = a}) . _Default . _Coerce

-- | The progress of the Spot Fleet request. If there is an error, the status is @error@ . After all requests are placed, the status is @pending_fulfillment@ . If the size of the fleet is equal to or greater than its target capacity, the status is @fulfilled@ . If the size of the fleet is decreased, the status is @pending_termination@ while Spot Instances are terminating.
sfrcActivityStatus :: Lens' SpotFleetRequestConfig (Maybe ActivityStatus)
sfrcActivityStatus = lens _sfrcActivityStatus (\s a -> s {_sfrcActivityStatus = a})

instance FromXML SpotFleetRequestConfig where
  parseXML x =
    SpotFleetRequestConfig'
      <$> (x .@? "spotFleetRequestConfig")
      <*> (x .@? "spotFleetRequestId")
      <*> (x .@? "spotFleetRequestState")
      <*> (x .@? "createTime")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "activityStatus")

instance Hashable SpotFleetRequestConfig

instance NFData SpotFleetRequestConfig

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeLogConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeLogConfigs where

import Network.AWS.CloudFront.Types.RealtimeLogConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of real-time log configurations.
--
--
--
-- /See:/ 'realtimeLogConfigs' smart constructor.
data RealtimeLogConfigs = RealtimeLogConfigs'
  { _rlcItems ::
      !(Maybe [RealtimeLogConfig]),
    _rlcNextMarker :: !(Maybe Text),
    _rlcMaxItems :: !Int,
    _rlcIsTruncated :: !Bool,
    _rlcMarker :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RealtimeLogConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlcItems' - Contains the list of real-time log configurations.
--
-- * 'rlcNextMarker' - If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off.
--
-- * 'rlcMaxItems' - The maximum number of real-time log configurations requested.
--
-- * 'rlcIsTruncated' - A flag that indicates whether there are more real-time log configurations than are contained in this list.
--
-- * 'rlcMarker' - This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
realtimeLogConfigs ::
  -- | 'rlcMaxItems'
  Int ->
  -- | 'rlcIsTruncated'
  Bool ->
  -- | 'rlcMarker'
  Text ->
  RealtimeLogConfigs
realtimeLogConfigs pMaxItems_ pIsTruncated_ pMarker_ =
  RealtimeLogConfigs'
    { _rlcItems = Nothing,
      _rlcNextMarker = Nothing,
      _rlcMaxItems = pMaxItems_,
      _rlcIsTruncated = pIsTruncated_,
      _rlcMarker = pMarker_
    }

-- | Contains the list of real-time log configurations.
rlcItems :: Lens' RealtimeLogConfigs [RealtimeLogConfig]
rlcItems = lens _rlcItems (\s a -> s {_rlcItems = a}) . _Default . _Coerce

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off.
rlcNextMarker :: Lens' RealtimeLogConfigs (Maybe Text)
rlcNextMarker = lens _rlcNextMarker (\s a -> s {_rlcNextMarker = a})

-- | The maximum number of real-time log configurations requested.
rlcMaxItems :: Lens' RealtimeLogConfigs Int
rlcMaxItems = lens _rlcMaxItems (\s a -> s {_rlcMaxItems = a})

-- | A flag that indicates whether there are more real-time log configurations than are contained in this list.
rlcIsTruncated :: Lens' RealtimeLogConfigs Bool
rlcIsTruncated = lens _rlcIsTruncated (\s a -> s {_rlcIsTruncated = a})

-- | This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
rlcMarker :: Lens' RealtimeLogConfigs Text
rlcMarker = lens _rlcMarker (\s a -> s {_rlcMarker = a})

instance FromXML RealtimeLogConfigs where
  parseXML x =
    RealtimeLogConfigs'
      <$> (x .@? "Items" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "NextMarker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "IsTruncated")
      <*> (x .@ "Marker")

instance Hashable RealtimeLogConfigs

instance NFData RealtimeLogConfigs

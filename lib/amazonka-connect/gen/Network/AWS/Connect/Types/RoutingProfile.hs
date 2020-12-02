{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.RoutingProfile where

import Network.AWS.Connect.Types.MediaConcurrency
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a routing profile.
--
--
--
-- /See:/ 'routingProfile' smart constructor.
data RoutingProfile = RoutingProfile'
  { _rpInstanceId ::
      !(Maybe Text),
    _rpRoutingProfileARN :: !(Maybe Text),
    _rpRoutingProfileId :: !(Maybe Text),
    _rpDefaultOutboundQueueId :: !(Maybe Text),
    _rpName :: !(Maybe Text),
    _rpMediaConcurrencies :: !(Maybe [MediaConcurrency]),
    _rpDescription :: !(Maybe Text),
    _rpTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoutingProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpInstanceId' - The identifier of the Amazon Connect instance.
--
-- * 'rpRoutingProfileARN' - The Amazon Resource Name (ARN) of the routing profile.
--
-- * 'rpRoutingProfileId' - The identifier of the routing profile.
--
-- * 'rpDefaultOutboundQueueId' - The identifier of the default outbound queue for this routing profile.
--
-- * 'rpName' - The name of the routing profile.
--
-- * 'rpMediaConcurrencies' - The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
--
-- * 'rpDescription' - The description of the routing profile.
--
-- * 'rpTags' - One or more tags.
routingProfile ::
  RoutingProfile
routingProfile =
  RoutingProfile'
    { _rpInstanceId = Nothing,
      _rpRoutingProfileARN = Nothing,
      _rpRoutingProfileId = Nothing,
      _rpDefaultOutboundQueueId = Nothing,
      _rpName = Nothing,
      _rpMediaConcurrencies = Nothing,
      _rpDescription = Nothing,
      _rpTags = Nothing
    }

-- | The identifier of the Amazon Connect instance.
rpInstanceId :: Lens' RoutingProfile (Maybe Text)
rpInstanceId = lens _rpInstanceId (\s a -> s {_rpInstanceId = a})

-- | The Amazon Resource Name (ARN) of the routing profile.
rpRoutingProfileARN :: Lens' RoutingProfile (Maybe Text)
rpRoutingProfileARN = lens _rpRoutingProfileARN (\s a -> s {_rpRoutingProfileARN = a})

-- | The identifier of the routing profile.
rpRoutingProfileId :: Lens' RoutingProfile (Maybe Text)
rpRoutingProfileId = lens _rpRoutingProfileId (\s a -> s {_rpRoutingProfileId = a})

-- | The identifier of the default outbound queue for this routing profile.
rpDefaultOutboundQueueId :: Lens' RoutingProfile (Maybe Text)
rpDefaultOutboundQueueId = lens _rpDefaultOutboundQueueId (\s a -> s {_rpDefaultOutboundQueueId = a})

-- | The name of the routing profile.
rpName :: Lens' RoutingProfile (Maybe Text)
rpName = lens _rpName (\s a -> s {_rpName = a})

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
rpMediaConcurrencies :: Lens' RoutingProfile [MediaConcurrency]
rpMediaConcurrencies = lens _rpMediaConcurrencies (\s a -> s {_rpMediaConcurrencies = a}) . _Default . _Coerce

-- | The description of the routing profile.
rpDescription :: Lens' RoutingProfile (Maybe Text)
rpDescription = lens _rpDescription (\s a -> s {_rpDescription = a})

-- | One or more tags.
rpTags :: Lens' RoutingProfile (HashMap Text (Text))
rpTags = lens _rpTags (\s a -> s {_rpTags = a}) . _Default . _Map

instance FromJSON RoutingProfile where
  parseJSON =
    withObject
      "RoutingProfile"
      ( \x ->
          RoutingProfile'
            <$> (x .:? "InstanceId")
            <*> (x .:? "RoutingProfileArn")
            <*> (x .:? "RoutingProfileId")
            <*> (x .:? "DefaultOutboundQueueId")
            <*> (x .:? "Name")
            <*> (x .:? "MediaConcurrencies" .!= mempty)
            <*> (x .:? "Description")
            <*> (x .:? "Tags" .!= mempty)
      )

instance Hashable RoutingProfile

instance NFData RoutingProfile

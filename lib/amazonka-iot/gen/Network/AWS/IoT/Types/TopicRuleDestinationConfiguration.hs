{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestinationConfiguration where

import Network.AWS.IoT.Types.HTTPURLDestinationConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration of the topic rule destination.
--
--
--
-- /See:/ 'topicRuleDestinationConfiguration' smart constructor.
newtype TopicRuleDestinationConfiguration = TopicRuleDestinationConfiguration'
  { _trdcHttpURLConfiguration ::
      Maybe
        HTTPURLDestinationConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TopicRuleDestinationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trdcHttpURLConfiguration' - Configuration of the HTTP URL.
topicRuleDestinationConfiguration ::
  TopicRuleDestinationConfiguration
topicRuleDestinationConfiguration =
  TopicRuleDestinationConfiguration'
    { _trdcHttpURLConfiguration =
        Nothing
    }

-- | Configuration of the HTTP URL.
trdcHttpURLConfiguration :: Lens' TopicRuleDestinationConfiguration (Maybe HTTPURLDestinationConfiguration)
trdcHttpURLConfiguration = lens _trdcHttpURLConfiguration (\s a -> s {_trdcHttpURLConfiguration = a})

instance Hashable TopicRuleDestinationConfiguration

instance NFData TopicRuleDestinationConfiguration

instance ToJSON TopicRuleDestinationConfiguration where
  toJSON TopicRuleDestinationConfiguration' {..} =
    object
      ( catMaybes
          [("httpUrlConfiguration" .=) <$> _trdcHttpURLConfiguration]
      )

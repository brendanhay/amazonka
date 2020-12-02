{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestination where

import Network.AWS.IoT.Types.HTTPURLDestinationProperties
import Network.AWS.IoT.Types.TopicRuleDestinationStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A topic rule destination.
--
--
--
-- /See:/ 'topicRuleDestination' smart constructor.
data TopicRuleDestination = TopicRuleDestination'
  { _trdStatus ::
      !(Maybe TopicRuleDestinationStatus),
    _trdHttpURLProperties ::
      !(Maybe HTTPURLDestinationProperties),
    _trdArn :: !(Maybe Text),
    _trdStatusReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TopicRuleDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trdStatus' - The status of the topic rule destination. Valid values are:     * IN_PROGRESS    * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.     * ENABLED    * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .     * DISABLED    * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .     * ERROR    * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
-- * 'trdHttpURLProperties' - Properties of the HTTP URL.
--
-- * 'trdArn' - The topic rule destination URL.
--
-- * 'trdStatusReason' - Additional details or reason why the topic rule destination is in the current status.
topicRuleDestination ::
  TopicRuleDestination
topicRuleDestination =
  TopicRuleDestination'
    { _trdStatus = Nothing,
      _trdHttpURLProperties = Nothing,
      _trdArn = Nothing,
      _trdStatusReason = Nothing
    }

-- | The status of the topic rule destination. Valid values are:     * IN_PROGRESS    * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.     * ENABLED    * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .     * DISABLED    * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .     * ERROR    * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
trdStatus :: Lens' TopicRuleDestination (Maybe TopicRuleDestinationStatus)
trdStatus = lens _trdStatus (\s a -> s {_trdStatus = a})

-- | Properties of the HTTP URL.
trdHttpURLProperties :: Lens' TopicRuleDestination (Maybe HTTPURLDestinationProperties)
trdHttpURLProperties = lens _trdHttpURLProperties (\s a -> s {_trdHttpURLProperties = a})

-- | The topic rule destination URL.
trdArn :: Lens' TopicRuleDestination (Maybe Text)
trdArn = lens _trdArn (\s a -> s {_trdArn = a})

-- | Additional details or reason why the topic rule destination is in the current status.
trdStatusReason :: Lens' TopicRuleDestination (Maybe Text)
trdStatusReason = lens _trdStatusReason (\s a -> s {_trdStatusReason = a})

instance FromJSON TopicRuleDestination where
  parseJSON =
    withObject
      "TopicRuleDestination"
      ( \x ->
          TopicRuleDestination'
            <$> (x .:? "status")
            <*> (x .:? "httpUrlProperties")
            <*> (x .:? "arn")
            <*> (x .:? "statusReason")
      )

instance Hashable TopicRuleDestination

instance NFData TopicRuleDestination

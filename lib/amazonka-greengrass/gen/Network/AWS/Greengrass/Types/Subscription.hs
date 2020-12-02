{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Subscription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a subscription.
--
-- /See:/ 'subscription' smart constructor.
data Subscription = Subscription'
  { _sTarget :: !Text,
    _sId :: !Text,
    _sSubject :: !Text,
    _sSource :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sTarget' - Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
--
-- * 'sId' - A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- * 'sSubject' - The MQTT topic used to route the message.
--
-- * 'sSource' - The source of the subscription. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
subscription ::
  -- | 'sTarget'
  Text ->
  -- | 'sId'
  Text ->
  -- | 'sSubject'
  Text ->
  -- | 'sSource'
  Text ->
  Subscription
subscription pTarget_ pId_ pSubject_ pSource_ =
  Subscription'
    { _sTarget = pTarget_,
      _sId = pId_,
      _sSubject = pSubject_,
      _sSource = pSource_
    }

-- | Where the message is sent to. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
sTarget :: Lens' Subscription Text
sTarget = lens _sTarget (\s a -> s {_sTarget = a})

-- | A descriptive or arbitrary ID for the subscription. This value must be unique within the subscription definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
sId :: Lens' Subscription Text
sId = lens _sId (\s a -> s {_sId = a})

-- | The MQTT topic used to route the message.
sSubject :: Lens' Subscription Text
sSubject = lens _sSubject (\s a -> s {_sSubject = a})

-- | The source of the subscription. Can be a thing ARN, a Lambda function ARN, a connector ARN, 'cloud' (which represents the AWS IoT cloud), or 'GGShadowService'.
sSource :: Lens' Subscription Text
sSource = lens _sSource (\s a -> s {_sSource = a})

instance FromJSON Subscription where
  parseJSON =
    withObject
      "Subscription"
      ( \x ->
          Subscription'
            <$> (x .: "Target")
            <*> (x .: "Id")
            <*> (x .: "Subject")
            <*> (x .: "Source")
      )

instance Hashable Subscription

instance NFData Subscription

instance ToJSON Subscription where
  toJSON Subscription' {..} =
    object
      ( catMaybes
          [ Just ("Target" .= _sTarget),
            Just ("Id" .= _sId),
            Just ("Subject" .= _sSubject),
            Just ("Source" .= _sSource)
          ]
      )

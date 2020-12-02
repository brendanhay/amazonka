{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.RepublishAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.RepublishAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to republish to another topic.
--
--
--
-- /See:/ 'republishAction' smart constructor.
data RepublishAction = RepublishAction'
  { _raQos :: !(Maybe Nat),
    _raRoleARN :: !Text,
    _raTopic :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RepublishAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raQos' - The Quality of Service (QoS) level to use when republishing messages. The default value is 0.
--
-- * 'raRoleARN' - The ARN of the IAM role that grants access.
--
-- * 'raTopic' - The name of the MQTT topic.
republishAction ::
  -- | 'raRoleARN'
  Text ->
  -- | 'raTopic'
  Text ->
  RepublishAction
republishAction pRoleARN_ pTopic_ =
  RepublishAction'
    { _raQos = Nothing,
      _raRoleARN = pRoleARN_,
      _raTopic = pTopic_
    }

-- | The Quality of Service (QoS) level to use when republishing messages. The default value is 0.
raQos :: Lens' RepublishAction (Maybe Natural)
raQos = lens _raQos (\s a -> s {_raQos = a}) . mapping _Nat

-- | The ARN of the IAM role that grants access.
raRoleARN :: Lens' RepublishAction Text
raRoleARN = lens _raRoleARN (\s a -> s {_raRoleARN = a})

-- | The name of the MQTT topic.
raTopic :: Lens' RepublishAction Text
raTopic = lens _raTopic (\s a -> s {_raTopic = a})

instance FromJSON RepublishAction where
  parseJSON =
    withObject
      "RepublishAction"
      ( \x ->
          RepublishAction'
            <$> (x .:? "qos") <*> (x .: "roleArn") <*> (x .: "topic")
      )

instance Hashable RepublishAction

instance NFData RepublishAction

instance ToJSON RepublishAction where
  toJSON RepublishAction' {..} =
    object
      ( catMaybes
          [ ("qos" .=) <$> _raQos,
            Just ("roleArn" .= _raRoleARN),
            Just ("topic" .= _raTopic)
          ]
      )

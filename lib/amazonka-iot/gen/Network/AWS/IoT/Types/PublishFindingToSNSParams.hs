{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PublishFindingToSNSParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PublishFindingToSNSParams where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameters to define a mitigation action that publishes findings to Amazon SNS. You can implement your own custom actions in response to the Amazon SNS messages.
--
--
--
-- /See:/ 'publishFindingToSNSParams' smart constructor.
newtype PublishFindingToSNSParams = PublishFindingToSNSParams'
  { _pftspTopicARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublishFindingToSNSParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pftspTopicARN' - The ARN of the topic to which you want to publish the findings.
publishFindingToSNSParams ::
  -- | 'pftspTopicARN'
  Text ->
  PublishFindingToSNSParams
publishFindingToSNSParams pTopicARN_ =
  PublishFindingToSNSParams' {_pftspTopicARN = pTopicARN_}

-- | The ARN of the topic to which you want to publish the findings.
pftspTopicARN :: Lens' PublishFindingToSNSParams Text
pftspTopicARN = lens _pftspTopicARN (\s a -> s {_pftspTopicARN = a})

instance FromJSON PublishFindingToSNSParams where
  parseJSON =
    withObject
      "PublishFindingToSNSParams"
      (\x -> PublishFindingToSNSParams' <$> (x .: "topicArn"))

instance Hashable PublishFindingToSNSParams

instance NFData PublishFindingToSNSParams

instance ToJSON PublishFindingToSNSParams where
  toJSON PublishFindingToSNSParams' {..} =
    object (catMaybes [Just ("topicArn" .= _pftspTopicARN)])

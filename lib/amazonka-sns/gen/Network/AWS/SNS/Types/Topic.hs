{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Topic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.Topic where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A wrapper type for the topic's Amazon Resource Name (ARN). To retrieve a topic's attributes, use @GetTopicAttributes@ .
--
--
--
-- /See:/ 'topic' smart constructor.
newtype Topic = Topic' {_tTopicARN :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Topic' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tTopicARN' - The topic's ARN.
topic ::
  Topic
topic = Topic' {_tTopicARN = Nothing}

-- | The topic's ARN.
tTopicARN :: Lens' Topic (Maybe Text)
tTopicARN = lens _tTopicARN (\s a -> s {_tTopicARN = a})

instance FromXML Topic where
  parseXML x = Topic' <$> (x .@? "TopicArn")

instance Hashable Topic

instance NFData Topic

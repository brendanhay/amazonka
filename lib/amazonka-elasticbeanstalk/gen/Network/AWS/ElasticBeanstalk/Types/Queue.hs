{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Queue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Queue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a queue.
--
--
--
-- /See:/ 'queue' smart constructor.
data Queue = Queue'
  { _qURL :: !(Maybe Text),
    _qName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Queue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qURL' - The URL of the queue.
--
-- * 'qName' - The name of the queue.
queue ::
  Queue
queue = Queue' {_qURL = Nothing, _qName = Nothing}

-- | The URL of the queue.
qURL :: Lens' Queue (Maybe Text)
qURL = lens _qURL (\s a -> s {_qURL = a})

-- | The name of the queue.
qName :: Lens' Queue (Maybe Text)
qName = lens _qName (\s a -> s {_qName = a})

instance FromXML Queue where
  parseXML x = Queue' <$> (x .@? "URL") <*> (x .@? "Name")

instance Hashable Queue

instance NFData Queue

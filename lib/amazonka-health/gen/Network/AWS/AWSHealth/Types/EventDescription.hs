{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The detailed description of the event. Included in the information returned by the <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails> operation.
--
--
--
-- /See:/ 'eventDescription' smart constructor.
newtype EventDescription = EventDescription'
  { _edLatestDescription ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edLatestDescription' - The most recent description of the event.
eventDescription ::
  EventDescription
eventDescription =
  EventDescription' {_edLatestDescription = Nothing}

-- | The most recent description of the event.
edLatestDescription :: Lens' EventDescription (Maybe Text)
edLatestDescription = lens _edLatestDescription (\s a -> s {_edLatestDescription = a})

instance FromJSON EventDescription where
  parseJSON =
    withObject
      "EventDescription"
      (\x -> EventDescription' <$> (x .:? "latestDescription"))

instance Hashable EventDescription

instance NFData EventDescription

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.EventTriggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.EventTriggerDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The container for the 'EventTriggerDefinition$EventResourceARN' .
--
--
--
-- /See:/ 'eventTriggerDefinition' smart constructor.
newtype EventTriggerDefinition = EventTriggerDefinition'
  { _etdEventResourceARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EventTriggerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etdEventResourceARN' - The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
eventTriggerDefinition ::
  EventTriggerDefinition
eventTriggerDefinition =
  EventTriggerDefinition' {_etdEventResourceARN = Nothing}

-- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is an AWS Lambda function's event trigger associated with this job.
etdEventResourceARN :: Lens' EventTriggerDefinition (Maybe Text)
etdEventResourceARN = lens _etdEventResourceARN (\s a -> s {_etdEventResourceARN = a})

instance FromJSON EventTriggerDefinition where
  parseJSON =
    withObject
      "EventTriggerDefinition"
      (\x -> EventTriggerDefinition' <$> (x .:? "EventResourceARN"))

instance Hashable EventTriggerDefinition

instance NFData EventTriggerDefinition

instance ToJSON EventTriggerDefinition where
  toJSON EventTriggerDefinition' {..} =
    object
      (catMaybes [("EventResourceARN" .=) <$> _etdEventResourceARN])

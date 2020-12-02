{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Schedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Schedule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The schedule for when to trigger an update.
--
--
--
-- /See:/ 'schedule' smart constructor.
newtype Schedule = Schedule' {_sExpression :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sExpression' - The expression that defines when to trigger an update. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules> in the /Amazon CloudWatch Events User Guide/ .
schedule ::
  Schedule
schedule = Schedule' {_sExpression = Nothing}

-- | The expression that defines when to trigger an update. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html Schedule Expressions for Rules> in the /Amazon CloudWatch Events User Guide/ .
sExpression :: Lens' Schedule (Maybe Text)
sExpression = lens _sExpression (\s a -> s {_sExpression = a})

instance FromJSON Schedule where
  parseJSON =
    withObject "Schedule" (\x -> Schedule' <$> (x .:? "expression"))

instance Hashable Schedule

instance NFData Schedule

instance ToJSON Schedule where
  toJSON Schedule' {..} =
    object (catMaybes [("expression" .=) <$> _sExpression])

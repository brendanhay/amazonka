{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyStateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyStateRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.State
import Network.AWS.Prelude

-- | Changes the status of a journey.
--
--
--
-- /See:/ 'journeyStateRequest' smart constructor.
newtype JourneyStateRequest = JourneyStateRequest'
  { _jsrState ::
      Maybe State
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneyStateRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsrState' - The status of the journey. Currently, the only supported value is CANCELLED. If you cancel a journey, Amazon Pinpoint continues to perform activities that are currently in progress, until those activities are complete. Amazon Pinpoint also continues to collect and aggregate analytics data for those activities, until they are complete, and any activities that were complete when you cancelled the journey. After you cancel a journey, you can't add, change, or remove any activities from the journey. In addition, Amazon Pinpoint stops evaluating the journey and doesn't perform any activities that haven't started.
journeyStateRequest ::
  JourneyStateRequest
journeyStateRequest = JourneyStateRequest' {_jsrState = Nothing}

-- | The status of the journey. Currently, the only supported value is CANCELLED. If you cancel a journey, Amazon Pinpoint continues to perform activities that are currently in progress, until those activities are complete. Amazon Pinpoint also continues to collect and aggregate analytics data for those activities, until they are complete, and any activities that were complete when you cancelled the journey. After you cancel a journey, you can't add, change, or remove any activities from the journey. In addition, Amazon Pinpoint stops evaluating the journey and doesn't perform any activities that haven't started.
jsrState :: Lens' JourneyStateRequest (Maybe State)
jsrState = lens _jsrState (\s a -> s {_jsrState = a})

instance Hashable JourneyStateRequest

instance NFData JourneyStateRequest

instance ToJSON JourneyStateRequest where
  toJSON JourneyStateRequest' {..} =
    object (catMaybes [("State" .=) <$> _jsrState])

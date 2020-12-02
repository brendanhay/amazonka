{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.CloudWatchEventsExecutionDataDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.CloudWatchEventsExecutionDataDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides details about execution input or output.
--
--
--
-- /See:/ 'cloudWatchEventsExecutionDataDetails' smart constructor.
newtype CloudWatchEventsExecutionDataDetails = CloudWatchEventsExecutionDataDetails'
  { _cweeddIncluded ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchEventsExecutionDataDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cweeddIncluded' - Indicates whether input or output was included in the response. Always @true@ for API calls.
cloudWatchEventsExecutionDataDetails ::
  CloudWatchEventsExecutionDataDetails
cloudWatchEventsExecutionDataDetails =
  CloudWatchEventsExecutionDataDetails' {_cweeddIncluded = Nothing}

-- | Indicates whether input or output was included in the response. Always @true@ for API calls.
cweeddIncluded :: Lens' CloudWatchEventsExecutionDataDetails (Maybe Bool)
cweeddIncluded = lens _cweeddIncluded (\s a -> s {_cweeddIncluded = a})

instance FromJSON CloudWatchEventsExecutionDataDetails where
  parseJSON =
    withObject
      "CloudWatchEventsExecutionDataDetails"
      ( \x ->
          CloudWatchEventsExecutionDataDetails' <$> (x .:? "included")
      )

instance Hashable CloudWatchEventsExecutionDataDetails

instance NFData CloudWatchEventsExecutionDataDetails

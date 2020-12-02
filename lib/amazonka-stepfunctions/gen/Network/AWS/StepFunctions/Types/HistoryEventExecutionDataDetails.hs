{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides details about input or output in an execution history event.
--
--
--
-- /See:/ 'historyEventExecutionDataDetails' smart constructor.
newtype HistoryEventExecutionDataDetails = HistoryEventExecutionDataDetails'
  { _heeddTruncated ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HistoryEventExecutionDataDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heeddTruncated' - Indicates whether input or output was truncated in the response. Always @false@ for API calls.
historyEventExecutionDataDetails ::
  HistoryEventExecutionDataDetails
historyEventExecutionDataDetails =
  HistoryEventExecutionDataDetails' {_heeddTruncated = Nothing}

-- | Indicates whether input or output was truncated in the response. Always @false@ for API calls.
heeddTruncated :: Lens' HistoryEventExecutionDataDetails (Maybe Bool)
heeddTruncated = lens _heeddTruncated (\s a -> s {_heeddTruncated = a})

instance FromJSON HistoryEventExecutionDataDetails where
  parseJSON =
    withObject
      "HistoryEventExecutionDataDetails"
      (\x -> HistoryEventExecutionDataDetails' <$> (x .:? "truncated"))

instance Hashable HistoryEventExecutionDataDetails

instance NFData HistoryEventExecutionDataDetails

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutEventsResultEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an event that failed to be submitted.
--
--
--
-- /See:/ 'putEventsResultEntry' smart constructor.
data PutEventsResultEntry = PutEventsResultEntry'
  { _pereErrorCode ::
      !(Maybe Text),
    _pereErrorMessage :: !(Maybe Text),
    _pereEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutEventsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pereErrorCode' - The error code that indicates why the event submission failed.
--
-- * 'pereErrorMessage' - The error message that explains why the event submission failed.
--
-- * 'pereEventId' - The ID of the event.
putEventsResultEntry ::
  PutEventsResultEntry
putEventsResultEntry =
  PutEventsResultEntry'
    { _pereErrorCode = Nothing,
      _pereErrorMessage = Nothing,
      _pereEventId = Nothing
    }

-- | The error code that indicates why the event submission failed.
pereErrorCode :: Lens' PutEventsResultEntry (Maybe Text)
pereErrorCode = lens _pereErrorCode (\s a -> s {_pereErrorCode = a})

-- | The error message that explains why the event submission failed.
pereErrorMessage :: Lens' PutEventsResultEntry (Maybe Text)
pereErrorMessage = lens _pereErrorMessage (\s a -> s {_pereErrorMessage = a})

-- | The ID of the event.
pereEventId :: Lens' PutEventsResultEntry (Maybe Text)
pereEventId = lens _pereEventId (\s a -> s {_pereEventId = a})

instance FromJSON PutEventsResultEntry where
  parseJSON =
    withObject
      "PutEventsResultEntry"
      ( \x ->
          PutEventsResultEntry'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "EventId")
      )

instance Hashable PutEventsResultEntry

instance NFData PutEventsResultEntry

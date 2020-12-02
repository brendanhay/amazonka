{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutPartnerEventsResultEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an event that a partner tried to generate, but failed.
--
--
--
-- /See:/ 'putPartnerEventsResultEntry' smart constructor.
data PutPartnerEventsResultEntry = PutPartnerEventsResultEntry'
  { _ppereErrorCode ::
      !(Maybe Text),
    _ppereErrorMessage :: !(Maybe Text),
    _ppereEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutPartnerEventsResultEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppereErrorCode' - The error code that indicates why the event submission failed.
--
-- * 'ppereErrorMessage' - The error message that explains why the event submission failed.
--
-- * 'ppereEventId' - The ID of the event.
putPartnerEventsResultEntry ::
  PutPartnerEventsResultEntry
putPartnerEventsResultEntry =
  PutPartnerEventsResultEntry'
    { _ppereErrorCode = Nothing,
      _ppereErrorMessage = Nothing,
      _ppereEventId = Nothing
    }

-- | The error code that indicates why the event submission failed.
ppereErrorCode :: Lens' PutPartnerEventsResultEntry (Maybe Text)
ppereErrorCode = lens _ppereErrorCode (\s a -> s {_ppereErrorCode = a})

-- | The error message that explains why the event submission failed.
ppereErrorMessage :: Lens' PutPartnerEventsResultEntry (Maybe Text)
ppereErrorMessage = lens _ppereErrorMessage (\s a -> s {_ppereErrorMessage = a})

-- | The ID of the event.
ppereEventId :: Lens' PutPartnerEventsResultEntry (Maybe Text)
ppereEventId = lens _ppereEventId (\s a -> s {_ppereEventId = a})

instance FromJSON PutPartnerEventsResultEntry where
  parseJSON =
    withObject
      "PutPartnerEventsResultEntry"
      ( \x ->
          PutPartnerEventsResultEntry'
            <$> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
            <*> (x .:? "EventId")
      )

instance Hashable PutPartnerEventsResultEntry

instance NFData PutPartnerEventsResultEntry

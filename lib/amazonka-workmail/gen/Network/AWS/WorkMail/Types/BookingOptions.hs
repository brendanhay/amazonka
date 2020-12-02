{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.BookingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.BookingOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | At least one delegate must be associated to the resource to disable automatic replies from the resource.
--
--
--
-- /See:/ 'bookingOptions' smart constructor.
data BookingOptions = BookingOptions'
  { _boAutoDeclineConflictingRequests ::
      !(Maybe Bool),
    _boAutoDeclineRecurringRequests :: !(Maybe Bool),
    _boAutoAcceptRequests :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BookingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'boAutoDeclineConflictingRequests' - The resource's ability to automatically decline any conflicting requests.
--
-- * 'boAutoDeclineRecurringRequests' - The resource's ability to automatically decline any recurring requests.
--
-- * 'boAutoAcceptRequests' - The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
bookingOptions ::
  BookingOptions
bookingOptions =
  BookingOptions'
    { _boAutoDeclineConflictingRequests = Nothing,
      _boAutoDeclineRecurringRequests = Nothing,
      _boAutoAcceptRequests = Nothing
    }

-- | The resource's ability to automatically decline any conflicting requests.
boAutoDeclineConflictingRequests :: Lens' BookingOptions (Maybe Bool)
boAutoDeclineConflictingRequests = lens _boAutoDeclineConflictingRequests (\s a -> s {_boAutoDeclineConflictingRequests = a})

-- | The resource's ability to automatically decline any recurring requests.
boAutoDeclineRecurringRequests :: Lens' BookingOptions (Maybe Bool)
boAutoDeclineRecurringRequests = lens _boAutoDeclineRecurringRequests (\s a -> s {_boAutoDeclineRecurringRequests = a})

-- | The resource's ability to automatically reply to requests. If disabled, delegates must be associated to the resource.
boAutoAcceptRequests :: Lens' BookingOptions (Maybe Bool)
boAutoAcceptRequests = lens _boAutoAcceptRequests (\s a -> s {_boAutoAcceptRequests = a})

instance FromJSON BookingOptions where
  parseJSON =
    withObject
      "BookingOptions"
      ( \x ->
          BookingOptions'
            <$> (x .:? "AutoDeclineConflictingRequests")
            <*> (x .:? "AutoDeclineRecurringRequests")
            <*> (x .:? "AutoAcceptRequests")
      )

instance Hashable BookingOptions

instance NFData BookingOptions

instance ToJSON BookingOptions where
  toJSON BookingOptions' {..} =
    object
      ( catMaybes
          [ ("AutoDeclineConflictingRequests" .=)
              <$> _boAutoDeclineConflictingRequests,
            ("AutoDeclineRecurringRequests" .=)
              <$> _boAutoDeclineRecurringRequests,
            ("AutoAcceptRequests" .=) <$> _boAutoAcceptRequests
          ]
      )

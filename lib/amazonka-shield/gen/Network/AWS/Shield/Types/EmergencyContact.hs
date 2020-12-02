{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.EmergencyContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.EmergencyContact where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contact information that the DRT can use to contact you if you have proactive engagement enabled, for escalations to the DRT and to initiate proactive customer support.
--
--
--
-- /See:/ 'emergencyContact' smart constructor.
data EmergencyContact = EmergencyContact'
  { _ecPhoneNumber ::
      !(Maybe Text),
    _ecContactNotes :: !(Maybe Text),
    _ecEmailAddress :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmergencyContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecPhoneNumber' - The phone number for the contact.
--
-- * 'ecContactNotes' - Additional notes regarding the contact.
--
-- * 'ecEmailAddress' - The email address for the contact.
emergencyContact ::
  -- | 'ecEmailAddress'
  Text ->
  EmergencyContact
emergencyContact pEmailAddress_ =
  EmergencyContact'
    { _ecPhoneNumber = Nothing,
      _ecContactNotes = Nothing,
      _ecEmailAddress = pEmailAddress_
    }

-- | The phone number for the contact.
ecPhoneNumber :: Lens' EmergencyContact (Maybe Text)
ecPhoneNumber = lens _ecPhoneNumber (\s a -> s {_ecPhoneNumber = a})

-- | Additional notes regarding the contact.
ecContactNotes :: Lens' EmergencyContact (Maybe Text)
ecContactNotes = lens _ecContactNotes (\s a -> s {_ecContactNotes = a})

-- | The email address for the contact.
ecEmailAddress :: Lens' EmergencyContact Text
ecEmailAddress = lens _ecEmailAddress (\s a -> s {_ecEmailAddress = a})

instance FromJSON EmergencyContact where
  parseJSON =
    withObject
      "EmergencyContact"
      ( \x ->
          EmergencyContact'
            <$> (x .:? "PhoneNumber")
            <*> (x .:? "ContactNotes")
            <*> (x .: "EmailAddress")
      )

instance Hashable EmergencyContact

instance NFData EmergencyContact

instance ToJSON EmergencyContact where
  toJSON EmergencyContact' {..} =
    object
      ( catMaybes
          [ ("PhoneNumber" .=) <$> _ecPhoneNumber,
            ("ContactNotes" .=) <$> _ecContactNotes,
            Just ("EmailAddress" .= _ecEmailAddress)
          ]
      )

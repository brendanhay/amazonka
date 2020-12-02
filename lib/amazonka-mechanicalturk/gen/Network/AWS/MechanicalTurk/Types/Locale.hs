{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Locale
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Locale where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Locale data structure represents a geographical region or location.
--
--
--
-- /See:/ 'locale' smart constructor.
data Locale = Locale'
  { _lSubdivision :: !(Maybe Text),
    _lCountry :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Locale' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lSubdivision' - The state or subdivision of the locale. A valid ISO 3166-2 subdivision code. For example, the code WA refers to the state of Washington.
--
-- * 'lCountry' - The country of the locale. Must be a valid ISO 3166 country code. For example, the code US refers to the United States of America.
locale ::
  -- | 'lCountry'
  Text ->
  Locale
locale pCountry_ =
  Locale' {_lSubdivision = Nothing, _lCountry = pCountry_}

-- | The state or subdivision of the locale. A valid ISO 3166-2 subdivision code. For example, the code WA refers to the state of Washington.
lSubdivision :: Lens' Locale (Maybe Text)
lSubdivision = lens _lSubdivision (\s a -> s {_lSubdivision = a})

-- | The country of the locale. Must be a valid ISO 3166 country code. For example, the code US refers to the United States of America.
lCountry :: Lens' Locale Text
lCountry = lens _lCountry (\s a -> s {_lCountry = a})

instance FromJSON Locale where
  parseJSON =
    withObject
      "Locale"
      (\x -> Locale' <$> (x .:? "Subdivision") <*> (x .: "Country"))

instance Hashable Locale

instance NFData Locale

instance ToJSON Locale where
  toJSON Locale' {..} =
    object
      ( catMaybes
          [ ("Subdivision" .=) <$> _lSubdivision,
            Just ("Country" .= _lCountry)
          ]
      )

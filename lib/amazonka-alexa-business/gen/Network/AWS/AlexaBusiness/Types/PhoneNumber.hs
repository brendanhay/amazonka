{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.PhoneNumber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.PhoneNumber where

import Network.AWS.AlexaBusiness.Types.PhoneNumberType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The phone number for the contact containing the raw number and phone number type.
--
--
--
-- /See:/ 'phoneNumber' smart constructor.
data PhoneNumber = PhoneNumber'
  { _pnNumber :: !(Sensitive Text),
    _pnType :: !(Sensitive PhoneNumberType)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PhoneNumber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnNumber' - The raw value of the phone number.
--
-- * 'pnType' - The type of the phone number.
phoneNumber ::
  -- | 'pnNumber'
  Text ->
  -- | 'pnType'
  PhoneNumberType ->
  PhoneNumber
phoneNumber pNumber_ pType_ =
  PhoneNumber'
    { _pnNumber = _Sensitive # pNumber_,
      _pnType = _Sensitive # pType_
    }

-- | The raw value of the phone number.
pnNumber :: Lens' PhoneNumber Text
pnNumber = lens _pnNumber (\s a -> s {_pnNumber = a}) . _Sensitive

-- | The type of the phone number.
pnType :: Lens' PhoneNumber PhoneNumberType
pnType = lens _pnType (\s a -> s {_pnType = a}) . _Sensitive

instance FromJSON PhoneNumber where
  parseJSON =
    withObject
      "PhoneNumber"
      (\x -> PhoneNumber' <$> (x .: "Number") <*> (x .: "Type"))

instance Hashable PhoneNumber

instance NFData PhoneNumber

instance ToJSON PhoneNumber where
  toJSON PhoneNumber' {..} =
    object
      ( catMaybes
          [Just ("Number" .= _pnNumber), Just ("Type" .= _pnType)]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SipAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SipAddress where

import Network.AWS.AlexaBusiness.Types.SipType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The SIP address for the contact containing the URI and SIP address type.
--
--
--
-- /See:/ 'sipAddress' smart constructor.
data SipAddress = SipAddress'
  { _saURI :: !(Sensitive Text),
    _saType :: !(Sensitive SipType)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'SipAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saURI' - The URI for the SIP address.
--
-- * 'saType' - The type of the SIP address.
sipAddress ::
  -- | 'saURI'
  Text ->
  -- | 'saType'
  SipType ->
  SipAddress
sipAddress pURI_ pType_ =
  SipAddress'
    { _saURI = _Sensitive # pURI_,
      _saType = _Sensitive # pType_
    }

-- | The URI for the SIP address.
saURI :: Lens' SipAddress Text
saURI = lens _saURI (\s a -> s {_saURI = a}) . _Sensitive

-- | The type of the SIP address.
saType :: Lens' SipAddress SipType
saType = lens _saType (\s a -> s {_saType = a}) . _Sensitive

instance FromJSON SipAddress where
  parseJSON =
    withObject
      "SipAddress"
      (\x -> SipAddress' <$> (x .: "Uri") <*> (x .: "Type"))

instance Hashable SipAddress

instance NFData SipAddress

instance ToJSON SipAddress where
  toJSON SipAddress' {..} =
    object
      (catMaybes [Just ("Uri" .= _saURI), Just ("Type" .= _saType)])

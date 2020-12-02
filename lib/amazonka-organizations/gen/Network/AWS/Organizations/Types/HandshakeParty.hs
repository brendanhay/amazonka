{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeParty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeParty where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.HandshakePartyType
import Network.AWS.Prelude

-- | Identifies a participant in a handshake.
--
--
--
-- /See:/ 'handshakeParty' smart constructor.
data HandshakeParty = HandshakeParty'
  { _hpId :: !(Sensitive Text),
    _hpType :: !HandshakePartyType
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HandshakeParty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpId' - The unique identifier (ID) for the party. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- * 'hpType' - The type of party.
handshakeParty ::
  -- | 'hpId'
  Text ->
  -- | 'hpType'
  HandshakePartyType ->
  HandshakeParty
handshakeParty pId_ pType_ =
  HandshakeParty' {_hpId = _Sensitive # pId_, _hpType = pType_}

-- | The unique identifier (ID) for the party. The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
hpId :: Lens' HandshakeParty Text
hpId = lens _hpId (\s a -> s {_hpId = a}) . _Sensitive

-- | The type of party.
hpType :: Lens' HandshakeParty HandshakePartyType
hpType = lens _hpType (\s a -> s {_hpType = a})

instance FromJSON HandshakeParty where
  parseJSON =
    withObject
      "HandshakeParty"
      (\x -> HandshakeParty' <$> (x .: "Id") <*> (x .: "Type"))

instance Hashable HandshakeParty

instance NFData HandshakeParty

instance ToJSON HandshakeParty where
  toJSON HandshakeParty' {..} =
    object
      (catMaybes [Just ("Id" .= _hpId), Just ("Type" .= _hpType)])

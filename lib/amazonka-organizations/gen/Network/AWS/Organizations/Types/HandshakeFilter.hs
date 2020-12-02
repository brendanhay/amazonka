{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeFilter where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.ActionType
import Network.AWS.Prelude

-- | Specifies the criteria that are used to select the handshakes for the operation.
--
--
--
-- /See:/ 'handshakeFilter' smart constructor.
data HandshakeFilter = HandshakeFilter'
  { _hfParentHandshakeId ::
      !(Maybe Text),
    _hfActionType :: !(Maybe ActionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HandshakeFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hfParentHandshakeId' - Specifies the parent handshake. Only used for handshake types that are a child of another type. If you specify @ParentHandshakeId@ , you cannot also specify @ActionType@ . The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- * 'hfActionType' - Specifies the type of handshake action. If you specify @ActionType@ , you cannot also specify @ParentHandshakeId@ .
handshakeFilter ::
  HandshakeFilter
handshakeFilter =
  HandshakeFilter'
    { _hfParentHandshakeId = Nothing,
      _hfActionType = Nothing
    }

-- | Specifies the parent handshake. Only used for handshake types that are a child of another type. If you specify @ParentHandshakeId@ , you cannot also specify @ActionType@ . The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
hfParentHandshakeId :: Lens' HandshakeFilter (Maybe Text)
hfParentHandshakeId = lens _hfParentHandshakeId (\s a -> s {_hfParentHandshakeId = a})

-- | Specifies the type of handshake action. If you specify @ActionType@ , you cannot also specify @ParentHandshakeId@ .
hfActionType :: Lens' HandshakeFilter (Maybe ActionType)
hfActionType = lens _hfActionType (\s a -> s {_hfActionType = a})

instance Hashable HandshakeFilter

instance NFData HandshakeFilter

instance ToJSON HandshakeFilter where
  toJSON HandshakeFilter' {..} =
    object
      ( catMaybes
          [ ("ParentHandshakeId" .=) <$> _hfParentHandshakeId,
            ("ActionType" .=) <$> _hfActionType
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateLinkAttributes where

import Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Updates a given typed link’s attributes inside a 'BatchRead' operation. Attributes to be updated must not contribute to the typed link’s identity, as defined by its @IdentityAttributeOrder@ . For more information, see 'UpdateLinkAttributes' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchUpdateLinkAttributes' smart constructor.
data BatchUpdateLinkAttributes = BatchUpdateLinkAttributes'
  { _bulaTypedLinkSpecifier ::
      !TypedLinkSpecifier,
    _bulaAttributeUpdates ::
      ![LinkAttributeUpdate]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdateLinkAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bulaTypedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
--
-- * 'bulaAttributeUpdates' - The attributes update structure.
batchUpdateLinkAttributes ::
  -- | 'bulaTypedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchUpdateLinkAttributes
batchUpdateLinkAttributes pTypedLinkSpecifier_ =
  BatchUpdateLinkAttributes'
    { _bulaTypedLinkSpecifier =
        pTypedLinkSpecifier_,
      _bulaAttributeUpdates = mempty
    }

-- | Allows a typed link specifier to be accepted as input.
bulaTypedLinkSpecifier :: Lens' BatchUpdateLinkAttributes TypedLinkSpecifier
bulaTypedLinkSpecifier = lens _bulaTypedLinkSpecifier (\s a -> s {_bulaTypedLinkSpecifier = a})

-- | The attributes update structure.
bulaAttributeUpdates :: Lens' BatchUpdateLinkAttributes [LinkAttributeUpdate]
bulaAttributeUpdates = lens _bulaAttributeUpdates (\s a -> s {_bulaAttributeUpdates = a}) . _Coerce

instance Hashable BatchUpdateLinkAttributes

instance NFData BatchUpdateLinkAttributes

instance ToJSON BatchUpdateLinkAttributes where
  toJSON BatchUpdateLinkAttributes' {..} =
    object
      ( catMaybes
          [ Just ("TypedLinkSpecifier" .= _bulaTypedLinkSpecifier),
            Just ("AttributeUpdates" .= _bulaAttributeUpdates)
          ]
      )

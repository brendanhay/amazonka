{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetLinkAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetLinkAttributes where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Retrieves attributes that are associated with a typed link inside a 'BatchRead' operation. For more information, see 'GetLinkAttributes' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchGetLinkAttributes' smart constructor.
data BatchGetLinkAttributes = BatchGetLinkAttributes'
  { _bglaTypedLinkSpecifier ::
      !TypedLinkSpecifier,
    _bglaAttributeNames :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetLinkAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bglaTypedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
--
-- * 'bglaAttributeNames' - A list of attribute names whose values will be retrieved.
batchGetLinkAttributes ::
  -- | 'bglaTypedLinkSpecifier'
  TypedLinkSpecifier ->
  BatchGetLinkAttributes
batchGetLinkAttributes pTypedLinkSpecifier_ =
  BatchGetLinkAttributes'
    { _bglaTypedLinkSpecifier =
        pTypedLinkSpecifier_,
      _bglaAttributeNames = mempty
    }

-- | Allows a typed link specifier to be accepted as input.
bglaTypedLinkSpecifier :: Lens' BatchGetLinkAttributes TypedLinkSpecifier
bglaTypedLinkSpecifier = lens _bglaTypedLinkSpecifier (\s a -> s {_bglaTypedLinkSpecifier = a})

-- | A list of attribute names whose values will be retrieved.
bglaAttributeNames :: Lens' BatchGetLinkAttributes [Text]
bglaAttributeNames = lens _bglaAttributeNames (\s a -> s {_bglaAttributeNames = a}) . _Coerce

instance Hashable BatchGetLinkAttributes

instance NFData BatchGetLinkAttributes

instance ToJSON BatchGetLinkAttributes where
  toJSON BatchGetLinkAttributes' {..} =
    object
      ( catMaybes
          [ Just ("TypedLinkSpecifier" .= _bglaTypedLinkSpecifier),
            Just ("AttributeNames" .= _bglaAttributeNames)
          ]
      )

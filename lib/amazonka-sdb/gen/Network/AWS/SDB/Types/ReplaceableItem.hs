{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.ReplaceableItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.ReplaceableItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SDB.Types.ReplaceableAttribute

-- |
--
--
--
-- /See:/ 'replaceableItem' smart constructor.
data ReplaceableItem = ReplaceableItem'
  { _riName :: !Text,
    _riAttributes :: ![ReplaceableAttribute]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplaceableItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riName' - The name of the replaceable item.
--
-- * 'riAttributes' - The list of attributes for a replaceable item.
replaceableItem ::
  -- | 'riName'
  Text ->
  ReplaceableItem
replaceableItem pName_ =
  ReplaceableItem' {_riName = pName_, _riAttributes = mempty}

-- | The name of the replaceable item.
riName :: Lens' ReplaceableItem Text
riName = lens _riName (\s a -> s {_riName = a})

-- | The list of attributes for a replaceable item.
riAttributes :: Lens' ReplaceableItem [ReplaceableAttribute]
riAttributes = lens _riAttributes (\s a -> s {_riAttributes = a}) . _Coerce

instance Hashable ReplaceableItem

instance NFData ReplaceableItem

instance ToQuery ReplaceableItem where
  toQuery ReplaceableItem' {..} =
    mconcat
      ["ItemName" =: _riName, toQueryList "Attribute" _riAttributes]

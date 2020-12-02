{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.DeletableItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.DeletableItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SDB.Types.Attribute

-- | /See:/ 'deletableItem' smart constructor.
data DeletableItem = DeletableItem'
  { _diAttributes ::
      !(Maybe [Attribute]),
    _diName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletableItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diAttributes' - Undocumented member.
--
-- * 'diName' - Undocumented member.
deletableItem ::
  -- | 'diName'
  Text ->
  DeletableItem
deletableItem pName_ =
  DeletableItem' {_diAttributes = Nothing, _diName = pName_}

-- | Undocumented member.
diAttributes :: Lens' DeletableItem [Attribute]
diAttributes = lens _diAttributes (\s a -> s {_diAttributes = a}) . _Default . _Coerce

-- | Undocumented member.
diName :: Lens' DeletableItem Text
diName = lens _diName (\s a -> s {_diName = a})

instance Hashable DeletableItem

instance NFData DeletableItem

instance ToQuery DeletableItem where
  toQuery DeletableItem' {..} =
    mconcat
      [ toQuery (toQueryList "Attribute" <$> _diAttributes),
        "ItemName" =: _diName
      ]

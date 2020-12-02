{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.ReplaceableAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.ReplaceableAttribute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'replaceableAttribute' smart constructor.
data ReplaceableAttribute = ReplaceableAttribute'
  { _raReplace ::
      !(Maybe Bool),
    _raName :: !Text,
    _raValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplaceableAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raReplace' - @false@
--
-- * 'raName' - The name of the replaceable attribute.
--
-- * 'raValue' - The value of the replaceable attribute.
replaceableAttribute ::
  -- | 'raName'
  Text ->
  -- | 'raValue'
  Text ->
  ReplaceableAttribute
replaceableAttribute pName_ pValue_ =
  ReplaceableAttribute'
    { _raReplace = Nothing,
      _raName = pName_,
      _raValue = pValue_
    }

-- | @false@
raReplace :: Lens' ReplaceableAttribute (Maybe Bool)
raReplace = lens _raReplace (\s a -> s {_raReplace = a})

-- | The name of the replaceable attribute.
raName :: Lens' ReplaceableAttribute Text
raName = lens _raName (\s a -> s {_raName = a})

-- | The value of the replaceable attribute.
raValue :: Lens' ReplaceableAttribute Text
raValue = lens _raValue (\s a -> s {_raValue = a})

instance Hashable ReplaceableAttribute

instance NFData ReplaceableAttribute

instance ToQuery ReplaceableAttribute where
  toQuery ReplaceableAttribute' {..} =
    mconcat
      ["Replace" =: _raReplace, "Name" =: _raName, "Value" =: _raValue]

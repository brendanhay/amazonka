{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.BuiltinIntentSlot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BuiltinIntentSlot where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a slot used in a built-in intent.
--
--
--
-- /See:/ 'builtinIntentSlot' smart constructor.
newtype BuiltinIntentSlot = BuiltinIntentSlot'
  { _bisName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BuiltinIntentSlot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bisName' - A list of the slots defined for the intent.
builtinIntentSlot ::
  BuiltinIntentSlot
builtinIntentSlot = BuiltinIntentSlot' {_bisName = Nothing}

-- | A list of the slots defined for the intent.
bisName :: Lens' BuiltinIntentSlot (Maybe Text)
bisName = lens _bisName (\s a -> s {_bisName = a})

instance FromJSON BuiltinIntentSlot where
  parseJSON =
    withObject
      "BuiltinIntentSlot"
      (\x -> BuiltinIntentSlot' <$> (x .:? "name"))

instance Hashable BuiltinIntentSlot

instance NFData BuiltinIntentSlot

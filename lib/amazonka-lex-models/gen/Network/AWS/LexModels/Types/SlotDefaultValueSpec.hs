{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotDefaultValueSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotDefaultValueSpec where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.SlotDefaultValue
import Network.AWS.Prelude

-- | Contains the default values for a slot. Default values are used when Amazon Lex hasn't determined a value for a slot.
--
--
--
-- /See:/ 'slotDefaultValueSpec' smart constructor.
newtype SlotDefaultValueSpec = SlotDefaultValueSpec'
  { _sdvsDefaultValueList ::
      [SlotDefaultValue]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SlotDefaultValueSpec' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdvsDefaultValueList' - The default values for a slot. You can specify more than one default. For example, you can specify a default value to use from a matching context variable, a session attribute, or a fixed value. The default value chosen is selected based on the order that you specify them in the list. For example, if you specify a context variable and a fixed value in that order, Amazon Lex uses the context variable if it is available, else it uses the fixed value.
slotDefaultValueSpec ::
  SlotDefaultValueSpec
slotDefaultValueSpec =
  SlotDefaultValueSpec' {_sdvsDefaultValueList = mempty}

-- | The default values for a slot. You can specify more than one default. For example, you can specify a default value to use from a matching context variable, a session attribute, or a fixed value. The default value chosen is selected based on the order that you specify them in the list. For example, if you specify a context variable and a fixed value in that order, Amazon Lex uses the context variable if it is available, else it uses the fixed value.
sdvsDefaultValueList :: Lens' SlotDefaultValueSpec [SlotDefaultValue]
sdvsDefaultValueList = lens _sdvsDefaultValueList (\s a -> s {_sdvsDefaultValueList = a}) . _Coerce

instance FromJSON SlotDefaultValueSpec where
  parseJSON =
    withObject
      "SlotDefaultValueSpec"
      ( \x ->
          SlotDefaultValueSpec' <$> (x .:? "defaultValueList" .!= mempty)
      )

instance Hashable SlotDefaultValueSpec

instance NFData SlotDefaultValueSpec

instance ToJSON SlotDefaultValueSpec where
  toJSON SlotDefaultValueSpec' {..} =
    object
      (catMaybes [Just ("defaultValueList" .= _sdvsDefaultValueList)])

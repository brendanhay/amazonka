{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotDefaultValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotDefaultValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A default value for a slot.
--
--
--
-- /See:/ 'slotDefaultValue' smart constructor.
newtype SlotDefaultValue = SlotDefaultValue'
  { _sdvDefaultValue ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SlotDefaultValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdvDefaultValue' - The default value for the slot. You can specify one of the following:     * @#context-name.slot-name@ - The slot value "slot-name" in the context "context-name."     * @{attribute}@ - The slot value of the session attribute "attribute."     * @'value'@ - The discrete value "value."
slotDefaultValue ::
  -- | 'sdvDefaultValue'
  Text ->
  SlotDefaultValue
slotDefaultValue pDefaultValue_ =
  SlotDefaultValue' {_sdvDefaultValue = pDefaultValue_}

-- | The default value for the slot. You can specify one of the following:     * @#context-name.slot-name@ - The slot value "slot-name" in the context "context-name."     * @{attribute}@ - The slot value of the session attribute "attribute."     * @'value'@ - The discrete value "value."
sdvDefaultValue :: Lens' SlotDefaultValue Text
sdvDefaultValue = lens _sdvDefaultValue (\s a -> s {_sdvDefaultValue = a})

instance FromJSON SlotDefaultValue where
  parseJSON =
    withObject
      "SlotDefaultValue"
      (\x -> SlotDefaultValue' <$> (x .: "defaultValue"))

instance Hashable SlotDefaultValue

instance NFData SlotDefaultValue

instance ToJSON SlotDefaultValue where
  toJSON SlotDefaultValue' {..} =
    object (catMaybes [Just ("defaultValue" .= _sdvDefaultValue)])

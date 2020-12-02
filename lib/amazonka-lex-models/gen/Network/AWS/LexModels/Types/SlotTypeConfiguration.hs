{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.SlotTypeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeConfiguration where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.SlotTypeRegexConfiguration
import Network.AWS.Prelude

-- | Provides configuration information for a slot type.
--
--
--
-- /See:/ 'slotTypeConfiguration' smart constructor.
newtype SlotTypeConfiguration = SlotTypeConfiguration'
  { _stcRegexConfiguration ::
      Maybe SlotTypeRegexConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SlotTypeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stcRegexConfiguration' - A regular expression used to validate the value of a slot.
slotTypeConfiguration ::
  SlotTypeConfiguration
slotTypeConfiguration =
  SlotTypeConfiguration' {_stcRegexConfiguration = Nothing}

-- | A regular expression used to validate the value of a slot.
stcRegexConfiguration :: Lens' SlotTypeConfiguration (Maybe SlotTypeRegexConfiguration)
stcRegexConfiguration = lens _stcRegexConfiguration (\s a -> s {_stcRegexConfiguration = a})

instance FromJSON SlotTypeConfiguration where
  parseJSON =
    withObject
      "SlotTypeConfiguration"
      (\x -> SlotTypeConfiguration' <$> (x .:? "regexConfiguration"))

instance Hashable SlotTypeConfiguration

instance NFData SlotTypeConfiguration

instance ToJSON SlotTypeConfiguration where
  toJSON SlotTypeConfiguration' {..} =
    object
      (catMaybes [("regexConfiguration" .=) <$> _stcRegexConfiguration])

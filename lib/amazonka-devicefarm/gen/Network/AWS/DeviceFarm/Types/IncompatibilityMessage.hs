{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.IncompatibilityMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.IncompatibilityMessage where

import Network.AWS.DeviceFarm.Types.DeviceAttribute
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about incompatibility.
--
--
--
-- /See:/ 'incompatibilityMessage' smart constructor.
data IncompatibilityMessage = IncompatibilityMessage'
  { _imType ::
      !(Maybe DeviceAttribute),
    _imMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IncompatibilityMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imType' - The type of incompatibility. Allowed values include:     * ARN     * FORM_FACTOR (for example, phone or tablet)     * MANUFACTURER     * PLATFORM (for example, Android or iOS)     * REMOTE_ACCESS_ENABLED     * APPIUM_VERSION
--
-- * 'imMessage' - A message about the incompatibility.
incompatibilityMessage ::
  IncompatibilityMessage
incompatibilityMessage =
  IncompatibilityMessage' {_imType = Nothing, _imMessage = Nothing}

-- | The type of incompatibility. Allowed values include:     * ARN     * FORM_FACTOR (for example, phone or tablet)     * MANUFACTURER     * PLATFORM (for example, Android or iOS)     * REMOTE_ACCESS_ENABLED     * APPIUM_VERSION
imType :: Lens' IncompatibilityMessage (Maybe DeviceAttribute)
imType = lens _imType (\s a -> s {_imType = a})

-- | A message about the incompatibility.
imMessage :: Lens' IncompatibilityMessage (Maybe Text)
imMessage = lens _imMessage (\s a -> s {_imMessage = a})

instance FromJSON IncompatibilityMessage where
  parseJSON =
    withObject
      "IncompatibilityMessage"
      ( \x ->
          IncompatibilityMessage' <$> (x .:? "type") <*> (x .:? "message")
      )

instance Hashable IncompatibilityMessage

instance NFData IncompatibilityMessage

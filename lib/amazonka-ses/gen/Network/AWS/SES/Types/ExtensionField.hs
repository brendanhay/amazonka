{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ExtensionField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ExtensionField where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Additional X-headers to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'extensionField' smart constructor.
data ExtensionField = ExtensionField'
  { _efName :: !Text,
    _efValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExtensionField' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efName' - The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- * 'efValue' - The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
extensionField ::
  -- | 'efName'
  Text ->
  -- | 'efValue'
  Text ->
  ExtensionField
extensionField pName_ pValue_ =
  ExtensionField' {_efName = pName_, _efValue = pValue_}

-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
efName :: Lens' ExtensionField Text
efName = lens _efName (\s a -> s {_efName = a})

-- | The value of the header to add. Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
efValue :: Lens' ExtensionField Text
efValue = lens _efValue (\s a -> s {_efValue = a})

instance Hashable ExtensionField

instance NFData ExtensionField

instance ToQuery ExtensionField where
  toQuery ExtensionField' {..} =
    mconcat ["Name" =: _efName, "Value" =: _efValue]

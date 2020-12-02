{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.AddHeaderAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.AddHeaderAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When included in a receipt rule, this action adds a header to the received email.
--
--
-- For information about adding a header using a receipt rule, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-add-header.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'addHeaderAction' smart constructor.
data AddHeaderAction = AddHeaderAction'
  { _ahaHeaderName :: !Text,
    _ahaHeaderValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddHeaderAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahaHeaderName' - The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
--
-- * 'ahaHeaderValue' - Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
addHeaderAction ::
  -- | 'ahaHeaderName'
  Text ->
  -- | 'ahaHeaderValue'
  Text ->
  AddHeaderAction
addHeaderAction pHeaderName_ pHeaderValue_ =
  AddHeaderAction'
    { _ahaHeaderName = pHeaderName_,
      _ahaHeaderValue = pHeaderValue_
    }

-- | The name of the header to add. Must be between 1 and 50 characters, inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and dashes only.
ahaHeaderName :: Lens' AddHeaderAction Text
ahaHeaderName = lens _ahaHeaderName (\s a -> s {_ahaHeaderName = a})

-- | Must be less than 2048 characters, and must not contain newline characters ("\r" or "\n").
ahaHeaderValue :: Lens' AddHeaderAction Text
ahaHeaderValue = lens _ahaHeaderValue (\s a -> s {_ahaHeaderValue = a})

instance FromXML AddHeaderAction where
  parseXML x =
    AddHeaderAction' <$> (x .@ "HeaderName") <*> (x .@ "HeaderValue")

instance Hashable AddHeaderAction

instance NFData AddHeaderAction

instance ToQuery AddHeaderAction where
  toQuery AddHeaderAction' {..} =
    mconcat
      ["HeaderName" =: _ahaHeaderName, "HeaderValue" =: _ahaHeaderValue]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.Button
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.Button where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an option to be shown on the client platform (Facebook, Slack, etc.)
--
--
--
-- /See:/ 'button' smart constructor.
data Button = Button' {_bText :: !Text, _bValue :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Button' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bText' - Text that is visible to the user on the button.
--
-- * 'bValue' - The value sent to Amazon Lex when a user chooses the button. For example, consider button text "NYC." When the user chooses the button, the value sent can be "New York City."
button ::
  -- | 'bText'
  Text ->
  -- | 'bValue'
  Text ->
  Button
button pText_ pValue_ = Button' {_bText = pText_, _bValue = pValue_}

-- | Text that is visible to the user on the button.
bText :: Lens' Button Text
bText = lens _bText (\s a -> s {_bText = a})

-- | The value sent to Amazon Lex when a user chooses the button. For example, consider button text "NYC." When the user chooses the button, the value sent can be "New York City."
bValue :: Lens' Button Text
bValue = lens _bValue (\s a -> s {_bValue = a})

instance FromJSON Button where
  parseJSON =
    withObject
      "Button"
      (\x -> Button' <$> (x .: "text") <*> (x .: "value"))

instance Hashable Button

instance NFData Button

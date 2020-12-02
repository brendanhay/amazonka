{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SimpleEmailPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleEmailPart where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the subject or body of an email message, represented as textual email data and the applicable character set.
--
--
--
-- /See:/ 'simpleEmailPart' smart constructor.
data SimpleEmailPart = SimpleEmailPart'
  { _sepData :: !(Maybe Text),
    _sepCharset :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SimpleEmailPart' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sepData' - The textual data of the message content.
--
-- * 'sepCharset' - The applicable character set for the message content.
simpleEmailPart ::
  SimpleEmailPart
simpleEmailPart =
  SimpleEmailPart' {_sepData = Nothing, _sepCharset = Nothing}

-- | The textual data of the message content.
sepData :: Lens' SimpleEmailPart (Maybe Text)
sepData = lens _sepData (\s a -> s {_sepData = a})

-- | The applicable character set for the message content.
sepCharset :: Lens' SimpleEmailPart (Maybe Text)
sepCharset = lens _sepCharset (\s a -> s {_sepCharset = a})

instance Hashable SimpleEmailPart

instance NFData SimpleEmailPart

instance ToJSON SimpleEmailPart where
  toJSON SimpleEmailPart' {..} =
    object
      ( catMaybes
          [("Data" .=) <$> _sepData, ("Charset" .=) <$> _sepCharset]
      )

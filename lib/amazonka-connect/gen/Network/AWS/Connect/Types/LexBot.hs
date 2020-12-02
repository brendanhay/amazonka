{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.LexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.LexBot where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information of an Amazon Lex bot.
--
--
--
-- /See:/ 'lexBot' smart constructor.
data LexBot = LexBot'
  { _lbLexRegion :: !(Maybe Text),
    _lbName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LexBot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbLexRegion' - The Region the Amazon Lex bot was created in.
--
-- * 'lbName' - The name of the Amazon Lex bot.
lexBot ::
  LexBot
lexBot = LexBot' {_lbLexRegion = Nothing, _lbName = Nothing}

-- | The Region the Amazon Lex bot was created in.
lbLexRegion :: Lens' LexBot (Maybe Text)
lbLexRegion = lens _lbLexRegion (\s a -> s {_lbLexRegion = a})

-- | The name of the Amazon Lex bot.
lbName :: Lens' LexBot (Maybe Text)
lbName = lens _lbName (\s a -> s {_lbName = a})

instance FromJSON LexBot where
  parseJSON =
    withObject
      "LexBot"
      (\x -> LexBot' <$> (x .:? "LexRegion") <*> (x .:? "Name"))

instance Hashable LexBot

instance NFData LexBot

instance ToJSON LexBot where
  toJSON LexBot' {..} =
    object
      ( catMaybes
          [("LexRegion" .=) <$> _lbLexRegion, ("Name" .=) <$> _lbName]
      )

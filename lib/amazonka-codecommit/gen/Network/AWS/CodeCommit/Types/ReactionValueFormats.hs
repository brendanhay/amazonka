{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReactionValueFormats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReactionValueFormats where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the values for reactions to a comment. AWS CodeCommit supports a limited set of reactions.
--
--
--
-- /See:/ 'reactionValueFormats' smart constructor.
data ReactionValueFormats = ReactionValueFormats'
  { _rvfEmoji ::
      !(Maybe Text),
    _rvfShortCode :: !(Maybe Text),
    _rvfUnicode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReactionValueFormats' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rvfEmoji' - The Emoji Version 1.0 graphic of the reaction. These graphics are interpreted slightly differently on different operating systems.
--
-- * 'rvfShortCode' - The emoji short code for the reaction. Short codes are interpreted slightly differently on different operating systems.
--
-- * 'rvfUnicode' - The Unicode codepoint for the reaction.
reactionValueFormats ::
  ReactionValueFormats
reactionValueFormats =
  ReactionValueFormats'
    { _rvfEmoji = Nothing,
      _rvfShortCode = Nothing,
      _rvfUnicode = Nothing
    }

-- | The Emoji Version 1.0 graphic of the reaction. These graphics are interpreted slightly differently on different operating systems.
rvfEmoji :: Lens' ReactionValueFormats (Maybe Text)
rvfEmoji = lens _rvfEmoji (\s a -> s {_rvfEmoji = a})

-- | The emoji short code for the reaction. Short codes are interpreted slightly differently on different operating systems.
rvfShortCode :: Lens' ReactionValueFormats (Maybe Text)
rvfShortCode = lens _rvfShortCode (\s a -> s {_rvfShortCode = a})

-- | The Unicode codepoint for the reaction.
rvfUnicode :: Lens' ReactionValueFormats (Maybe Text)
rvfUnicode = lens _rvfUnicode (\s a -> s {_rvfUnicode = a})

instance FromJSON ReactionValueFormats where
  parseJSON =
    withObject
      "ReactionValueFormats"
      ( \x ->
          ReactionValueFormats'
            <$> (x .:? "emoji") <*> (x .:? "shortCode") <*> (x .:? "unicode")
      )

instance Hashable ReactionValueFormats

instance NFData ReactionValueFormats

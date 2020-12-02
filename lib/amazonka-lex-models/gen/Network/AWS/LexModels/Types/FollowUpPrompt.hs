{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.FollowUpPrompt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.FollowUpPrompt where

import Network.AWS.Lens
import Network.AWS.LexModels.Types.Prompt
import Network.AWS.LexModels.Types.Statement
import Network.AWS.Prelude

-- | A prompt for additional activity after an intent is fulfilled. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to find out whether the user wants to order drinks.
--
--
--
-- /See:/ 'followUpPrompt' smart constructor.
data FollowUpPrompt = FollowUpPrompt'
  { _fupPrompt :: !Prompt,
    _fupRejectionStatement :: !Statement
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FollowUpPrompt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fupPrompt' - Prompts for information from the user.
--
-- * 'fupRejectionStatement' - If the user answers "no" to the question defined in the @prompt@ field, Amazon Lex responds with this statement to acknowledge that the intent was canceled.
followUpPrompt ::
  -- | 'fupPrompt'
  Prompt ->
  -- | 'fupRejectionStatement'
  Statement ->
  FollowUpPrompt
followUpPrompt pPrompt_ pRejectionStatement_ =
  FollowUpPrompt'
    { _fupPrompt = pPrompt_,
      _fupRejectionStatement = pRejectionStatement_
    }

-- | Prompts for information from the user.
fupPrompt :: Lens' FollowUpPrompt Prompt
fupPrompt = lens _fupPrompt (\s a -> s {_fupPrompt = a})

-- | If the user answers "no" to the question defined in the @prompt@ field, Amazon Lex responds with this statement to acknowledge that the intent was canceled.
fupRejectionStatement :: Lens' FollowUpPrompt Statement
fupRejectionStatement = lens _fupRejectionStatement (\s a -> s {_fupRejectionStatement = a})

instance FromJSON FollowUpPrompt where
  parseJSON =
    withObject
      "FollowUpPrompt"
      ( \x ->
          FollowUpPrompt'
            <$> (x .: "prompt") <*> (x .: "rejectionStatement")
      )

instance Hashable FollowUpPrompt

instance NFData FollowUpPrompt

instance ToJSON FollowUpPrompt where
  toJSON FollowUpPrompt' {..} =
    object
      ( catMaybes
          [ Just ("prompt" .= _fupPrompt),
            Just ("rejectionStatement" .= _fupRejectionStatement)
          ]
      )

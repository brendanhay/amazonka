{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexPatternSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexPatternSetUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.ChangeAction

-- | In an 'UpdateRegexPatternSet' request, @RegexPatternSetUpdate@ specifies whether to insert or delete a @RegexPatternString@ and includes the settings for the @RegexPatternString@ .
--
--
--
-- /See:/ 'regexPatternSetUpdate' smart constructor.
data RegexPatternSetUpdate = RegexPatternSetUpdate'
  { _rpsuAction ::
      !ChangeAction,
    _rpsuRegexPatternString :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegexPatternSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsuAction' - Specifies whether to insert or delete a @RegexPatternString@ .
--
-- * 'rpsuRegexPatternString' - Specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
regexPatternSetUpdate ::
  -- | 'rpsuAction'
  ChangeAction ->
  -- | 'rpsuRegexPatternString'
  Text ->
  RegexPatternSetUpdate
regexPatternSetUpdate pAction_ pRegexPatternString_ =
  RegexPatternSetUpdate'
    { _rpsuAction = pAction_,
      _rpsuRegexPatternString = pRegexPatternString_
    }

-- | Specifies whether to insert or delete a @RegexPatternString@ .
rpsuAction :: Lens' RegexPatternSetUpdate ChangeAction
rpsuAction = lens _rpsuAction (\s a -> s {_rpsuAction = a})

-- | Specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
rpsuRegexPatternString :: Lens' RegexPatternSetUpdate Text
rpsuRegexPatternString = lens _rpsuRegexPatternString (\s a -> s {_rpsuRegexPatternString = a})

instance Hashable RegexPatternSetUpdate

instance NFData RegexPatternSetUpdate

instance ToJSON RegexPatternSetUpdate where
  toJSON RegexPatternSetUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _rpsuAction),
            Just ("RegexPatternString" .= _rpsuRegexPatternString)
          ]
      )

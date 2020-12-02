{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexMatchSetUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.ChangeAction
import Network.AWS.WAFRegional.Types.RegexMatchTuple

-- | In an 'UpdateRegexMatchSet' request, @RegexMatchSetUpdate@ specifies whether to insert or delete a 'RegexMatchTuple' and includes the settings for the @RegexMatchTuple@ .
--
--
--
-- /See:/ 'regexMatchSetUpdate' smart constructor.
data RegexMatchSetUpdate = RegexMatchSetUpdate'
  { _rmsuAction ::
      !ChangeAction,
    _rmsuRegexMatchTuple :: !RegexMatchTuple
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegexMatchSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmsuAction' - Specifies whether to insert or delete a 'RegexMatchTuple' .
--
-- * 'rmsuRegexMatchTuple' - Information about the part of a web request that you want AWS WAF to inspect and the identifier of the regular expression (regex) pattern that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @RegexMatchTuple@ values must exactly match the values in the @RegexMatchTuple@ that you want to delete from the @RegexMatchSet@ .
regexMatchSetUpdate ::
  -- | 'rmsuAction'
  ChangeAction ->
  -- | 'rmsuRegexMatchTuple'
  RegexMatchTuple ->
  RegexMatchSetUpdate
regexMatchSetUpdate pAction_ pRegexMatchTuple_ =
  RegexMatchSetUpdate'
    { _rmsuAction = pAction_,
      _rmsuRegexMatchTuple = pRegexMatchTuple_
    }

-- | Specifies whether to insert or delete a 'RegexMatchTuple' .
rmsuAction :: Lens' RegexMatchSetUpdate ChangeAction
rmsuAction = lens _rmsuAction (\s a -> s {_rmsuAction = a})

-- | Information about the part of a web request that you want AWS WAF to inspect and the identifier of the regular expression (regex) pattern that you want AWS WAF to search for. If you specify @DELETE@ for the value of @Action@ , the @RegexMatchTuple@ values must exactly match the values in the @RegexMatchTuple@ that you want to delete from the @RegexMatchSet@ .
rmsuRegexMatchTuple :: Lens' RegexMatchSetUpdate RegexMatchTuple
rmsuRegexMatchTuple = lens _rmsuRegexMatchTuple (\s a -> s {_rmsuRegexMatchTuple = a})

instance Hashable RegexMatchSetUpdate

instance NFData RegexMatchSetUpdate

instance ToJSON RegexMatchSetUpdate where
  toJSON RegexMatchSetUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _rmsuAction),
            Just ("RegexMatchTuple" .= _rmsuRegexMatchTuple)
          ]
      )

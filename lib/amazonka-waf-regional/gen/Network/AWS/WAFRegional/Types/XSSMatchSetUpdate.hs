{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.XSSMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.XSSMatchSetUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.ChangeAction
import Network.AWS.WAFRegional.Types.XSSMatchTuple

-- | Specifies the part of a web request that you want to inspect for cross-site scripting attacks and indicates whether you want to add the specification to an 'XssMatchSet' or delete it from an @XssMatchSet@ .
--
--
--
-- /See:/ 'xssMatchSetUpdate' smart constructor.
data XSSMatchSetUpdate = XSSMatchSetUpdate'
  { _xmsuAction ::
      !ChangeAction,
    _xmsuXSSMatchTuple :: !XSSMatchTuple
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'XSSMatchSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xmsuAction' - Specify @INSERT@ to add an 'XssMatchSetUpdate' to an 'XssMatchSet' . Use @DELETE@ to remove an @XssMatchSetUpdate@ from an @XssMatchSet@ .
--
-- * 'xmsuXSSMatchTuple' - Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
xssMatchSetUpdate ::
  -- | 'xmsuAction'
  ChangeAction ->
  -- | 'xmsuXSSMatchTuple'
  XSSMatchTuple ->
  XSSMatchSetUpdate
xssMatchSetUpdate pAction_ pXSSMatchTuple_ =
  XSSMatchSetUpdate'
    { _xmsuAction = pAction_,
      _xmsuXSSMatchTuple = pXSSMatchTuple_
    }

-- | Specify @INSERT@ to add an 'XssMatchSetUpdate' to an 'XssMatchSet' . Use @DELETE@ to remove an @XssMatchSetUpdate@ from an @XssMatchSet@ .
xmsuAction :: Lens' XSSMatchSetUpdate ChangeAction
xmsuAction = lens _xmsuAction (\s a -> s {_xmsuAction = a})

-- | Specifies the part of a web request that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header.
xmsuXSSMatchTuple :: Lens' XSSMatchSetUpdate XSSMatchTuple
xmsuXSSMatchTuple = lens _xmsuXSSMatchTuple (\s a -> s {_xmsuXSSMatchTuple = a})

instance Hashable XSSMatchSetUpdate

instance NFData XSSMatchSetUpdate

instance ToJSON XSSMatchSetUpdate where
  toJSON XSSMatchSetUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _xmsuAction),
            Just ("XssMatchTuple" .= _xmsuXSSMatchTuple)
          ]
      )

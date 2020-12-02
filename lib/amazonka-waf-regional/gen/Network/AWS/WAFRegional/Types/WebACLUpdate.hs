{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.WebACLUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WebACLUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.ActivatedRule
import Network.AWS.WAFRegional.Types.ChangeAction

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
--
--
--
-- /See:/ 'webACLUpdate' smart constructor.
data WebACLUpdate = WebACLUpdate'
  { _wauAction :: !ChangeAction,
    _wauActivatedRule :: !ActivatedRule
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebACLUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wauAction' - Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
--
-- * 'wauActivatedRule' - The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
webACLUpdate ::
  -- | 'wauAction'
  ChangeAction ->
  -- | 'wauActivatedRule'
  ActivatedRule ->
  WebACLUpdate
webACLUpdate pAction_ pActivatedRule_ =
  WebACLUpdate'
    { _wauAction = pAction_,
      _wauActivatedRule = pActivatedRule_
    }

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a @WebACL@ .
wauAction :: Lens' WebACLUpdate ChangeAction
wauAction = lens _wauAction (\s a -> s {_wauAction = a})

-- | The @ActivatedRule@ object in an 'UpdateWebACL' request specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
wauActivatedRule :: Lens' WebACLUpdate ActivatedRule
wauActivatedRule = lens _wauActivatedRule (\s a -> s {_wauActivatedRule = a})

instance Hashable WebACLUpdate

instance NFData WebACLUpdate

instance ToJSON WebACLUpdate where
  toJSON WebACLUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _wauAction),
            Just ("ActivatedRule" .= _wauActivatedRule)
          ]
      )

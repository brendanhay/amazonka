{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RuleUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.ChangeAction
import Network.AWS.WAFRegional.Types.Predicate

-- | Specifies a @Predicate@ (such as an @IPSet@ ) and indicates whether you want to add it to a @Rule@ or delete it from a @Rule@ .
--
--
--
-- /See:/ 'ruleUpdate' smart constructor.
data RuleUpdate = RuleUpdate'
  { _ruAction :: !ChangeAction,
    _ruPredicate :: !Predicate
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RuleUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ruAction' - Specify @INSERT@ to add a @Predicate@ to a @Rule@ . Use @DELETE@ to remove a @Predicate@ from a @Rule@ .
--
-- * 'ruPredicate' - The ID of the @Predicate@ (such as an @IPSet@ ) that you want to add to a @Rule@ .
ruleUpdate ::
  -- | 'ruAction'
  ChangeAction ->
  -- | 'ruPredicate'
  Predicate ->
  RuleUpdate
ruleUpdate pAction_ pPredicate_ =
  RuleUpdate' {_ruAction = pAction_, _ruPredicate = pPredicate_}

-- | Specify @INSERT@ to add a @Predicate@ to a @Rule@ . Use @DELETE@ to remove a @Predicate@ from a @Rule@ .
ruAction :: Lens' RuleUpdate ChangeAction
ruAction = lens _ruAction (\s a -> s {_ruAction = a})

-- | The ID of the @Predicate@ (such as an @IPSet@ ) that you want to add to a @Rule@ .
ruPredicate :: Lens' RuleUpdate Predicate
ruPredicate = lens _ruPredicate (\s a -> s {_ruPredicate = a})

instance Hashable RuleUpdate

instance NFData RuleUpdate

instance ToJSON RuleUpdate where
  toJSON RuleUpdate' {..} =
    object
      ( catMaybes
          [Just ("Action" .= _ruAction), Just ("Predicate" .= _ruPredicate)]
      )

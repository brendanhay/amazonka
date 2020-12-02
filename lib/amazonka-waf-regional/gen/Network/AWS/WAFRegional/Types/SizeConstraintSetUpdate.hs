{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SizeConstraintSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SizeConstraintSetUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAFRegional.Types.ChangeAction
import Network.AWS.WAFRegional.Types.SizeConstraint

-- | Specifies the part of a web request that you want to inspect the size of and indicates whether you want to add the specification to a 'SizeConstraintSet' or delete it from a @SizeConstraintSet@ .
--
--
--
-- /See:/ 'sizeConstraintSetUpdate' smart constructor.
data SizeConstraintSetUpdate = SizeConstraintSetUpdate'
  { _scsuAction ::
      !ChangeAction,
    _scsuSizeConstraint :: !SizeConstraint
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SizeConstraintSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scsuAction' - Specify @INSERT@ to add a 'SizeConstraintSetUpdate' to a 'SizeConstraintSet' . Use @DELETE@ to remove a @SizeConstraintSetUpdate@ from a @SizeConstraintSet@ .
--
-- * 'scsuSizeConstraint' - Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
sizeConstraintSetUpdate ::
  -- | 'scsuAction'
  ChangeAction ->
  -- | 'scsuSizeConstraint'
  SizeConstraint ->
  SizeConstraintSetUpdate
sizeConstraintSetUpdate pAction_ pSizeConstraint_ =
  SizeConstraintSetUpdate'
    { _scsuAction = pAction_,
      _scsuSizeConstraint = pSizeConstraint_
    }

-- | Specify @INSERT@ to add a 'SizeConstraintSetUpdate' to a 'SizeConstraintSet' . Use @DELETE@ to remove a @SizeConstraintSetUpdate@ from a @SizeConstraintSet@ .
scsuAction :: Lens' SizeConstraintSetUpdate ChangeAction
scsuAction = lens _scsuAction (\s a -> s {_scsuAction = a})

-- | Specifies a constraint on the size of a part of the web request. AWS WAF uses the @Size@ , @ComparisonOperator@ , and @FieldToMatch@ to build an expression in the form of "@Size@ @ComparisonOperator@ size in bytes of @FieldToMatch@ ". If that expression is true, the @SizeConstraint@ is considered to match.
scsuSizeConstraint :: Lens' SizeConstraintSetUpdate SizeConstraint
scsuSizeConstraint = lens _scsuSizeConstraint (\s a -> s {_scsuSizeConstraint = a})

instance Hashable SizeConstraintSetUpdate

instance NFData SizeConstraintSetUpdate

instance ToJSON SizeConstraintSetUpdate where
  toJSON SizeConstraintSetUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _scsuAction),
            Just ("SizeConstraint" .= _scsuSizeConstraint)
          ]
      )

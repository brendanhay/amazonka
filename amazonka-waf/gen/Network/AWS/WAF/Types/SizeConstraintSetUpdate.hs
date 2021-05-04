{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SizeConstraintSetUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraintSetUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.SizeConstraint

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies the part of a web request that you want to inspect the size of
-- and indicates whether you want to add the specification to a
-- SizeConstraintSet or delete it from a @SizeConstraintSet@.
--
-- /See:/ 'newSizeConstraintSetUpdate' smart constructor.
data SizeConstraintSetUpdate = SizeConstraintSetUpdate'
  { -- | Specify @INSERT@ to add a SizeConstraintSetUpdate to a
    -- SizeConstraintSet. Use @DELETE@ to remove a @SizeConstraintSetUpdate@
    -- from a @SizeConstraintSet@.
    action :: ChangeAction,
    -- | Specifies a constraint on the size of a part of the web request. AWS WAF
    -- uses the @Size@, @ComparisonOperator@, and @FieldToMatch@ to build an
    -- expression in the form of \"@Size@ @ComparisonOperator@ size in bytes of
    -- @FieldToMatch@\". If that expression is true, the @SizeConstraint@ is
    -- considered to match.
    sizeConstraint :: SizeConstraint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SizeConstraintSetUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'sizeConstraintSetUpdate_action' - Specify @INSERT@ to add a SizeConstraintSetUpdate to a
-- SizeConstraintSet. Use @DELETE@ to remove a @SizeConstraintSetUpdate@
-- from a @SizeConstraintSet@.
--
-- 'sizeConstraint', 'sizeConstraintSetUpdate_sizeConstraint' - Specifies a constraint on the size of a part of the web request. AWS WAF
-- uses the @Size@, @ComparisonOperator@, and @FieldToMatch@ to build an
-- expression in the form of \"@Size@ @ComparisonOperator@ size in bytes of
-- @FieldToMatch@\". If that expression is true, the @SizeConstraint@ is
-- considered to match.
newSizeConstraintSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'sizeConstraint'
  SizeConstraint ->
  SizeConstraintSetUpdate
newSizeConstraintSetUpdate pAction_ pSizeConstraint_ =
  SizeConstraintSetUpdate'
    { action = pAction_,
      sizeConstraint = pSizeConstraint_
    }

-- | Specify @INSERT@ to add a SizeConstraintSetUpdate to a
-- SizeConstraintSet. Use @DELETE@ to remove a @SizeConstraintSetUpdate@
-- from a @SizeConstraintSet@.
sizeConstraintSetUpdate_action :: Lens.Lens' SizeConstraintSetUpdate ChangeAction
sizeConstraintSetUpdate_action = Lens.lens (\SizeConstraintSetUpdate' {action} -> action) (\s@SizeConstraintSetUpdate' {} a -> s {action = a} :: SizeConstraintSetUpdate)

-- | Specifies a constraint on the size of a part of the web request. AWS WAF
-- uses the @Size@, @ComparisonOperator@, and @FieldToMatch@ to build an
-- expression in the form of \"@Size@ @ComparisonOperator@ size in bytes of
-- @FieldToMatch@\". If that expression is true, the @SizeConstraint@ is
-- considered to match.
sizeConstraintSetUpdate_sizeConstraint :: Lens.Lens' SizeConstraintSetUpdate SizeConstraint
sizeConstraintSetUpdate_sizeConstraint = Lens.lens (\SizeConstraintSetUpdate' {sizeConstraint} -> sizeConstraint) (\s@SizeConstraintSetUpdate' {} a -> s {sizeConstraint = a} :: SizeConstraintSetUpdate)

instance Prelude.Hashable SizeConstraintSetUpdate

instance Prelude.NFData SizeConstraintSetUpdate

instance Prelude.ToJSON SizeConstraintSetUpdate where
  toJSON SizeConstraintSetUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ("SizeConstraint" Prelude..= sizeConstraint)
          ]
      )

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
-- Module      : Network.AWS.WAF.Types.RegexMatchSetUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexMatchSetUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.RegexMatchTuple

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- In an UpdateRegexMatchSet request, @RegexMatchSetUpdate@ specifies
-- whether to insert or delete a RegexMatchTuple and includes the settings
-- for the @RegexMatchTuple@.
--
-- /See:/ 'newRegexMatchSetUpdate' smart constructor.
data RegexMatchSetUpdate = RegexMatchSetUpdate'
  { -- | Specifies whether to insert or delete a RegexMatchTuple.
    action :: ChangeAction,
    -- | Information about the part of a web request that you want AWS WAF to
    -- inspect and the identifier of the regular expression (regex) pattern
    -- that you want AWS WAF to search for. If you specify @DELETE@ for the
    -- value of @Action@, the @RegexMatchTuple@ values must exactly match the
    -- values in the @RegexMatchTuple@ that you want to delete from the
    -- @RegexMatchSet@.
    regexMatchTuple :: RegexMatchTuple
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegexMatchSetUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'regexMatchSetUpdate_action' - Specifies whether to insert or delete a RegexMatchTuple.
--
-- 'regexMatchTuple', 'regexMatchSetUpdate_regexMatchTuple' - Information about the part of a web request that you want AWS WAF to
-- inspect and the identifier of the regular expression (regex) pattern
-- that you want AWS WAF to search for. If you specify @DELETE@ for the
-- value of @Action@, the @RegexMatchTuple@ values must exactly match the
-- values in the @RegexMatchTuple@ that you want to delete from the
-- @RegexMatchSet@.
newRegexMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'regexMatchTuple'
  RegexMatchTuple ->
  RegexMatchSetUpdate
newRegexMatchSetUpdate pAction_ pRegexMatchTuple_ =
  RegexMatchSetUpdate'
    { action = pAction_,
      regexMatchTuple = pRegexMatchTuple_
    }

-- | Specifies whether to insert or delete a RegexMatchTuple.
regexMatchSetUpdate_action :: Lens.Lens' RegexMatchSetUpdate ChangeAction
regexMatchSetUpdate_action = Lens.lens (\RegexMatchSetUpdate' {action} -> action) (\s@RegexMatchSetUpdate' {} a -> s {action = a} :: RegexMatchSetUpdate)

-- | Information about the part of a web request that you want AWS WAF to
-- inspect and the identifier of the regular expression (regex) pattern
-- that you want AWS WAF to search for. If you specify @DELETE@ for the
-- value of @Action@, the @RegexMatchTuple@ values must exactly match the
-- values in the @RegexMatchTuple@ that you want to delete from the
-- @RegexMatchSet@.
regexMatchSetUpdate_regexMatchTuple :: Lens.Lens' RegexMatchSetUpdate RegexMatchTuple
regexMatchSetUpdate_regexMatchTuple = Lens.lens (\RegexMatchSetUpdate' {regexMatchTuple} -> regexMatchTuple) (\s@RegexMatchSetUpdate' {} a -> s {regexMatchTuple = a} :: RegexMatchSetUpdate)

instance Prelude.Hashable RegexMatchSetUpdate

instance Prelude.NFData RegexMatchSetUpdate

instance Prelude.ToJSON RegexMatchSetUpdate where
  toJSON RegexMatchSetUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ("RegexMatchTuple" Prelude..= regexMatchTuple)
          ]
      )

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
-- Module      : Network.AWS.WAFRegional.Types.RegexPatternSetUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexPatternSetUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAFRegional.Types.ChangeAction

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- In an UpdateRegexPatternSet request, @RegexPatternSetUpdate@ specifies
-- whether to insert or delete a @RegexPatternString@ and includes the
-- settings for the @RegexPatternString@.
--
-- /See:/ 'newRegexPatternSetUpdate' smart constructor.
data RegexPatternSetUpdate = RegexPatternSetUpdate'
  { -- | Specifies whether to insert or delete a @RegexPatternString@.
    action :: ChangeAction,
    -- | Specifies the regular expression (regex) pattern that you want AWS WAF
    -- to search for, such as @B[a\@]dB[o0]t@.
    regexPatternString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegexPatternSetUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'regexPatternSetUpdate_action' - Specifies whether to insert or delete a @RegexPatternString@.
--
-- 'regexPatternString', 'regexPatternSetUpdate_regexPatternString' - Specifies the regular expression (regex) pattern that you want AWS WAF
-- to search for, such as @B[a\@]dB[o0]t@.
newRegexPatternSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'regexPatternString'
  Prelude.Text ->
  RegexPatternSetUpdate
newRegexPatternSetUpdate
  pAction_
  pRegexPatternString_ =
    RegexPatternSetUpdate'
      { action = pAction_,
        regexPatternString = pRegexPatternString_
      }

-- | Specifies whether to insert or delete a @RegexPatternString@.
regexPatternSetUpdate_action :: Lens.Lens' RegexPatternSetUpdate ChangeAction
regexPatternSetUpdate_action = Lens.lens (\RegexPatternSetUpdate' {action} -> action) (\s@RegexPatternSetUpdate' {} a -> s {action = a} :: RegexPatternSetUpdate)

-- | Specifies the regular expression (regex) pattern that you want AWS WAF
-- to search for, such as @B[a\@]dB[o0]t@.
regexPatternSetUpdate_regexPatternString :: Lens.Lens' RegexPatternSetUpdate Prelude.Text
regexPatternSetUpdate_regexPatternString = Lens.lens (\RegexPatternSetUpdate' {regexPatternString} -> regexPatternString) (\s@RegexPatternSetUpdate' {} a -> s {regexPatternString = a} :: RegexPatternSetUpdate)

instance Prelude.Hashable RegexPatternSetUpdate

instance Prelude.NFData RegexPatternSetUpdate

instance Prelude.ToJSON RegexPatternSetUpdate where
  toJSON RegexPatternSetUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ( "RegexPatternString"
                  Prelude..= regexPatternString
              )
          ]
      )

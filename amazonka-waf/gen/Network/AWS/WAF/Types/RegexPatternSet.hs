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
-- Module      : Network.AWS.WAF.Types.RegexPatternSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexPatternSet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The @RegexPatternSet@ specifies the regular expression (regex) pattern
-- that you want AWS WAF to search for, such as @B[a\@]dB[o0]t@. You can
-- then configure AWS WAF to reject those requests.
--
-- /See:/ 'newRegexPatternSet' smart constructor.
data RegexPatternSet = RegexPatternSet'
  { -- | A friendly name or description of the RegexPatternSet. You can\'t change
    -- @Name@ after you create a @RegexPatternSet@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the @RegexPatternSet@. You use @RegexPatternSetId@ to
    -- get information about a @RegexPatternSet@, update a @RegexPatternSet@,
    -- remove a @RegexPatternSet@ from a @RegexMatchSet@, and delete a
    -- @RegexPatternSet@ from AWS WAF.
    --
    -- @RegexMatchSetId@ is returned by CreateRegexPatternSet and by
    -- ListRegexPatternSets.
    regexPatternSetId :: Prelude.Text,
    -- | Specifies the regular expression (regex) patterns that you want AWS WAF
    -- to search for, such as @B[a\@]dB[o0]t@.
    regexPatternStrings :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegexPatternSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'regexPatternSet_name' - A friendly name or description of the RegexPatternSet. You can\'t change
-- @Name@ after you create a @RegexPatternSet@.
--
-- 'regexPatternSetId', 'regexPatternSet_regexPatternSetId' - The identifier for the @RegexPatternSet@. You use @RegexPatternSetId@ to
-- get information about a @RegexPatternSet@, update a @RegexPatternSet@,
-- remove a @RegexPatternSet@ from a @RegexMatchSet@, and delete a
-- @RegexPatternSet@ from AWS WAF.
--
-- @RegexMatchSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
--
-- 'regexPatternStrings', 'regexPatternSet_regexPatternStrings' - Specifies the regular expression (regex) patterns that you want AWS WAF
-- to search for, such as @B[a\@]dB[o0]t@.
newRegexPatternSet ::
  -- | 'regexPatternSetId'
  Prelude.Text ->
  RegexPatternSet
newRegexPatternSet pRegexPatternSetId_ =
  RegexPatternSet'
    { name = Prelude.Nothing,
      regexPatternSetId = pRegexPatternSetId_,
      regexPatternStrings = Prelude.mempty
    }

-- | A friendly name or description of the RegexPatternSet. You can\'t change
-- @Name@ after you create a @RegexPatternSet@.
regexPatternSet_name :: Lens.Lens' RegexPatternSet (Prelude.Maybe Prelude.Text)
regexPatternSet_name = Lens.lens (\RegexPatternSet' {name} -> name) (\s@RegexPatternSet' {} a -> s {name = a} :: RegexPatternSet)

-- | The identifier for the @RegexPatternSet@. You use @RegexPatternSetId@ to
-- get information about a @RegexPatternSet@, update a @RegexPatternSet@,
-- remove a @RegexPatternSet@ from a @RegexMatchSet@, and delete a
-- @RegexPatternSet@ from AWS WAF.
--
-- @RegexMatchSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
regexPatternSet_regexPatternSetId :: Lens.Lens' RegexPatternSet Prelude.Text
regexPatternSet_regexPatternSetId = Lens.lens (\RegexPatternSet' {regexPatternSetId} -> regexPatternSetId) (\s@RegexPatternSet' {} a -> s {regexPatternSetId = a} :: RegexPatternSet)

-- | Specifies the regular expression (regex) patterns that you want AWS WAF
-- to search for, such as @B[a\@]dB[o0]t@.
regexPatternSet_regexPatternStrings :: Lens.Lens' RegexPatternSet [Prelude.Text]
regexPatternSet_regexPatternStrings = Lens.lens (\RegexPatternSet' {regexPatternStrings} -> regexPatternStrings) (\s@RegexPatternSet' {} a -> s {regexPatternStrings = a} :: RegexPatternSet) Prelude.. Prelude._Coerce

instance Prelude.FromJSON RegexPatternSet where
  parseJSON =
    Prelude.withObject
      "RegexPatternSet"
      ( \x ->
          RegexPatternSet'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "RegexPatternSetId")
            Prelude.<*> ( x Prelude..:? "RegexPatternStrings"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RegexPatternSet

instance Prelude.NFData RegexPatternSet

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
-- Module      : Amazonka.WAF.Types.RegexMatchSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.RegexMatchSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAF.Types.RegexMatchTuple

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- In a GetRegexMatchSet request, @RegexMatchSet@ is a complex type that
-- contains the @RegexMatchSetId@ and @Name@ of a @RegexMatchSet@, and the
-- values that you specified when you updated the @RegexMatchSet@.
--
-- The values are contained in a @RegexMatchTuple@ object, which specify
-- the parts of web requests that you want AWS WAF to inspect and the
-- values that you want AWS WAF to search for. If a @RegexMatchSet@
-- contains more than one @RegexMatchTuple@ object, a request needs to
-- match the settings in only one @ByteMatchTuple@ to be considered a
-- match.
--
-- /See:/ 'newRegexMatchSet' smart constructor.
data RegexMatchSet = RegexMatchSet'
  { -- | A friendly name or description of the RegexMatchSet. You can\'t change
    -- @Name@ after you create a @RegexMatchSet@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The @RegexMatchSetId@ for a @RegexMatchSet@. You use @RegexMatchSetId@
    -- to get information about a @RegexMatchSet@ (see GetRegexMatchSet),
    -- update a @RegexMatchSet@ (see UpdateRegexMatchSet), insert a
    -- @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see
    -- UpdateRule), and delete a @RegexMatchSet@ from AWS WAF (see
    -- DeleteRegexMatchSet).
    --
    -- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
    -- ListRegexMatchSets.
    regexMatchSetId :: Prelude.Maybe Prelude.Text,
    -- | Contains an array of RegexMatchTuple objects. Each @RegexMatchTuple@
    -- object contains:
    --
    -- -   The part of a web request that you want AWS WAF to inspect, such as
    --     a query string or the value of the @User-Agent@ header.
    --
    -- -   The identifier of the pattern (a regular expression) that you want
    --     AWS WAF to look for. For more information, see RegexPatternSet.
    --
    -- -   Whether to perform any conversions on the request, such as
    --     converting it to lowercase, before inspecting it for the specified
    --     string.
    regexMatchTuples :: Prelude.Maybe [RegexMatchTuple]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegexMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'regexMatchSet_name' - A friendly name or description of the RegexMatchSet. You can\'t change
-- @Name@ after you create a @RegexMatchSet@.
--
-- 'regexMatchSetId', 'regexMatchSet_regexMatchSetId' - The @RegexMatchSetId@ for a @RegexMatchSet@. You use @RegexMatchSetId@
-- to get information about a @RegexMatchSet@ (see GetRegexMatchSet),
-- update a @RegexMatchSet@ (see UpdateRegexMatchSet), insert a
-- @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see
-- UpdateRule), and delete a @RegexMatchSet@ from AWS WAF (see
-- DeleteRegexMatchSet).
--
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
--
-- 'regexMatchTuples', 'regexMatchSet_regexMatchTuples' - Contains an array of RegexMatchTuple objects. Each @RegexMatchTuple@
-- object contains:
--
-- -   The part of a web request that you want AWS WAF to inspect, such as
--     a query string or the value of the @User-Agent@ header.
--
-- -   The identifier of the pattern (a regular expression) that you want
--     AWS WAF to look for. For more information, see RegexPatternSet.
--
-- -   Whether to perform any conversions on the request, such as
--     converting it to lowercase, before inspecting it for the specified
--     string.
newRegexMatchSet ::
  RegexMatchSet
newRegexMatchSet =
  RegexMatchSet'
    { name = Prelude.Nothing,
      regexMatchSetId = Prelude.Nothing,
      regexMatchTuples = Prelude.Nothing
    }

-- | A friendly name or description of the RegexMatchSet. You can\'t change
-- @Name@ after you create a @RegexMatchSet@.
regexMatchSet_name :: Lens.Lens' RegexMatchSet (Prelude.Maybe Prelude.Text)
regexMatchSet_name = Lens.lens (\RegexMatchSet' {name} -> name) (\s@RegexMatchSet' {} a -> s {name = a} :: RegexMatchSet)

-- | The @RegexMatchSetId@ for a @RegexMatchSet@. You use @RegexMatchSetId@
-- to get information about a @RegexMatchSet@ (see GetRegexMatchSet),
-- update a @RegexMatchSet@ (see UpdateRegexMatchSet), insert a
-- @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see
-- UpdateRule), and delete a @RegexMatchSet@ from AWS WAF (see
-- DeleteRegexMatchSet).
--
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
regexMatchSet_regexMatchSetId :: Lens.Lens' RegexMatchSet (Prelude.Maybe Prelude.Text)
regexMatchSet_regexMatchSetId = Lens.lens (\RegexMatchSet' {regexMatchSetId} -> regexMatchSetId) (\s@RegexMatchSet' {} a -> s {regexMatchSetId = a} :: RegexMatchSet)

-- | Contains an array of RegexMatchTuple objects. Each @RegexMatchTuple@
-- object contains:
--
-- -   The part of a web request that you want AWS WAF to inspect, such as
--     a query string or the value of the @User-Agent@ header.
--
-- -   The identifier of the pattern (a regular expression) that you want
--     AWS WAF to look for. For more information, see RegexPatternSet.
--
-- -   Whether to perform any conversions on the request, such as
--     converting it to lowercase, before inspecting it for the specified
--     string.
regexMatchSet_regexMatchTuples :: Lens.Lens' RegexMatchSet (Prelude.Maybe [RegexMatchTuple])
regexMatchSet_regexMatchTuples = Lens.lens (\RegexMatchSet' {regexMatchTuples} -> regexMatchTuples) (\s@RegexMatchSet' {} a -> s {regexMatchTuples = a} :: RegexMatchSet) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RegexMatchSet where
  parseJSON =
    Data.withObject
      "RegexMatchSet"
      ( \x ->
          RegexMatchSet'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RegexMatchSetId")
            Prelude.<*> ( x
                            Data..:? "RegexMatchTuples"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RegexMatchSet where
  hashWithSalt _salt RegexMatchSet' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regexMatchSetId
      `Prelude.hashWithSalt` regexMatchTuples

instance Prelude.NFData RegexMatchSet where
  rnf RegexMatchSet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf regexMatchSetId
      `Prelude.seq` Prelude.rnf regexMatchTuples

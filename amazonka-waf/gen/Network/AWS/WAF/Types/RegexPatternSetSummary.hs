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
-- Module      : Network.AWS.WAF.Types.RegexPatternSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexPatternSetSummary where

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
-- Returned by ListRegexPatternSets. Each @RegexPatternSetSummary@ object
-- includes the @Name@ and @RegexPatternSetId@ for one RegexPatternSet.
--
-- /See:/ 'newRegexPatternSetSummary' smart constructor.
data RegexPatternSetSummary = RegexPatternSetSummary'
  { -- | The @RegexPatternSetId@ for a @RegexPatternSet@. You use
    -- @RegexPatternSetId@ to get information about a @RegexPatternSet@, update
    -- a @RegexPatternSet@, remove a @RegexPatternSet@ from a @RegexMatchSet@,
    -- and delete a @RegexPatternSet@ from AWS WAF.
    --
    -- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
    -- ListRegexPatternSets.
    regexPatternSetId :: Prelude.Text,
    -- | A friendly name or description of the RegexPatternSet. You can\'t change
    -- @Name@ after you create a @RegexPatternSet@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegexPatternSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexPatternSetId', 'regexPatternSetSummary_regexPatternSetId' - The @RegexPatternSetId@ for a @RegexPatternSet@. You use
-- @RegexPatternSetId@ to get information about a @RegexPatternSet@, update
-- a @RegexPatternSet@, remove a @RegexPatternSet@ from a @RegexMatchSet@,
-- and delete a @RegexPatternSet@ from AWS WAF.
--
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
--
-- 'name', 'regexPatternSetSummary_name' - A friendly name or description of the RegexPatternSet. You can\'t change
-- @Name@ after you create a @RegexPatternSet@.
newRegexPatternSetSummary ::
  -- | 'regexPatternSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  RegexPatternSetSummary
newRegexPatternSetSummary pRegexPatternSetId_ pName_ =
  RegexPatternSetSummary'
    { regexPatternSetId =
        pRegexPatternSetId_,
      name = pName_
    }

-- | The @RegexPatternSetId@ for a @RegexPatternSet@. You use
-- @RegexPatternSetId@ to get information about a @RegexPatternSet@, update
-- a @RegexPatternSet@, remove a @RegexPatternSet@ from a @RegexMatchSet@,
-- and delete a @RegexPatternSet@ from AWS WAF.
--
-- @RegexPatternSetId@ is returned by CreateRegexPatternSet and by
-- ListRegexPatternSets.
regexPatternSetSummary_regexPatternSetId :: Lens.Lens' RegexPatternSetSummary Prelude.Text
regexPatternSetSummary_regexPatternSetId = Lens.lens (\RegexPatternSetSummary' {regexPatternSetId} -> regexPatternSetId) (\s@RegexPatternSetSummary' {} a -> s {regexPatternSetId = a} :: RegexPatternSetSummary)

-- | A friendly name or description of the RegexPatternSet. You can\'t change
-- @Name@ after you create a @RegexPatternSet@.
regexPatternSetSummary_name :: Lens.Lens' RegexPatternSetSummary Prelude.Text
regexPatternSetSummary_name = Lens.lens (\RegexPatternSetSummary' {name} -> name) (\s@RegexPatternSetSummary' {} a -> s {name = a} :: RegexPatternSetSummary)

instance Prelude.FromJSON RegexPatternSetSummary where
  parseJSON =
    Prelude.withObject
      "RegexPatternSetSummary"
      ( \x ->
          RegexPatternSetSummary'
            Prelude.<$> (x Prelude..: "RegexPatternSetId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable RegexPatternSetSummary

instance Prelude.NFData RegexPatternSetSummary

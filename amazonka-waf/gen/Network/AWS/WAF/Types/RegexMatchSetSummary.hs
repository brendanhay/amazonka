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
-- Module      : Network.AWS.WAF.Types.RegexMatchSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexMatchSetSummary where

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
-- Returned by ListRegexMatchSets. Each @RegexMatchSetSummary@ object
-- includes the @Name@ and @RegexMatchSetId@ for one RegexMatchSet.
--
-- /See:/ 'newRegexMatchSetSummary' smart constructor.
data RegexMatchSetSummary = RegexMatchSetSummary'
  { -- | The @RegexMatchSetId@ for a @RegexMatchSet@. You use @RegexMatchSetId@
    -- to get information about a @RegexMatchSet@, update a @RegexMatchSet@,
    -- remove a @RegexMatchSet@ from a @Rule@, and delete a @RegexMatchSet@
    -- from AWS WAF.
    --
    -- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
    -- ListRegexMatchSets.
    regexMatchSetId :: Prelude.Text,
    -- | A friendly name or description of the RegexMatchSet. You can\'t change
    -- @Name@ after you create a @RegexMatchSet@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegexMatchSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexMatchSetId', 'regexMatchSetSummary_regexMatchSetId' - The @RegexMatchSetId@ for a @RegexMatchSet@. You use @RegexMatchSetId@
-- to get information about a @RegexMatchSet@, update a @RegexMatchSet@,
-- remove a @RegexMatchSet@ from a @Rule@, and delete a @RegexMatchSet@
-- from AWS WAF.
--
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
--
-- 'name', 'regexMatchSetSummary_name' - A friendly name or description of the RegexMatchSet. You can\'t change
-- @Name@ after you create a @RegexMatchSet@.
newRegexMatchSetSummary ::
  -- | 'regexMatchSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  RegexMatchSetSummary
newRegexMatchSetSummary pRegexMatchSetId_ pName_ =
  RegexMatchSetSummary'
    { regexMatchSetId =
        pRegexMatchSetId_,
      name = pName_
    }

-- | The @RegexMatchSetId@ for a @RegexMatchSet@. You use @RegexMatchSetId@
-- to get information about a @RegexMatchSet@, update a @RegexMatchSet@,
-- remove a @RegexMatchSet@ from a @Rule@, and delete a @RegexMatchSet@
-- from AWS WAF.
--
-- @RegexMatchSetId@ is returned by CreateRegexMatchSet and by
-- ListRegexMatchSets.
regexMatchSetSummary_regexMatchSetId :: Lens.Lens' RegexMatchSetSummary Prelude.Text
regexMatchSetSummary_regexMatchSetId = Lens.lens (\RegexMatchSetSummary' {regexMatchSetId} -> regexMatchSetId) (\s@RegexMatchSetSummary' {} a -> s {regexMatchSetId = a} :: RegexMatchSetSummary)

-- | A friendly name or description of the RegexMatchSet. You can\'t change
-- @Name@ after you create a @RegexMatchSet@.
regexMatchSetSummary_name :: Lens.Lens' RegexMatchSetSummary Prelude.Text
regexMatchSetSummary_name = Lens.lens (\RegexMatchSetSummary' {name} -> name) (\s@RegexMatchSetSummary' {} a -> s {name = a} :: RegexMatchSetSummary)

instance Prelude.FromJSON RegexMatchSetSummary where
  parseJSON =
    Prelude.withObject
      "RegexMatchSetSummary"
      ( \x ->
          RegexMatchSetSummary'
            Prelude.<$> (x Prelude..: "RegexMatchSetId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable RegexMatchSetSummary

instance Prelude.NFData RegexMatchSetSummary

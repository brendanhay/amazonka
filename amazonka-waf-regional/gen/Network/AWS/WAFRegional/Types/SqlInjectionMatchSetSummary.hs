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
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSetSummary where

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
-- The @Id@ and @Name@ of a @SqlInjectionMatchSet@.
--
-- /See:/ 'newSqlInjectionMatchSetSummary' smart constructor.
data SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary'
  { -- | A unique identifier for a @SqlInjectionMatchSet@. You use
    -- @SqlInjectionMatchSetId@ to get information about a
    -- @SqlInjectionMatchSet@ (see GetSqlInjectionMatchSet), update a
    -- @SqlInjectionMatchSet@ (see UpdateSqlInjectionMatchSet), insert a
    -- @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see
    -- UpdateRule), and delete a @SqlInjectionMatchSet@ from AWS WAF (see
    -- DeleteSqlInjectionMatchSet).
    --
    -- @SqlInjectionMatchSetId@ is returned by CreateSqlInjectionMatchSet and
    -- by ListSqlInjectionMatchSets.
    sqlInjectionMatchSetId :: Prelude.Text,
    -- | The name of the @SqlInjectionMatchSet@, if any, specified by @Id@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SqlInjectionMatchSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sqlInjectionMatchSetId', 'sqlInjectionMatchSetSummary_sqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@. You use
-- @SqlInjectionMatchSetId@ to get information about a
-- @SqlInjectionMatchSet@ (see GetSqlInjectionMatchSet), update a
-- @SqlInjectionMatchSet@ (see UpdateSqlInjectionMatchSet), insert a
-- @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see
-- UpdateRule), and delete a @SqlInjectionMatchSet@ from AWS WAF (see
-- DeleteSqlInjectionMatchSet).
--
-- @SqlInjectionMatchSetId@ is returned by CreateSqlInjectionMatchSet and
-- by ListSqlInjectionMatchSets.
--
-- 'name', 'sqlInjectionMatchSetSummary_name' - The name of the @SqlInjectionMatchSet@, if any, specified by @Id@.
newSqlInjectionMatchSetSummary ::
  -- | 'sqlInjectionMatchSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  SqlInjectionMatchSetSummary
newSqlInjectionMatchSetSummary
  pSqlInjectionMatchSetId_
  pName_ =
    SqlInjectionMatchSetSummary'
      { sqlInjectionMatchSetId =
          pSqlInjectionMatchSetId_,
        name = pName_
      }

-- | A unique identifier for a @SqlInjectionMatchSet@. You use
-- @SqlInjectionMatchSetId@ to get information about a
-- @SqlInjectionMatchSet@ (see GetSqlInjectionMatchSet), update a
-- @SqlInjectionMatchSet@ (see UpdateSqlInjectionMatchSet), insert a
-- @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see
-- UpdateRule), and delete a @SqlInjectionMatchSet@ from AWS WAF (see
-- DeleteSqlInjectionMatchSet).
--
-- @SqlInjectionMatchSetId@ is returned by CreateSqlInjectionMatchSet and
-- by ListSqlInjectionMatchSets.
sqlInjectionMatchSetSummary_sqlInjectionMatchSetId :: Lens.Lens' SqlInjectionMatchSetSummary Prelude.Text
sqlInjectionMatchSetSummary_sqlInjectionMatchSetId = Lens.lens (\SqlInjectionMatchSetSummary' {sqlInjectionMatchSetId} -> sqlInjectionMatchSetId) (\s@SqlInjectionMatchSetSummary' {} a -> s {sqlInjectionMatchSetId = a} :: SqlInjectionMatchSetSummary)

-- | The name of the @SqlInjectionMatchSet@, if any, specified by @Id@.
sqlInjectionMatchSetSummary_name :: Lens.Lens' SqlInjectionMatchSetSummary Prelude.Text
sqlInjectionMatchSetSummary_name = Lens.lens (\SqlInjectionMatchSetSummary' {name} -> name) (\s@SqlInjectionMatchSetSummary' {} a -> s {name = a} :: SqlInjectionMatchSetSummary)

instance Prelude.FromJSON SqlInjectionMatchSetSummary where
  parseJSON =
    Prelude.withObject
      "SqlInjectionMatchSetSummary"
      ( \x ->
          SqlInjectionMatchSetSummary'
            Prelude.<$> (x Prelude..: "SqlInjectionMatchSetId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable SqlInjectionMatchSetSummary

instance Prelude.NFData SqlInjectionMatchSetSummary

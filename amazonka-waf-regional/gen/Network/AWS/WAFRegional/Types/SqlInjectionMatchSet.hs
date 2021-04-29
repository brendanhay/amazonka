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
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- A complex type that contains @SqlInjectionMatchTuple@ objects, which
-- specify the parts of web requests that you want AWS WAF to inspect for
-- snippets of malicious SQL code and, if you want AWS WAF to inspect a
-- header, the name of the header. If a @SqlInjectionMatchSet@ contains
-- more than one @SqlInjectionMatchTuple@ object, a request needs to
-- include snippets of SQL code in only one of the specified parts of the
-- request to be considered a match.
--
-- /See:/ 'newSqlInjectionMatchSet' smart constructor.
data SqlInjectionMatchSet = SqlInjectionMatchSet'
  { -- | The name, if any, of the @SqlInjectionMatchSet@.
    name :: Prelude.Maybe Prelude.Text,
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
    sqlInjectionMatchSetId :: Prelude.Text,
    -- | Specifies the parts of web requests that you want to inspect for
    -- snippets of malicious SQL code.
    sqlInjectionMatchTuples :: [SqlInjectionMatchTuple]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SqlInjectionMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'sqlInjectionMatchSet_name' - The name, if any, of the @SqlInjectionMatchSet@.
--
-- 'sqlInjectionMatchSetId', 'sqlInjectionMatchSet_sqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@. You use
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
-- 'sqlInjectionMatchTuples', 'sqlInjectionMatchSet_sqlInjectionMatchTuples' - Specifies the parts of web requests that you want to inspect for
-- snippets of malicious SQL code.
newSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Prelude.Text ->
  SqlInjectionMatchSet
newSqlInjectionMatchSet pSqlInjectionMatchSetId_ =
  SqlInjectionMatchSet'
    { name = Prelude.Nothing,
      sqlInjectionMatchSetId = pSqlInjectionMatchSetId_,
      sqlInjectionMatchTuples = Prelude.mempty
    }

-- | The name, if any, of the @SqlInjectionMatchSet@.
sqlInjectionMatchSet_name :: Lens.Lens' SqlInjectionMatchSet (Prelude.Maybe Prelude.Text)
sqlInjectionMatchSet_name = Lens.lens (\SqlInjectionMatchSet' {name} -> name) (\s@SqlInjectionMatchSet' {} a -> s {name = a} :: SqlInjectionMatchSet)

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
sqlInjectionMatchSet_sqlInjectionMatchSetId :: Lens.Lens' SqlInjectionMatchSet Prelude.Text
sqlInjectionMatchSet_sqlInjectionMatchSetId = Lens.lens (\SqlInjectionMatchSet' {sqlInjectionMatchSetId} -> sqlInjectionMatchSetId) (\s@SqlInjectionMatchSet' {} a -> s {sqlInjectionMatchSetId = a} :: SqlInjectionMatchSet)

-- | Specifies the parts of web requests that you want to inspect for
-- snippets of malicious SQL code.
sqlInjectionMatchSet_sqlInjectionMatchTuples :: Lens.Lens' SqlInjectionMatchSet [SqlInjectionMatchTuple]
sqlInjectionMatchSet_sqlInjectionMatchTuples = Lens.lens (\SqlInjectionMatchSet' {sqlInjectionMatchTuples} -> sqlInjectionMatchTuples) (\s@SqlInjectionMatchSet' {} a -> s {sqlInjectionMatchTuples = a} :: SqlInjectionMatchSet) Prelude.. Prelude._Coerce

instance Prelude.FromJSON SqlInjectionMatchSet where
  parseJSON =
    Prelude.withObject
      "SqlInjectionMatchSet"
      ( \x ->
          SqlInjectionMatchSet'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "SqlInjectionMatchSetId")
            Prelude.<*> ( x Prelude..:? "SqlInjectionMatchTuples"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SqlInjectionMatchSet

instance Prelude.NFData SqlInjectionMatchSet

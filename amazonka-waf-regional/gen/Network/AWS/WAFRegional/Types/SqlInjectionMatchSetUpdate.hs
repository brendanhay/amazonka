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
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSetUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAFRegional.Types.ChangeAction
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
-- Specifies the part of a web request that you want to inspect for
-- snippets of malicious SQL code and indicates whether you want to add the
-- specification to a SqlInjectionMatchSet or delete it from a
-- @SqlInjectionMatchSet@.
--
-- /See:/ 'newSqlInjectionMatchSetUpdate' smart constructor.
data SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate'
  { -- | Specify @INSERT@ to add a SqlInjectionMatchSetUpdate to a
    -- SqlInjectionMatchSet. Use @DELETE@ to remove a
    -- @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@.
    action :: ChangeAction,
    -- | Specifies the part of a web request that you want AWS WAF to inspect for
    -- snippets of malicious SQL code and, if you want AWS WAF to inspect a
    -- header, the name of the header.
    sqlInjectionMatchTuple :: SqlInjectionMatchTuple
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SqlInjectionMatchSetUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'sqlInjectionMatchSetUpdate_action' - Specify @INSERT@ to add a SqlInjectionMatchSetUpdate to a
-- SqlInjectionMatchSet. Use @DELETE@ to remove a
-- @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@.
--
-- 'sqlInjectionMatchTuple', 'sqlInjectionMatchSetUpdate_sqlInjectionMatchTuple' - Specifies the part of a web request that you want AWS WAF to inspect for
-- snippets of malicious SQL code and, if you want AWS WAF to inspect a
-- header, the name of the header.
newSqlInjectionMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'sqlInjectionMatchTuple'
  SqlInjectionMatchTuple ->
  SqlInjectionMatchSetUpdate
newSqlInjectionMatchSetUpdate
  pAction_
  pSqlInjectionMatchTuple_ =
    SqlInjectionMatchSetUpdate'
      { action = pAction_,
        sqlInjectionMatchTuple =
          pSqlInjectionMatchTuple_
      }

-- | Specify @INSERT@ to add a SqlInjectionMatchSetUpdate to a
-- SqlInjectionMatchSet. Use @DELETE@ to remove a
-- @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@.
sqlInjectionMatchSetUpdate_action :: Lens.Lens' SqlInjectionMatchSetUpdate ChangeAction
sqlInjectionMatchSetUpdate_action = Lens.lens (\SqlInjectionMatchSetUpdate' {action} -> action) (\s@SqlInjectionMatchSetUpdate' {} a -> s {action = a} :: SqlInjectionMatchSetUpdate)

-- | Specifies the part of a web request that you want AWS WAF to inspect for
-- snippets of malicious SQL code and, if you want AWS WAF to inspect a
-- header, the name of the header.
sqlInjectionMatchSetUpdate_sqlInjectionMatchTuple :: Lens.Lens' SqlInjectionMatchSetUpdate SqlInjectionMatchTuple
sqlInjectionMatchSetUpdate_sqlInjectionMatchTuple = Lens.lens (\SqlInjectionMatchSetUpdate' {sqlInjectionMatchTuple} -> sqlInjectionMatchTuple) (\s@SqlInjectionMatchSetUpdate' {} a -> s {sqlInjectionMatchTuple = a} :: SqlInjectionMatchSetUpdate)

instance Prelude.Hashable SqlInjectionMatchSetUpdate

instance Prelude.NFData SqlInjectionMatchSetUpdate

instance Prelude.ToJSON SqlInjectionMatchSetUpdate where
  toJSON SqlInjectionMatchSetUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ( "SqlInjectionMatchTuple"
                  Prelude..= sqlInjectionMatchTuple
              )
          ]
      )

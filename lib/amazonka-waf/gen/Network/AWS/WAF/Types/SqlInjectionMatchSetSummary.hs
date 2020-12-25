{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SqlInjectionMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SqlInjectionMatchSetSummary
  ( SqlInjectionMatchSetSummary (..),

    -- * Smart constructor
    mkSqlInjectionMatchSetSummary,

    -- * Lenses
    simssSqlInjectionMatchSetId,
    simssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.Name as Types
import qualified Network.AWS.WAF.Types.SqlInjectionMatchSetId as Types

-- | The @Id@ and @Name@ of a @SqlInjectionMatchSet@ .
--
-- /See:/ 'mkSqlInjectionMatchSetSummary' smart constructor.
data SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary'
  { -- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
    --
    -- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
    sqlInjectionMatchSetId :: Types.SqlInjectionMatchSetId,
    -- | The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SqlInjectionMatchSetSummary' value with any optional fields omitted.
mkSqlInjectionMatchSetSummary ::
  -- | 'sqlInjectionMatchSetId'
  Types.SqlInjectionMatchSetId ->
  -- | 'name'
  Types.Name ->
  SqlInjectionMatchSetSummary
mkSqlInjectionMatchSetSummary sqlInjectionMatchSetId name =
  SqlInjectionMatchSetSummary' {sqlInjectionMatchSetId, name}

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simssSqlInjectionMatchSetId :: Lens.Lens' SqlInjectionMatchSetSummary Types.SqlInjectionMatchSetId
simssSqlInjectionMatchSetId = Lens.field @"sqlInjectionMatchSetId"
{-# DEPRECATED simssSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

-- | The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simssName :: Lens.Lens' SqlInjectionMatchSetSummary Types.Name
simssName = Lens.field @"name"
{-# DEPRECATED simssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON SqlInjectionMatchSetSummary where
  parseJSON =
    Core.withObject "SqlInjectionMatchSetSummary" Core.$
      \x ->
        SqlInjectionMatchSetSummary'
          Core.<$> (x Core..: "SqlInjectionMatchSetId") Core.<*> (x Core..: "Name")

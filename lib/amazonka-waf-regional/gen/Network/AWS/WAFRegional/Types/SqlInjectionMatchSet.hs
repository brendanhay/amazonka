{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
  ( SqlInjectionMatchSet (..)
  -- * Smart constructor
  , mkSqlInjectionMatchSet
  -- * Lenses
  , simsSqlInjectionMatchSetId
  , simsSqlInjectionMatchTuples
  , simsName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAFRegional.Types.ResourceId as Types
import qualified Network.AWS.WAFRegional.Types.ResourceName as Types
import qualified Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple as Types

-- | A complex type that contains @SqlInjectionMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header. If a @SqlInjectionMatchSet@ contains more than one @SqlInjectionMatchTuple@ object, a request needs to include snippets of SQL code in only one of the specified parts of the request to be considered a match.
--
-- /See:/ 'mkSqlInjectionMatchSet' smart constructor.
data SqlInjectionMatchSet = SqlInjectionMatchSet'
  { sqlInjectionMatchSetId :: Types.ResourceId
    -- ^ A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
  , sqlInjectionMatchTuples :: [Types.SqlInjectionMatchTuple]
    -- ^ Specifies the parts of web requests that you want to inspect for snippets of malicious SQL code.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name, if any, of the @SqlInjectionMatchSet@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SqlInjectionMatchSet' value with any optional fields omitted.
mkSqlInjectionMatchSet
    :: Types.ResourceId -- ^ 'sqlInjectionMatchSetId'
    -> SqlInjectionMatchSet
mkSqlInjectionMatchSet sqlInjectionMatchSetId
  = SqlInjectionMatchSet'{sqlInjectionMatchSetId,
                          sqlInjectionMatchTuples = Core.mempty, name = Core.Nothing}

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsSqlInjectionMatchSetId :: Lens.Lens' SqlInjectionMatchSet Types.ResourceId
simsSqlInjectionMatchSetId = Lens.field @"sqlInjectionMatchSetId"
{-# INLINEABLE simsSqlInjectionMatchSetId #-}
{-# DEPRECATED sqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead"  #-}

-- | Specifies the parts of web requests that you want to inspect for snippets of malicious SQL code.
--
-- /Note:/ Consider using 'sqlInjectionMatchTuples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsSqlInjectionMatchTuples :: Lens.Lens' SqlInjectionMatchSet [Types.SqlInjectionMatchTuple]
simsSqlInjectionMatchTuples = Lens.field @"sqlInjectionMatchTuples"
{-# INLINEABLE simsSqlInjectionMatchTuples #-}
{-# DEPRECATED sqlInjectionMatchTuples "Use generic-lens or generic-optics with 'sqlInjectionMatchTuples' instead"  #-}

-- | The name, if any, of the @SqlInjectionMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsName :: Lens.Lens' SqlInjectionMatchSet (Core.Maybe Types.ResourceName)
simsName = Lens.field @"name"
{-# INLINEABLE simsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON SqlInjectionMatchSet where
        parseJSON
          = Core.withObject "SqlInjectionMatchSet" Core.$
              \ x ->
                SqlInjectionMatchSet' Core.<$>
                  (x Core..: "SqlInjectionMatchSetId") Core.<*>
                    x Core..:? "SqlInjectionMatchTuples" Core..!= Core.mempty
                    Core.<*> x Core..:? "Name"

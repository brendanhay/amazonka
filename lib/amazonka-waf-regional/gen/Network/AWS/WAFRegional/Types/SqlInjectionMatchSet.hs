-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSet
  ( SqlInjectionMatchSet (..),

    -- * Smart constructor
    mkSqlInjectionMatchSet,

    -- * Lenses
    simsName,
    simsSqlInjectionMatchSetId,
    simsSqlInjectionMatchTuples,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.SqlInjectionMatchTuple

-- | A complex type that contains @SqlInjectionMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header. If a @SqlInjectionMatchSet@ contains more than one @SqlInjectionMatchTuple@ object, a request needs to include snippets of SQL code in only one of the specified parts of the request to be considered a match.
--
-- /See:/ 'mkSqlInjectionMatchSet' smart constructor.
data SqlInjectionMatchSet = SqlInjectionMatchSet'
  { name ::
      Lude.Maybe Lude.Text,
    sqlInjectionMatchSetId :: Lude.Text,
    sqlInjectionMatchTuples ::
      [SqlInjectionMatchTuple]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- * 'name' - The name, if any, of the @SqlInjectionMatchSet@ .
-- * 'sqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
-- * 'sqlInjectionMatchTuples' - Specifies the parts of web requests that you want to inspect for snippets of malicious SQL code.
mkSqlInjectionMatchSet ::
  -- | 'sqlInjectionMatchSetId'
  Lude.Text ->
  SqlInjectionMatchSet
mkSqlInjectionMatchSet pSqlInjectionMatchSetId_ =
  SqlInjectionMatchSet'
    { name = Lude.Nothing,
      sqlInjectionMatchSetId = pSqlInjectionMatchSetId_,
      sqlInjectionMatchTuples = Lude.mempty
    }

-- | The name, if any, of the @SqlInjectionMatchSet@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsName :: Lens.Lens' SqlInjectionMatchSet (Lude.Maybe Lude.Text)
simsName = Lens.lens (name :: SqlInjectionMatchSet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SqlInjectionMatchSet)
{-# DEPRECATED simsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsSqlInjectionMatchSetId :: Lens.Lens' SqlInjectionMatchSet Lude.Text
simsSqlInjectionMatchSetId = Lens.lens (sqlInjectionMatchSetId :: SqlInjectionMatchSet -> Lude.Text) (\s a -> s {sqlInjectionMatchSetId = a} :: SqlInjectionMatchSet)
{-# DEPRECATED simsSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

-- | Specifies the parts of web requests that you want to inspect for snippets of malicious SQL code.
--
-- /Note:/ Consider using 'sqlInjectionMatchTuples' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simsSqlInjectionMatchTuples :: Lens.Lens' SqlInjectionMatchSet [SqlInjectionMatchTuple]
simsSqlInjectionMatchTuples = Lens.lens (sqlInjectionMatchTuples :: SqlInjectionMatchSet -> [SqlInjectionMatchTuple]) (\s a -> s {sqlInjectionMatchTuples = a} :: SqlInjectionMatchSet)
{-# DEPRECATED simsSqlInjectionMatchTuples "Use generic-lens or generic-optics with 'sqlInjectionMatchTuples' instead." #-}

instance Lude.FromJSON SqlInjectionMatchSet where
  parseJSON =
    Lude.withObject
      "SqlInjectionMatchSet"
      ( \x ->
          SqlInjectionMatchSet'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..: "SqlInjectionMatchSetId")
            Lude.<*> (x Lude..:? "SqlInjectionMatchTuples" Lude..!= Lude.mempty)
      )

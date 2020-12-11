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
import qualified Network.AWS.Prelude as Lude

-- | The @Id@ and @Name@ of a @SqlInjectionMatchSet@ .
--
-- /See:/ 'mkSqlInjectionMatchSetSummary' smart constructor.
data SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary'
  { sqlInjectionMatchSetId ::
      Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SqlInjectionMatchSetSummary' with the minimum fields required to make a request.
--
-- * 'name' - The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
-- * 'sqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
mkSqlInjectionMatchSetSummary ::
  -- | 'sqlInjectionMatchSetId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  SqlInjectionMatchSetSummary
mkSqlInjectionMatchSetSummary pSqlInjectionMatchSetId_ pName_ =
  SqlInjectionMatchSetSummary'
    { sqlInjectionMatchSetId =
        pSqlInjectionMatchSetId_,
      name = pName_
    }

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simssSqlInjectionMatchSetId :: Lens.Lens' SqlInjectionMatchSetSummary Lude.Text
simssSqlInjectionMatchSetId = Lens.lens (sqlInjectionMatchSetId :: SqlInjectionMatchSetSummary -> Lude.Text) (\s a -> s {sqlInjectionMatchSetId = a} :: SqlInjectionMatchSetSummary)
{-# DEPRECATED simssSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

-- | The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simssName :: Lens.Lens' SqlInjectionMatchSetSummary Lude.Text
simssName = Lens.lens (name :: SqlInjectionMatchSetSummary -> Lude.Text) (\s a -> s {name = a} :: SqlInjectionMatchSetSummary)
{-# DEPRECATED simssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON SqlInjectionMatchSetSummary where
  parseJSON =
    Lude.withObject
      "SqlInjectionMatchSetSummary"
      ( \x ->
          SqlInjectionMatchSetSummary'
            Lude.<$> (x Lude..: "SqlInjectionMatchSetId") Lude.<*> (x Lude..: "Name")
      )

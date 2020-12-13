{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.SqlInjectionMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.SqlInjectionMatchSetSummary
  ( SqlInjectionMatchSetSummary (..),

    -- * Smart constructor
    mkSqlInjectionMatchSetSummary,

    -- * Lenses
    simssName,
    simssSqlInjectionMatchSetId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @Id@ and @Name@ of a @SqlInjectionMatchSet@ .
--
-- /See:/ 'mkSqlInjectionMatchSetSummary' smart constructor.
data SqlInjectionMatchSetSummary = SqlInjectionMatchSetSummary'
  { -- | The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
    name :: Lude.Text,
    -- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
    --
    -- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
    sqlInjectionMatchSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SqlInjectionMatchSetSummary' with the minimum fields required to make a request.
--
-- * 'name' - The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
-- * 'sqlInjectionMatchSetId' - A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
mkSqlInjectionMatchSetSummary ::
  -- | 'name'
  Lude.Text ->
  -- | 'sqlInjectionMatchSetId'
  Lude.Text ->
  SqlInjectionMatchSetSummary
mkSqlInjectionMatchSetSummary pName_ pSqlInjectionMatchSetId_ =
  SqlInjectionMatchSetSummary'
    { name = pName_,
      sqlInjectionMatchSetId = pSqlInjectionMatchSetId_
    }

-- | The name of the @SqlInjectionMatchSet@ , if any, specified by @Id@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simssName :: Lens.Lens' SqlInjectionMatchSetSummary Lude.Text
simssName = Lens.lens (name :: SqlInjectionMatchSetSummary -> Lude.Text) (\s a -> s {name = a} :: SqlInjectionMatchSetSummary)
{-# DEPRECATED simssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A unique identifier for a @SqlInjectionMatchSet@ . You use @SqlInjectionMatchSetId@ to get information about a @SqlInjectionMatchSet@ (see 'GetSqlInjectionMatchSet' ), update a @SqlInjectionMatchSet@ (see 'UpdateSqlInjectionMatchSet' ), insert a @SqlInjectionMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @SqlInjectionMatchSet@ from AWS WAF (see 'DeleteSqlInjectionMatchSet' ).
--
-- @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
simssSqlInjectionMatchSetId :: Lens.Lens' SqlInjectionMatchSetSummary Lude.Text
simssSqlInjectionMatchSetId = Lens.lens (sqlInjectionMatchSetId :: SqlInjectionMatchSetSummary -> Lude.Text) (\s a -> s {sqlInjectionMatchSetId = a} :: SqlInjectionMatchSetSummary)
{-# DEPRECATED simssSqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead." #-}

instance Lude.FromJSON SqlInjectionMatchSetSummary where
  parseJSON =
    Lude.withObject
      "SqlInjectionMatchSetSummary"
      ( \x ->
          SqlInjectionMatchSetSummary'
            Lude.<$> (x Lude..: "Name") Lude.<*> (x Lude..: "SqlInjectionMatchSetId")
      )

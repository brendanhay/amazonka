{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ExpressionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ExpressionStatus
  ( ExpressionStatus (..),

    -- * Smart constructor
    mkExpressionStatus,

    -- * Lenses
    esStatus,
    esOptions,
  )
where

import Network.AWS.CloudSearch.Types.Expression
import Network.AWS.CloudSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The value of an @Expression@ and its current status.
--
-- /See:/ 'mkExpressionStatus' smart constructor.
data ExpressionStatus = ExpressionStatus'
  { status :: OptionStatus,
    -- | The expression that is evaluated for sorting while processing a search request.
    options :: Expression
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExpressionStatus' with the minimum fields required to make a request.
--
-- * 'status' -
-- * 'options' - The expression that is evaluated for sorting while processing a search request.
mkExpressionStatus ::
  -- | 'status'
  OptionStatus ->
  -- | 'options'
  Expression ->
  ExpressionStatus
mkExpressionStatus pStatus_ pOptions_ =
  ExpressionStatus' {status = pStatus_, options = pOptions_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatus :: Lens.Lens' ExpressionStatus OptionStatus
esStatus = Lens.lens (status :: ExpressionStatus -> OptionStatus) (\s a -> s {status = a} :: ExpressionStatus)
{-# DEPRECATED esStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The expression that is evaluated for sorting while processing a search request.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esOptions :: Lens.Lens' ExpressionStatus Expression
esOptions = Lens.lens (options :: ExpressionStatus -> Expression) (\s a -> s {options = a} :: ExpressionStatus)
{-# DEPRECATED esOptions "Use generic-lens or generic-optics with 'options' instead." #-}

instance Lude.FromXML ExpressionStatus where
  parseXML x =
    ExpressionStatus'
      Lude.<$> (x Lude..@ "Status") Lude.<*> (x Lude..@ "Options")

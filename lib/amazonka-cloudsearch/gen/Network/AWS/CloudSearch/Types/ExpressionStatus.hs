{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ExpressionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.ExpressionStatus
  ( ExpressionStatus (..)
  -- * Smart constructor
  , mkExpressionStatus
  -- * Lenses
  , esOptions
  , esStatus
  ) where

import qualified Network.AWS.CloudSearch.Types.Expression as Types
import qualified Network.AWS.CloudSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The value of an @Expression@ and its current status.
--
-- /See:/ 'mkExpressionStatus' smart constructor.
data ExpressionStatus = ExpressionStatus'
  { options :: Types.Expression
    -- ^ The expression that is evaluated for sorting while processing a search request.
  , status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ExpressionStatus' value with any optional fields omitted.
mkExpressionStatus
    :: Types.Expression -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> ExpressionStatus
mkExpressionStatus options status
  = ExpressionStatus'{options, status}

-- | The expression that is evaluated for sorting while processing a search request.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esOptions :: Lens.Lens' ExpressionStatus Types.Expression
esOptions = Lens.field @"options"
{-# INLINEABLE esOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esStatus :: Lens.Lens' ExpressionStatus Types.OptionStatus
esStatus = Lens.field @"status"
{-# INLINEABLE esStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML ExpressionStatus where
        parseXML x
          = ExpressionStatus' Core.<$>
              (x Core..@ "Options") Core.<*> x Core..@ "Status"

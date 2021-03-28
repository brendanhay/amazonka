{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexFieldStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.IndexFieldStatus
  ( IndexFieldStatus (..)
  -- * Smart constructor
  , mkIndexFieldStatus
  -- * Lenses
  , ifsOptions
  , ifsStatus
  ) where

import qualified Network.AWS.CloudSearch.Types.IndexField as Types
import qualified Network.AWS.CloudSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The value of an @IndexField@ and its current status.
--
-- /See:/ 'mkIndexFieldStatus' smart constructor.
data IndexFieldStatus = IndexFieldStatus'
  { options :: Types.IndexField
  , status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'IndexFieldStatus' value with any optional fields omitted.
mkIndexFieldStatus
    :: Types.IndexField -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> IndexFieldStatus
mkIndexFieldStatus options status
  = IndexFieldStatus'{options, status}

-- | Undocumented field.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsOptions :: Lens.Lens' IndexFieldStatus Types.IndexField
ifsOptions = Lens.field @"options"
{-# INLINEABLE ifsOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifsStatus :: Lens.Lens' IndexFieldStatus Types.OptionStatus
ifsStatus = Lens.field @"status"
{-# INLINEABLE ifsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML IndexFieldStatus where
        parseXML x
          = IndexFieldStatus' Core.<$>
              (x Core..@ "Options") Core.<*> x Core..@ "Status"

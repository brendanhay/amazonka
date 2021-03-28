{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.SuggesterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.SuggesterStatus
  ( SuggesterStatus (..)
  -- * Smart constructor
  , mkSuggesterStatus
  -- * Lenses
  , ssOptions
  , ssStatus
  ) where

import qualified Network.AWS.CloudSearch.Types.OptionStatus as Types
import qualified Network.AWS.CloudSearch.Types.Suggester as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The value of a @Suggester@ and its current status.
--
-- /See:/ 'mkSuggesterStatus' smart constructor.
data SuggesterStatus = SuggesterStatus'
  { options :: Types.Suggester
  , status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'SuggesterStatus' value with any optional fields omitted.
mkSuggesterStatus
    :: Types.Suggester -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> SuggesterStatus
mkSuggesterStatus options status
  = SuggesterStatus'{options, status}

-- | Undocumented field.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssOptions :: Lens.Lens' SuggesterStatus Types.Suggester
ssOptions = Lens.field @"options"
{-# INLINEABLE ssOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStatus :: Lens.Lens' SuggesterStatus Types.OptionStatus
ssStatus = Lens.field @"status"
{-# INLINEABLE ssStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML SuggesterStatus where
        parseXML x
          = SuggesterStatus' Core.<$>
              (x Core..@ "Options") Core.<*> x Core..@ "Status"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ReportedOs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.ReportedOs
  ( ReportedOs (..)
  -- * Smart constructor
  , mkReportedOs
  -- * Lenses
  , roFamily
  , roName
  , roVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A registered instance's reported operating system.
--
-- /See:/ 'mkReportedOs' smart constructor.
data ReportedOs = ReportedOs'
  { family :: Core.Maybe Core.Text
    -- ^ The operating system family.
  , name :: Core.Maybe Core.Text
    -- ^ The operating system name.
  , version :: Core.Maybe Core.Text
    -- ^ The operating system version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportedOs' value with any optional fields omitted.
mkReportedOs
    :: ReportedOs
mkReportedOs
  = ReportedOs'{family = Core.Nothing, name = Core.Nothing,
                version = Core.Nothing}

-- | The operating system family.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roFamily :: Lens.Lens' ReportedOs (Core.Maybe Core.Text)
roFamily = Lens.field @"family"
{-# INLINEABLE roFamily #-}
{-# DEPRECATED family "Use generic-lens or generic-optics with 'family' instead"  #-}

-- | The operating system name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roName :: Lens.Lens' ReportedOs (Core.Maybe Core.Text)
roName = Lens.field @"name"
{-# INLINEABLE roName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The operating system version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roVersion :: Lens.Lens' ReportedOs (Core.Maybe Core.Text)
roVersion = Lens.field @"version"
{-# INLINEABLE roVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON ReportedOs where
        parseJSON
          = Core.withObject "ReportedOs" Core.$
              \ x ->
                ReportedOs' Core.<$>
                  (x Core..:? "Family") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "Version"

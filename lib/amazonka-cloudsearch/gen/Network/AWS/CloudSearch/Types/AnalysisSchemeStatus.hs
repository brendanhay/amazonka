{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AnalysisSchemeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisSchemeStatus
  ( AnalysisSchemeStatus (..),

    -- * Smart constructor
    mkAnalysisSchemeStatus,

    -- * Lenses
    assOptions,
    assStatus,
  )
where

import qualified Network.AWS.CloudSearch.Types.AnalysisScheme as Types
import qualified Network.AWS.CloudSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status and configuration of an @AnalysisScheme@ .
--
-- /See:/ 'mkAnalysisSchemeStatus' smart constructor.
data AnalysisSchemeStatus = AnalysisSchemeStatus'
  { options :: Types.AnalysisScheme,
    status :: Types.OptionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AnalysisSchemeStatus' value with any optional fields omitted.
mkAnalysisSchemeStatus ::
  -- | 'options'
  Types.AnalysisScheme ->
  -- | 'status'
  Types.OptionStatus ->
  AnalysisSchemeStatus
mkAnalysisSchemeStatus options status =
  AnalysisSchemeStatus' {options, status}

-- | Undocumented field.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assOptions :: Lens.Lens' AnalysisSchemeStatus Types.AnalysisScheme
assOptions = Lens.field @"options"
{-# DEPRECATED assOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
assStatus :: Lens.Lens' AnalysisSchemeStatus Types.OptionStatus
assStatus = Lens.field @"status"
{-# DEPRECATED assStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML AnalysisSchemeStatus where
  parseXML x =
    AnalysisSchemeStatus'
      Core.<$> (x Core..@ "Options") Core.<*> (x Core..@ "Status")

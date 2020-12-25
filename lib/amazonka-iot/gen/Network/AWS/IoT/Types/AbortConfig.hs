{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AbortConfig
  ( AbortConfig (..),

    -- * Smart constructor
    mkAbortConfig,

    -- * Lenses
    acCriteriaList,
  )
where

import qualified Network.AWS.IoT.Types.AbortCriteria as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The criteria that determine when and how a job abort takes place.
--
-- /See:/ 'mkAbortConfig' smart constructor.
newtype AbortConfig = AbortConfig'
  { -- | The list of criteria that determine when and how to abort the job.
    criteriaList :: Core.NonEmpty Types.AbortCriteria
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AbortConfig' value with any optional fields omitted.
mkAbortConfig ::
  -- | 'criteriaList'
  Core.NonEmpty Types.AbortCriteria ->
  AbortConfig
mkAbortConfig criteriaList = AbortConfig' {criteriaList}

-- | The list of criteria that determine when and how to abort the job.
--
-- /Note:/ Consider using 'criteriaList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acCriteriaList :: Lens.Lens' AbortConfig (Core.NonEmpty Types.AbortCriteria)
acCriteriaList = Lens.field @"criteriaList"
{-# DEPRECATED acCriteriaList "Use generic-lens or generic-optics with 'criteriaList' instead." #-}

instance Core.FromJSON AbortConfig where
  toJSON AbortConfig {..} =
    Core.object
      (Core.catMaybes [Core.Just ("criteriaList" Core..= criteriaList)])

instance Core.FromJSON AbortConfig where
  parseJSON =
    Core.withObject "AbortConfig" Core.$
      \x -> AbortConfig' Core.<$> (x Core..: "criteriaList")

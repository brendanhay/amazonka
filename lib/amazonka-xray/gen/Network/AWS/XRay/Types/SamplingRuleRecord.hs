{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingRuleRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingRuleRecord
  ( SamplingRuleRecord (..),

    -- * Smart constructor
    mkSamplingRuleRecord,

    -- * Lenses
    srrCreatedAt,
    srrModifiedAt,
    srrSamplingRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.SamplingRule as Types

-- | A 'SamplingRule' and its metadata.
--
-- /See:/ 'mkSamplingRuleRecord' smart constructor.
data SamplingRuleRecord = SamplingRuleRecord'
  { -- | When the rule was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | When the rule was last modified.
    modifiedAt :: Core.Maybe Core.NominalDiffTime,
    -- | The sampling rule.
    samplingRule :: Core.Maybe Types.SamplingRule
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SamplingRuleRecord' value with any optional fields omitted.
mkSamplingRuleRecord ::
  SamplingRuleRecord
mkSamplingRuleRecord =
  SamplingRuleRecord'
    { createdAt = Core.Nothing,
      modifiedAt = Core.Nothing,
      samplingRule = Core.Nothing
    }

-- | When the rule was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrCreatedAt :: Lens.Lens' SamplingRuleRecord (Core.Maybe Core.NominalDiffTime)
srrCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED srrCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | When the rule was last modified.
--
-- /Note:/ Consider using 'modifiedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrModifiedAt :: Lens.Lens' SamplingRuleRecord (Core.Maybe Core.NominalDiffTime)
srrModifiedAt = Lens.field @"modifiedAt"
{-# DEPRECATED srrModifiedAt "Use generic-lens or generic-optics with 'modifiedAt' instead." #-}

-- | The sampling rule.
--
-- /Note:/ Consider using 'samplingRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srrSamplingRule :: Lens.Lens' SamplingRuleRecord (Core.Maybe Types.SamplingRule)
srrSamplingRule = Lens.field @"samplingRule"
{-# DEPRECATED srrSamplingRule "Use generic-lens or generic-optics with 'samplingRule' instead." #-}

instance Core.FromJSON SamplingRuleRecord where
  parseJSON =
    Core.withObject "SamplingRuleRecord" Core.$
      \x ->
        SamplingRuleRecord'
          Core.<$> (x Core..:? "CreatedAt")
          Core.<*> (x Core..:? "ModifiedAt")
          Core.<*> (x Core..:? "SamplingRule")

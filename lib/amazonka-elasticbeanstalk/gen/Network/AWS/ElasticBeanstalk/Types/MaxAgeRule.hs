{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
  ( MaxAgeRule (..),

    -- * Smart constructor
    mkMaxAgeRule,

    -- * Lenses
    marEnabled,
    marDeleteSourceFromS3,
    marMaxAgeInDays,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A lifecycle rule that deletes application versions after the specified number of days.
--
-- /See:/ 'mkMaxAgeRule' smart constructor.
data MaxAgeRule = MaxAgeRule'
  { -- | Specify @true@ to apply the rule, or @false@ to disable it.
    enabled :: Core.Bool,
    -- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
    deleteSourceFromS3 :: Core.Maybe Core.Bool,
    -- | Specify the number of days to retain an application versions.
    maxAgeInDays :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaxAgeRule' value with any optional fields omitted.
mkMaxAgeRule ::
  -- | 'enabled'
  Core.Bool ->
  MaxAgeRule
mkMaxAgeRule enabled =
  MaxAgeRule'
    { enabled,
      deleteSourceFromS3 = Core.Nothing,
      maxAgeInDays = Core.Nothing
    }

-- | Specify @true@ to apply the rule, or @false@ to disable it.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
marEnabled :: Lens.Lens' MaxAgeRule Core.Bool
marEnabled = Lens.field @"enabled"
{-# DEPRECATED marEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
--
-- /Note:/ Consider using 'deleteSourceFromS3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
marDeleteSourceFromS3 :: Lens.Lens' MaxAgeRule (Core.Maybe Core.Bool)
marDeleteSourceFromS3 = Lens.field @"deleteSourceFromS3"
{-# DEPRECATED marDeleteSourceFromS3 "Use generic-lens or generic-optics with 'deleteSourceFromS3' instead." #-}

-- | Specify the number of days to retain an application versions.
--
-- /Note:/ Consider using 'maxAgeInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
marMaxAgeInDays :: Lens.Lens' MaxAgeRule (Core.Maybe Core.Int)
marMaxAgeInDays = Lens.field @"maxAgeInDays"
{-# DEPRECATED marMaxAgeInDays "Use generic-lens or generic-optics with 'maxAgeInDays' instead." #-}

instance Core.FromXML MaxAgeRule where
  parseXML x =
    MaxAgeRule'
      Core.<$> (x Core..@ "Enabled")
      Core.<*> (x Core..@? "DeleteSourceFromS3")
      Core.<*> (x Core..@? "MaxAgeInDays")

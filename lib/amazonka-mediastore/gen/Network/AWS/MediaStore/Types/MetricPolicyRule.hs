{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.MetricPolicyRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.MetricPolicyRule
  ( MetricPolicyRule (..),

    -- * Smart constructor
    mkMetricPolicyRule,

    -- * Lenses
    mprObjectGroup,
    mprObjectGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaStore.Types.ObjectGroup as Types
import qualified Network.AWS.MediaStore.Types.ObjectGroupName as Types
import qualified Network.AWS.Prelude as Core

-- | A setting that enables metrics at the object level. Each rule contains an object group and an object group name. If the policy includes the MetricPolicyRules parameter, you must include at least one rule. Each metric policy can include up to five rules by default. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
--
-- /See:/ 'mkMetricPolicyRule' smart constructor.
data MetricPolicyRule = MetricPolicyRule'
  { -- | A path or file name that defines which objects to include in the group. Wildcards (*) are acceptable.
    objectGroup :: Types.ObjectGroup,
    -- | A name that allows you to refer to the object group.
    objectGroupName :: Types.ObjectGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MetricPolicyRule' value with any optional fields omitted.
mkMetricPolicyRule ::
  -- | 'objectGroup'
  Types.ObjectGroup ->
  -- | 'objectGroupName'
  Types.ObjectGroupName ->
  MetricPolicyRule
mkMetricPolicyRule objectGroup objectGroupName =
  MetricPolicyRule' {objectGroup, objectGroupName}

-- | A path or file name that defines which objects to include in the group. Wildcards (*) are acceptable.
--
-- /Note:/ Consider using 'objectGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprObjectGroup :: Lens.Lens' MetricPolicyRule Types.ObjectGroup
mprObjectGroup = Lens.field @"objectGroup"
{-# DEPRECATED mprObjectGroup "Use generic-lens or generic-optics with 'objectGroup' instead." #-}

-- | A name that allows you to refer to the object group.
--
-- /Note:/ Consider using 'objectGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprObjectGroupName :: Lens.Lens' MetricPolicyRule Types.ObjectGroupName
mprObjectGroupName = Lens.field @"objectGroupName"
{-# DEPRECATED mprObjectGroupName "Use generic-lens or generic-optics with 'objectGroupName' instead." #-}

instance Core.FromJSON MetricPolicyRule where
  toJSON MetricPolicyRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectGroup" Core..= objectGroup),
            Core.Just ("ObjectGroupName" Core..= objectGroupName)
          ]
      )

instance Core.FromJSON MetricPolicyRule where
  parseJSON =
    Core.withObject "MetricPolicyRule" Core.$
      \x ->
        MetricPolicyRule'
          Core.<$> (x Core..: "ObjectGroup") Core.<*> (x Core..: "ObjectGroupName")

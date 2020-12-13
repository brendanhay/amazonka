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
import qualified Network.AWS.Prelude as Lude

-- | A setting that enables metrics at the object level. Each rule contains an object group and an object group name. If the policy includes the MetricPolicyRules parameter, you must include at least one rule. Each metric policy can include up to five rules by default. You can also <https://console.aws.amazon.com/servicequotas/home?region=us-east-1#!/services/mediastore/quotas request a quota increase> to allow up to 300 rules per policy.
--
-- /See:/ 'mkMetricPolicyRule' smart constructor.
data MetricPolicyRule = MetricPolicyRule'
  { -- | A path or file name that defines which objects to include in the group. Wildcards (*) are acceptable.
    objectGroup :: Lude.Text,
    -- | A name that allows you to refer to the object group.
    objectGroupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MetricPolicyRule' with the minimum fields required to make a request.
--
-- * 'objectGroup' - A path or file name that defines which objects to include in the group. Wildcards (*) are acceptable.
-- * 'objectGroupName' - A name that allows you to refer to the object group.
mkMetricPolicyRule ::
  -- | 'objectGroup'
  Lude.Text ->
  -- | 'objectGroupName'
  Lude.Text ->
  MetricPolicyRule
mkMetricPolicyRule pObjectGroup_ pObjectGroupName_ =
  MetricPolicyRule'
    { objectGroup = pObjectGroup_,
      objectGroupName = pObjectGroupName_
    }

-- | A path or file name that defines which objects to include in the group. Wildcards (*) are acceptable.
--
-- /Note:/ Consider using 'objectGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprObjectGroup :: Lens.Lens' MetricPolicyRule Lude.Text
mprObjectGroup = Lens.lens (objectGroup :: MetricPolicyRule -> Lude.Text) (\s a -> s {objectGroup = a} :: MetricPolicyRule)
{-# DEPRECATED mprObjectGroup "Use generic-lens or generic-optics with 'objectGroup' instead." #-}

-- | A name that allows you to refer to the object group.
--
-- /Note:/ Consider using 'objectGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mprObjectGroupName :: Lens.Lens' MetricPolicyRule Lude.Text
mprObjectGroupName = Lens.lens (objectGroupName :: MetricPolicyRule -> Lude.Text) (\s a -> s {objectGroupName = a} :: MetricPolicyRule)
{-# DEPRECATED mprObjectGroupName "Use generic-lens or generic-optics with 'objectGroupName' instead." #-}

instance Lude.FromJSON MetricPolicyRule where
  parseJSON =
    Lude.withObject
      "MetricPolicyRule"
      ( \x ->
          MetricPolicyRule'
            Lude.<$> (x Lude..: "ObjectGroup") Lude.<*> (x Lude..: "ObjectGroupName")
      )

instance Lude.ToJSON MetricPolicyRule where
  toJSON MetricPolicyRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ObjectGroup" Lude..= objectGroup),
            Lude.Just ("ObjectGroupName" Lude..= objectGroupName)
          ]
      )

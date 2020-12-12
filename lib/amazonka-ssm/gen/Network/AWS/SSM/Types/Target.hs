{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.Target
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.Target
  ( Target (..),

    -- * Smart constructor
    mkTarget,

    -- * Lenses
    tValues,
    tKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An array of search criteria that targets instances using a Key,Value combination that you specify.
--
-- Supported formats include the following.
--
--     * @Key=InstanceIds,Values=/instance-id-1/ ,/instance-id-2/ ,/instance-id-3/ @
--
--
--     * @Key=tag:/my-tag-key/ ,Values=/my-tag-value-1/ ,/my-tag-value-2/ @
--
--
--     * @Key=tag-key,Values=/my-tag-key-1/ ,/my-tag-key-2/ @
--
--
--     * __Run Command and Maintenance window targets only__ : @Key=resource-groups:Name,Values=/resource-group-name/ @
--
--
--     * __Maintenance window targets only__ : @Key=resource-groups:ResourceTypeFilters,Values=/resource-type-1/ ,/resource-type-2/ @
--
--
--     * __Automation targets only__ : @Key=ResourceGroup;Values=/resource-group-name/ @
--
--
-- For example:
--
--     * @Key=InstanceIds,Values=i-02573cafcfEXAMPLE,i-0471e04240EXAMPLE,i-07782c72faEXAMPLE@
--
--
--     * @Key=tag:CostCenter,Values=CostCenter1,CostCenter2,CostCenter3@
--
--
--     * @Key=tag-key,Values=Name,Instance-Type,CostCenter@
--
--
--     * __Run Command and Maintenance window targets only__ : @Key=resource-groups:Name,Values=ProductionResourceGroup@
-- This example demonstrates how to target all resources in the resource group __ProductionResourceGroup__ in your maintenance window.
--
--
--     * __Maintenance window targets only__ : @Key=resource-groups:ResourceTypeFilters,Values=/AWS::EC2::INSTANCE/ ,/AWS::EC2::VPC/ @
-- This example demonstrates how to target only EC2 instances and VPCs in your maintenance window.
--
--
--     * __Automation targets only__ : @Key=ResourceGroup,Values=MyResourceGroup@
--
--
--     * __State Manager association targets only__ : @Key=InstanceIds,Values=/*/ @
-- This example demonstrates how to target all managed instances in the AWS Region where the association was created.
--
--
-- For more information about how to send commands that target instances using @Key,Value@ parameters, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html#send-commands-targeting Targeting multiple instances> in the /AWS Systems Manager User Guide/ .
--
-- /See:/ 'mkTarget' smart constructor.
data Target = Target'
  { values :: Lude.Maybe [Lude.Text],
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Target' with the minimum fields required to make a request.
--
-- * 'key' - User-defined criteria for sending commands that target instances that meet the criteria.
-- * 'values' - User-defined criteria that maps to @Key@ . For example, if you specified @tag:ServerRole@ , you could specify @value:WebServer@ to run a command on instances that include EC2 tags of @ServerRole,WebServer@ .
mkTarget ::
  Target
mkTarget = Target' {values = Lude.Nothing, key = Lude.Nothing}

-- | User-defined criteria that maps to @Key@ . For example, if you specified @tag:ServerRole@ , you could specify @value:WebServer@ to run a command on instances that include EC2 tags of @ServerRole,WebServer@ .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tValues :: Lens.Lens' Target (Lude.Maybe [Lude.Text])
tValues = Lens.lens (values :: Target -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: Target)
{-# DEPRECATED tValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | User-defined criteria for sending commands that target instances that meet the criteria.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKey :: Lens.Lens' Target (Lude.Maybe Lude.Text)
tKey = Lens.lens (key :: Target -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: Target)
{-# DEPRECATED tKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON Target where
  parseJSON =
    Lude.withObject
      "Target"
      ( \x ->
          Target'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Key")
      )

instance Lude.ToJSON Target where
  toJSON Target' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Values" Lude..=) Lude.<$> values, ("Key" Lude..=) Lude.<$> key]
      )

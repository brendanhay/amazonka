{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.GatewayGroupSummary
  ( GatewayGroupSummary (..),

    -- * Smart constructor
    mkGatewayGroupSummary,

    -- * Lenses
    ggsArn,
    ggsDescription,
    ggsName,
  )
where

import qualified Network.AWS.AlexaBusiness.Types.Arn as Types
import qualified Network.AWS.AlexaBusiness.Types.Description as Types
import qualified Network.AWS.AlexaBusiness.Types.GatewayGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The summary of a gateway group.
--
-- /See:/ 'mkGatewayGroupSummary' smart constructor.
data GatewayGroupSummary = GatewayGroupSummary'
  { -- | The ARN of the gateway group.
    arn :: Core.Maybe Types.Arn,
    -- | The description of the gateway group.
    description :: Core.Maybe Types.Description,
    -- | The name of the gateway group.
    name :: Core.Maybe Types.GatewayGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GatewayGroupSummary' value with any optional fields omitted.
mkGatewayGroupSummary ::
  GatewayGroupSummary
mkGatewayGroupSummary =
  GatewayGroupSummary'
    { arn = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing
    }

-- | The ARN of the gateway group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggsArn :: Lens.Lens' GatewayGroupSummary (Core.Maybe Types.Arn)
ggsArn = Lens.field @"arn"
{-# DEPRECATED ggsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The description of the gateway group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggsDescription :: Lens.Lens' GatewayGroupSummary (Core.Maybe Types.Description)
ggsDescription = Lens.field @"description"
{-# DEPRECATED ggsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the gateway group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggsName :: Lens.Lens' GatewayGroupSummary (Core.Maybe Types.GatewayGroupName)
ggsName = Lens.field @"name"
{-# DEPRECATED ggsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GatewayGroupSummary where
  parseJSON =
    Core.withObject "GatewayGroupSummary" Core.$
      \x ->
        GatewayGroupSummary'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "Name")

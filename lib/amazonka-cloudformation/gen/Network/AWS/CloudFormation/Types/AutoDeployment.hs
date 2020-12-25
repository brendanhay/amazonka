{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AutoDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.AutoDeployment
  ( AutoDeployment (..),

    -- * Smart constructor
    mkAutoDeployment,

    -- * Lenses
    adEnabled,
    adRetainStacksOnAccountRemoval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- /See:/ 'mkAutoDeployment' smart constructor.
data AutoDeployment = AutoDeployment'
  { -- | If set to @true@ , StackSets automatically deploys additional stack instances to AWS Organizations accounts that are added to a target organization or organizational unit (OU) in the specified Regions. If an account is removed from a target organization or OU, StackSets deletes stack instances from the account in the specified Regions.
    enabled :: Core.Maybe Core.Bool,
    -- | If set to @true@ , stack resources are retained when an account is removed from a target organization or OU. If set to @false@ , stack resources are deleted. Specify only if @Enabled@ is set to @True@ .
    retainStacksOnAccountRemoval :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoDeployment' value with any optional fields omitted.
mkAutoDeployment ::
  AutoDeployment
mkAutoDeployment =
  AutoDeployment'
    { enabled = Core.Nothing,
      retainStacksOnAccountRemoval = Core.Nothing
    }

-- | If set to @true@ , StackSets automatically deploys additional stack instances to AWS Organizations accounts that are added to a target organization or organizational unit (OU) in the specified Regions. If an account is removed from a target organization or OU, StackSets deletes stack instances from the account in the specified Regions.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adEnabled :: Lens.Lens' AutoDeployment (Core.Maybe Core.Bool)
adEnabled = Lens.field @"enabled"
{-# DEPRECATED adEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | If set to @true@ , stack resources are retained when an account is removed from a target organization or OU. If set to @false@ , stack resources are deleted. Specify only if @Enabled@ is set to @True@ .
--
-- /Note:/ Consider using 'retainStacksOnAccountRemoval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRetainStacksOnAccountRemoval :: Lens.Lens' AutoDeployment (Core.Maybe Core.Bool)
adRetainStacksOnAccountRemoval = Lens.field @"retainStacksOnAccountRemoval"
{-# DEPRECATED adRetainStacksOnAccountRemoval "Use generic-lens or generic-optics with 'retainStacksOnAccountRemoval' instead." #-}

instance Core.FromXML AutoDeployment where
  parseXML x =
    AutoDeployment'
      Core.<$> (x Core..@? "Enabled")
      Core.<*> (x Core..@? "RetainStacksOnAccountRemoval")

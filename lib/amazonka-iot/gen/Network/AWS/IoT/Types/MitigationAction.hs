{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.MitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.MitigationAction
  ( MitigationAction (..),

    -- * Smart constructor
    mkMitigationAction,

    -- * Lenses
    maActionParams,
    maId,
    maName,
    maRoleArn,
  )
where

import qualified Network.AWS.IoT.Types.MitigationActionId as Types
import qualified Network.AWS.IoT.Types.MitigationActionName as Types
import qualified Network.AWS.IoT.Types.MitigationActionParams as Types
import qualified Network.AWS.IoT.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes which changes should be applied as part of a mitigation action.
--
-- /See:/ 'mkMitigationAction' smart constructor.
data MitigationAction = MitigationAction'
  { -- | The set of parameters for this mitigation action. The parameters vary, depending on the kind of action you apply.
    actionParams :: Core.Maybe Types.MitigationActionParams,
    -- | A unique identifier for the mitigation action.
    id :: Core.Maybe Types.MitigationActionId,
    -- | A user-friendly name for the mitigation action.
    name :: Core.Maybe Types.MitigationActionName,
    -- | The IAM role ARN used to apply this mitigation action.
    roleArn :: Core.Maybe Types.RoleArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MitigationAction' value with any optional fields omitted.
mkMitigationAction ::
  MitigationAction
mkMitigationAction =
  MitigationAction'
    { actionParams = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      roleArn = Core.Nothing
    }

-- | The set of parameters for this mitigation action. The parameters vary, depending on the kind of action you apply.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maActionParams :: Lens.Lens' MitigationAction (Core.Maybe Types.MitigationActionParams)
maActionParams = Lens.field @"actionParams"
{-# DEPRECATED maActionParams "Use generic-lens or generic-optics with 'actionParams' instead." #-}

-- | A unique identifier for the mitigation action.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maId :: Lens.Lens' MitigationAction (Core.Maybe Types.MitigationActionId)
maId = Lens.field @"id"
{-# DEPRECATED maId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A user-friendly name for the mitigation action.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maName :: Lens.Lens' MitigationAction (Core.Maybe Types.MitigationActionName)
maName = Lens.field @"name"
{-# DEPRECATED maName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The IAM role ARN used to apply this mitigation action.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
maRoleArn :: Lens.Lens' MitigationAction (Core.Maybe Types.RoleArn)
maRoleArn = Lens.field @"roleArn"
{-# DEPRECATED maRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

instance Core.FromJSON MitigationAction where
  parseJSON =
    Core.withObject "MitigationAction" Core.$
      \x ->
        MitigationAction'
          Core.<$> (x Core..:? "actionParams")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "roleArn")

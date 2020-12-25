{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.DeploymentCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.DeploymentCommand
  ( DeploymentCommand (..),

    -- * Smart constructor
    mkDeploymentCommand,

    -- * Lenses
    dcName,
    dcArgs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.DeploymentCommandName as Types
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Used to specify a stack or deployment command.
--
-- /See:/ 'mkDeploymentCommand' smart constructor.
data DeploymentCommand = DeploymentCommand'
  { -- | Specifies the operation. You can specify only one command.
    --
    -- For stacks, the following commands are available:
    --
    --     * @execute_recipes@ : Execute one or more recipes. To specify the recipes, set an @Args@ parameter named @recipes@ to the list of recipes to be executed. For example, to execute @phpapp::appsetup@ , set @Args@ to @{"recipes":["phpapp::appsetup"]}@ .
    --
    --
    --     * @install_dependencies@ : Install the stack's dependencies.
    --
    --
    --     * @update_custom_cookbooks@ : Update the stack's custom cookbooks.
    --
    --
    --     * @update_dependencies@ : Update the stack's dependencies.
    --
    --
    -- For apps, the following commands are available:
    --
    --     * @deploy@ : Deploy an app. Ruby on Rails apps have an optional @Args@ parameter named @migrate@ . Set @Args@ to {"migrate":["true"]} to migrate the database. The default setting is {"migrate":["false"]}.
    --
    --
    --     * @rollback@ Roll the app back to the previous version. When you update an app, AWS OpsWorks Stacks stores the previous version, up to a maximum of five versions. You can use this command to roll an app back as many as four versions.
    --
    --
    --     * @start@ : Start the app's web or application server.
    --
    --
    --     * @stop@ : Stop the app's web or application server.
    --
    --
    --     * @restart@ : Restart the app's web or application server.
    --
    --
    --     * @undeploy@ : Undeploy the app.
    name :: Types.DeploymentCommandName,
    -- | The arguments of those commands that take arguments. It should be set to a JSON object with the following format:
    --
    -- @{"arg_name1" : ["value1", "value2", ...], "arg_name2" : ["value1", "value2", ...], ...}@
    -- The @update_dependencies@ command takes two arguments:
    --
    --     * @upgrade_os_to@ - Specifies the desired Amazon Linux version for instances whose OS you want to upgrade, such as @Amazon Linux 2016.09@ . You must also set the @allow_reboot@ argument to true.
    --
    --
    --     * @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to reboot the instances if necessary, after installing the updates. This argument can be set to either @true@ or @false@ . The default value is @false@ .
    --
    --
    -- For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@ to the following.
    -- @{ "upgrade_os_to":["Amazon Linux 2016.09"], "allow_reboot":["true"] } @
    args :: Core.Maybe (Core.HashMap Types.String [Types.String])
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeploymentCommand' value with any optional fields omitted.
mkDeploymentCommand ::
  -- | 'name'
  Types.DeploymentCommandName ->
  DeploymentCommand
mkDeploymentCommand name =
  DeploymentCommand' {name, args = Core.Nothing}

-- | Specifies the operation. You can specify only one command.
--
-- For stacks, the following commands are available:
--
--     * @execute_recipes@ : Execute one or more recipes. To specify the recipes, set an @Args@ parameter named @recipes@ to the list of recipes to be executed. For example, to execute @phpapp::appsetup@ , set @Args@ to @{"recipes":["phpapp::appsetup"]}@ .
--
--
--     * @install_dependencies@ : Install the stack's dependencies.
--
--
--     * @update_custom_cookbooks@ : Update the stack's custom cookbooks.
--
--
--     * @update_dependencies@ : Update the stack's dependencies.
--
--
-- For apps, the following commands are available:
--
--     * @deploy@ : Deploy an app. Ruby on Rails apps have an optional @Args@ parameter named @migrate@ . Set @Args@ to {"migrate":["true"]} to migrate the database. The default setting is {"migrate":["false"]}.
--
--
--     * @rollback@ Roll the app back to the previous version. When you update an app, AWS OpsWorks Stacks stores the previous version, up to a maximum of five versions. You can use this command to roll an app back as many as four versions.
--
--
--     * @start@ : Start the app's web or application server.
--
--
--     * @stop@ : Stop the app's web or application server.
--
--
--     * @restart@ : Restart the app's web or application server.
--
--
--     * @undeploy@ : Undeploy the app.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DeploymentCommand Types.DeploymentCommandName
dcName = Lens.field @"name"
{-# DEPRECATED dcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The arguments of those commands that take arguments. It should be set to a JSON object with the following format:
--
-- @{"arg_name1" : ["value1", "value2", ...], "arg_name2" : ["value1", "value2", ...], ...}@
-- The @update_dependencies@ command takes two arguments:
--
--     * @upgrade_os_to@ - Specifies the desired Amazon Linux version for instances whose OS you want to upgrade, such as @Amazon Linux 2016.09@ . You must also set the @allow_reboot@ argument to true.
--
--
--     * @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to reboot the instances if necessary, after installing the updates. This argument can be set to either @true@ or @false@ . The default value is @false@ .
--
--
-- For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@ to the following.
-- @{ "upgrade_os_to":["Amazon Linux 2016.09"], "allow_reboot":["true"] } @
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcArgs :: Lens.Lens' DeploymentCommand (Core.Maybe (Core.HashMap Types.String [Types.String]))
dcArgs = Lens.field @"args"
{-# DEPRECATED dcArgs "Use generic-lens or generic-optics with 'args' instead." #-}

instance Core.FromJSON DeploymentCommand where
  toJSON DeploymentCommand {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Name" Core..= name), ("Args" Core..=) Core.<$> args]
      )

instance Core.FromJSON DeploymentCommand where
  parseJSON =
    Core.withObject "DeploymentCommand" Core.$
      \x ->
        DeploymentCommand'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..:? "Args")

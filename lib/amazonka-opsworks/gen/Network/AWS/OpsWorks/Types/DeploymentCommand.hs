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
    dcArgs,
    dcName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.DeploymentCommandName
import qualified Network.AWS.Prelude as Lude

-- | Used to specify a stack or deployment command.
--
-- /See:/ 'mkDeploymentCommand' smart constructor.
data DeploymentCommand = DeploymentCommand'
  { args ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    name :: DeploymentCommandName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeploymentCommand' with the minimum fields required to make a request.
--
-- * 'args' - The arguments of those commands that take arguments. It should be set to a JSON object with the following format:
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
-- * 'name' - Specifies the operation. You can specify only one command.
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
mkDeploymentCommand ::
  -- | 'name'
  DeploymentCommandName ->
  DeploymentCommand
mkDeploymentCommand pName_ =
  DeploymentCommand' {args = Lude.Nothing, name = pName_}

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
dcArgs :: Lens.Lens' DeploymentCommand (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
dcArgs = Lens.lens (args :: DeploymentCommand -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {args = a} :: DeploymentCommand)
{-# DEPRECATED dcArgs "Use generic-lens or generic-optics with 'args' instead." #-}

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
dcName :: Lens.Lens' DeploymentCommand DeploymentCommandName
dcName = Lens.lens (name :: DeploymentCommand -> DeploymentCommandName) (\s a -> s {name = a} :: DeploymentCommand)
{-# DEPRECATED dcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON DeploymentCommand where
  parseJSON =
    Lude.withObject
      "DeploymentCommand"
      ( \x ->
          DeploymentCommand'
            Lude.<$> (x Lude..:? "Args" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON DeploymentCommand where
  toJSON DeploymentCommand' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Args" Lude..=) Lude.<$> args, Lude.Just ("Name" Lude..= name)]
      )

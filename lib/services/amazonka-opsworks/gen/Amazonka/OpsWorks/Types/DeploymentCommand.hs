{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.Types.DeploymentCommand
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.DeploymentCommand where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.DeploymentCommandName
import qualified Amazonka.Prelude as Prelude

-- | Used to specify a stack or deployment command.
--
-- /See:/ 'newDeploymentCommand' smart constructor.
data DeploymentCommand = DeploymentCommand'
  { -- | The arguments of those commands that take arguments. It should be set to
    -- a JSON object with the following format:
    --
    -- @{\"arg_name1\" : [\"value1\", \"value2\", ...], \"arg_name2\" : [\"value1\", \"value2\", ...], ...}@
    --
    -- The @update_dependencies@ command takes two arguments:
    --
    -- -   @upgrade_os_to@ - Specifies the desired Amazon Linux version for
    --     instances whose OS you want to upgrade, such as
    --     @Amazon Linux 2016.09@. You must also set the @allow_reboot@
    --     argument to true.
    --
    -- -   @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to
    --     reboot the instances if necessary, after installing the updates.
    --     This argument can be set to either @true@ or @false@. The default
    --     value is @false@.
    --
    -- For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@
    -- to the following.
    --
    -- @ { \"upgrade_os_to\":[\"Amazon Linux 2016.09\"], \"allow_reboot\":[\"true\"] } @
    args :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | Specifies the operation. You can specify only one command.
    --
    -- For stacks, the following commands are available:
    --
    -- -   @execute_recipes@: Execute one or more recipes. To specify the
    --     recipes, set an @Args@ parameter named @recipes@ to the list of
    --     recipes to be executed. For example, to execute @phpapp::appsetup@,
    --     set @Args@ to @{\"recipes\":[\"phpapp::appsetup\"]}@.
    --
    -- -   @install_dependencies@: Install the stack\'s dependencies.
    --
    -- -   @update_custom_cookbooks@: Update the stack\'s custom cookbooks.
    --
    -- -   @update_dependencies@: Update the stack\'s dependencies.
    --
    -- The update_dependencies and install_dependencies commands are supported
    -- only for Linux instances. You can run the commands successfully on
    -- Windows instances, but they do nothing.
    --
    -- For apps, the following commands are available:
    --
    -- -   @deploy@: Deploy an app. Ruby on Rails apps have an optional @Args@
    --     parameter named @migrate@. Set @Args@ to {\"migrate\":[\"true\"]} to
    --     migrate the database. The default setting is
    --     {\"migrate\":[\"false\"]}.
    --
    -- -   @rollback@ Roll the app back to the previous version. When you
    --     update an app, AWS OpsWorks Stacks stores the previous version, up
    --     to a maximum of five versions. You can use this command to roll an
    --     app back as many as four versions.
    --
    -- -   @start@: Start the app\'s web or application server.
    --
    -- -   @stop@: Stop the app\'s web or application server.
    --
    -- -   @restart@: Restart the app\'s web or application server.
    --
    -- -   @undeploy@: Undeploy the app.
    name :: DeploymentCommandName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'deploymentCommand_args' - The arguments of those commands that take arguments. It should be set to
-- a JSON object with the following format:
--
-- @{\"arg_name1\" : [\"value1\", \"value2\", ...], \"arg_name2\" : [\"value1\", \"value2\", ...], ...}@
--
-- The @update_dependencies@ command takes two arguments:
--
-- -   @upgrade_os_to@ - Specifies the desired Amazon Linux version for
--     instances whose OS you want to upgrade, such as
--     @Amazon Linux 2016.09@. You must also set the @allow_reboot@
--     argument to true.
--
-- -   @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to
--     reboot the instances if necessary, after installing the updates.
--     This argument can be set to either @true@ or @false@. The default
--     value is @false@.
--
-- For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@
-- to the following.
--
-- @ { \"upgrade_os_to\":[\"Amazon Linux 2016.09\"], \"allow_reboot\":[\"true\"] } @
--
-- 'name', 'deploymentCommand_name' - Specifies the operation. You can specify only one command.
--
-- For stacks, the following commands are available:
--
-- -   @execute_recipes@: Execute one or more recipes. To specify the
--     recipes, set an @Args@ parameter named @recipes@ to the list of
--     recipes to be executed. For example, to execute @phpapp::appsetup@,
--     set @Args@ to @{\"recipes\":[\"phpapp::appsetup\"]}@.
--
-- -   @install_dependencies@: Install the stack\'s dependencies.
--
-- -   @update_custom_cookbooks@: Update the stack\'s custom cookbooks.
--
-- -   @update_dependencies@: Update the stack\'s dependencies.
--
-- The update_dependencies and install_dependencies commands are supported
-- only for Linux instances. You can run the commands successfully on
-- Windows instances, but they do nothing.
--
-- For apps, the following commands are available:
--
-- -   @deploy@: Deploy an app. Ruby on Rails apps have an optional @Args@
--     parameter named @migrate@. Set @Args@ to {\"migrate\":[\"true\"]} to
--     migrate the database. The default setting is
--     {\"migrate\":[\"false\"]}.
--
-- -   @rollback@ Roll the app back to the previous version. When you
--     update an app, AWS OpsWorks Stacks stores the previous version, up
--     to a maximum of five versions. You can use this command to roll an
--     app back as many as four versions.
--
-- -   @start@: Start the app\'s web or application server.
--
-- -   @stop@: Stop the app\'s web or application server.
--
-- -   @restart@: Restart the app\'s web or application server.
--
-- -   @undeploy@: Undeploy the app.
newDeploymentCommand ::
  -- | 'name'
  DeploymentCommandName ->
  DeploymentCommand
newDeploymentCommand pName_ =
  DeploymentCommand'
    { args = Prelude.Nothing,
      name = pName_
    }

-- | The arguments of those commands that take arguments. It should be set to
-- a JSON object with the following format:
--
-- @{\"arg_name1\" : [\"value1\", \"value2\", ...], \"arg_name2\" : [\"value1\", \"value2\", ...], ...}@
--
-- The @update_dependencies@ command takes two arguments:
--
-- -   @upgrade_os_to@ - Specifies the desired Amazon Linux version for
--     instances whose OS you want to upgrade, such as
--     @Amazon Linux 2016.09@. You must also set the @allow_reboot@
--     argument to true.
--
-- -   @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to
--     reboot the instances if necessary, after installing the updates.
--     This argument can be set to either @true@ or @false@. The default
--     value is @false@.
--
-- For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@
-- to the following.
--
-- @ { \"upgrade_os_to\":[\"Amazon Linux 2016.09\"], \"allow_reboot\":[\"true\"] } @
deploymentCommand_args :: Lens.Lens' DeploymentCommand (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
deploymentCommand_args = Lens.lens (\DeploymentCommand' {args} -> args) (\s@DeploymentCommand' {} a -> s {args = a} :: DeploymentCommand) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the operation. You can specify only one command.
--
-- For stacks, the following commands are available:
--
-- -   @execute_recipes@: Execute one or more recipes. To specify the
--     recipes, set an @Args@ parameter named @recipes@ to the list of
--     recipes to be executed. For example, to execute @phpapp::appsetup@,
--     set @Args@ to @{\"recipes\":[\"phpapp::appsetup\"]}@.
--
-- -   @install_dependencies@: Install the stack\'s dependencies.
--
-- -   @update_custom_cookbooks@: Update the stack\'s custom cookbooks.
--
-- -   @update_dependencies@: Update the stack\'s dependencies.
--
-- The update_dependencies and install_dependencies commands are supported
-- only for Linux instances. You can run the commands successfully on
-- Windows instances, but they do nothing.
--
-- For apps, the following commands are available:
--
-- -   @deploy@: Deploy an app. Ruby on Rails apps have an optional @Args@
--     parameter named @migrate@. Set @Args@ to {\"migrate\":[\"true\"]} to
--     migrate the database. The default setting is
--     {\"migrate\":[\"false\"]}.
--
-- -   @rollback@ Roll the app back to the previous version. When you
--     update an app, AWS OpsWorks Stacks stores the previous version, up
--     to a maximum of five versions. You can use this command to roll an
--     app back as many as four versions.
--
-- -   @start@: Start the app\'s web or application server.
--
-- -   @stop@: Stop the app\'s web or application server.
--
-- -   @restart@: Restart the app\'s web or application server.
--
-- -   @undeploy@: Undeploy the app.
deploymentCommand_name :: Lens.Lens' DeploymentCommand DeploymentCommandName
deploymentCommand_name = Lens.lens (\DeploymentCommand' {name} -> name) (\s@DeploymentCommand' {} a -> s {name = a} :: DeploymentCommand)

instance Data.FromJSON DeploymentCommand where
  parseJSON =
    Data.withObject
      "DeploymentCommand"
      ( \x ->
          DeploymentCommand'
            Prelude.<$> (x Data..:? "Args" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable DeploymentCommand where
  hashWithSalt _salt DeploymentCommand' {..} =
    _salt `Prelude.hashWithSalt` args
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeploymentCommand where
  rnf DeploymentCommand' {..} =
    Prelude.rnf args `Prelude.seq` Prelude.rnf name

instance Data.ToJSON DeploymentCommand where
  toJSON DeploymentCommand' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Args" Data..=) Prelude.<$> args,
            Prelude.Just ("Name" Data..= name)
          ]
      )
